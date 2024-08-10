# ---------- EMOTIONAL DISTRESS ---------- #

# LOAD DATA
df <- read.csv("https://raw.githubusercontent.com/jackbhuber/datasets/main/Baylor_Religion_Survey_Wave_VI_2021.csv", header = TRUE)

# Deal with missing data
df$Q4A[is.na(df$Q4A)] <- round(mean(df$Q4A, na.rm=TRUE),0) # sad
df$Q4B[is.na(df$Q4B)] <- round(mean(df$Q4B, na.rm=TRUE),0) # worried
df$Q4C[is.na(df$Q4C)] <- round(mean(df$Q4C, na.rm=TRUE),0) # tense
df$Q4D[is.na(df$Q4D)] <- round(mean(df$Q4D, na.rm=TRUE),0) # anxious
df$Q4E[is.na(df$Q4E)] <- round(mean(df$Q4E, na.rm=TRUE),0) # restless
df$Q4F[is.na(df$Q4F)] <- round(mean(df$Q4F, na.rm=TRUE),0) # bored
df$Q4G[is.na(df$Q4G)] <- round(mean(df$Q4G, na.rm=TRUE),0) # angry

# Df of emotional distress columns
# distress = c('Q4A', 'Q4B', 'Q4C', 'Q4D', 'Q4E', 'Q4F', 'Q4G')
distress = c('Q4A', 'Q4C', 'Q4E', 'Q4G')

df_distress <- df[ , distress]

# Collapsing categories as necessary
# df_distress[df_distress == 1] <- 1
# df_distress[df_distress == 2] <- 1
# df_distress[df_distress == 3] <- 2
# df_distress[df_distress == 4] <- 3
# df_distress[df_distress == 5] <- 4

# Frequencies of response categories
alpha(df_distress)$response.freq

# Analysis of df_paffect df
head(df_distress)
summary(df_distress)

# Correlation matrix
df.cor = cor(df_distress)
df.cor # The lowest correlation is 0.58
corrplot(df.cor, type="lower", method="color", tl.pos = 'd')
cronbach.alpha(df_distress) # Cronbach's alpha is 0.798

# -----------------------------------------------------------

# RATING SCALE ANALYSIS

nitems <- ncol(df_distress)
deltas <- mod1$xsi[1:nitems, ]

# 1. Fit the model
mod1 <- tam.mml(df_distress, irtmodel = "RSM") 
# EAP reliability = 0.793

# 2. Summary statistics
summary(mod1) # model summary
# Category thresholds increase in difficulty as expected
# All items are close in difficulty

# 3. Inspect the item parameter and centralized category thresholds
mod1$item_irt

# 4. Plots of item curves
plot(mod1)
plot(mod1, 
     type = "items", 
     export = FALSE, 
     observed = TRUE, 
     low = -6, 
     high = 6)
# On all items, option "2" appears not to measure anything distinct from 1 or 3

plot(mod1, 
     type = "expected", 
     ngroups = 6, 
     low  = -6, 
     high = 6,
     package = "lattice", 
     overlay = TRUE,
     observed = FALSE)

# 5. Item fit statistics
fit <- msq.itemfit(mod1)$itemfit
(fit <- data.frame(fit[1], round(fit[3:8], 2)))
# 

# 6. Item difficulties
# hist(diffic$xsi, breaks=10) # histogram of item difficulties

# 7. Plot the test information curve
info.myTAM <- IRT.informationCurves(mod1)

plot(info.myTAM, xlab = "Theta", 
     ylab = "Test information", 
     main = "Test Information Curve")

# 8.  Plot a construct alley using outfit
plot(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:ncol(df_distress)], 
     xlim = c(0.5, 1.5), type = "n",
     xlab = "Outfit mean square", 
     ylab = "Item location in logits")

text(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:nitems],
     labels = colnames(df_distress), 
     cex = 0.9, 
     font = 2)

abline(v = 1)
abline(v = 1.30, col = "blue")
abline(v = 0.70, col = "blue")


# 9. Plot a construct alley using infit
plot(msq.itemfit(mod1)$itemfit$Infit, 
     deltas$xsi[1:nitems], 
     xlim = c(0.5, 1.5), 
     type = "n",
     xlab = "Infit mean square", 
     ylab = "Item location in logits")

text(msq.itemfit(mod1)$itemfit$Infit, 
     deltas$xsi[1:nitems],
     labels = colnames(df_distress), 
     cex = 0.9, font = 2)

abline(v = 1)
abline(v = 1.30, col = "blue")
abline(v = 0.70, col = "blue")

# 10. Get the abilities
abil <- tam.wle(mod1) # get abilities
df$distress <- abil$theta # attach abilities to df
hist(df$distress) # histogram of abilities
jarque.test(df$distress) # normality test of abilities

# 11. WrightMap
IRT.WrightMap(mod1)


