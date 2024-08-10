# ---------- FREQUENCY OF PRAYER ---------- #

# LOAD DATA
df <- read.csv("https://raw.githubusercontent.com/jackbhuber/datasets/main/Baylor_Religion_Survey_Wave_VI_2021.csv", header = TRUE)

# Original coding
# Q54A - How often outside of religious services do you... a. Pray alone for less than five minutes
# 1) Never	287	24.2
# 2) Seldom	189	16.0
# 3) Monthly	56	4.7
# 4) Weekly	106	9.0
# 5) Daily	364	30.7
# 6) Several times a day	182	15.4
# Missing	64

df$Q54A[is.na(df$Q54A)] <- round(mean(df$Q54A, na.rm=TRUE),0) # Pray alone < 5 minutes
df$Q54B[is.na(df$Q54B)] <- round(mean(df$Q54B, na.rm=TRUE),0) # Pray alone >= 5 minutes
df$Q54C[is.na(df$Q54C)] <- round(mean(df$Q54C, na.rm=TRUE),0) # Pray with others
df$Q54D[is.na(df$Q54D)] <- round(mean(df$Q54D, na.rm=TRUE),0) # Meditate
df$Q54E[is.na(df$Q54E)] <- round(mean(df$Q54E, na.rm=TRUE),0) # Say grace (pray before meals)

# Df of selected columns
prayfreq = c('Q54A', 'Q54B', 'Q54C', 'Q54D', 'Q54E')
df_prayfreq <- df[ , prayfreq]

# Collapsing categories as necessary
df_prayfreq[df_prayfreq == 1] <- 1
df_prayfreq[df_prayfreq == 2] <- 2
df_prayfreq[df_prayfreq == 3] <- 3
df_prayfreq[df_prayfreq == 4] <- 4
df_prayfreq[df_prayfreq == 5] <- 4
df_prayfreq[df_prayfreq == 6] <- 6

# Frequencies of response categories
alpha(df_prayfreq)$response.freq

# Analysis of df_impacts df
head(df_prayfreq)
summary(df_prayfreq)

# Correlation matrix
df.cor = cor(df_prayfreq)
df.cor # The lowest correlation is 0.23
corrplot(df.cor, type="lower", method="color", tl.pos = 'd')
cronbach.alpha(df_prayfreq) # Cronbach's alpha is 0.851

# -----------------------------------------------------------

# RATING SCALE ANALYSIS

# 1. Fit the model
mod1 <- tam.mml(df_prayfreq, irtmodel = "RSM")
# EAP reliability = 0.833

nitems <- ncol(df_prayfreq)
deltas <- mod1$xsi[1:nitems, ]

# 2. Summary statistics
summary(mod1) # model summary

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

# 6. Item difficulties
# hist(diffic$xsi, breaks=10) # histogram of item difficulties

# 7. Plot the test information curve
info.myTAM <- IRT.informationCurves(mod1)
plot(info.myTAM, xlab = "Theta", 
     ylab = "Test information", 
     main = "Test Information Curve")

# 8.  Plot a construct alley using outfit
plot(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:ncol(df_prayfreq)], 
     xlim = c(0.5, 1.5), type = "n",
     xlab = "Outfit mean square", 
     ylab = "Item location in logits")

text(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:nitems],
     labels = colnames(df_prayfreq), 
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
     labels = colnames(df_prayfreq), 
     cex = 0.9, font = 2)

abline(v = 1)
abline(v = 1.30, col = "blue")
abline(v = 0.70, col = "blue")

# 10. WrightMap
IRT.WrightMap(mod1)

# 11. Get the abilities
abil <- tam.wle(mod1) # get abilities
df$prayer_frequency <- abil$theta # attach abilities to df
# df$agency <- df$agency*10 + 100
hist(df$prayer_frequency) # histogram of abilities
jarque.test(df$prayer_frequency) # normality test of abilities
