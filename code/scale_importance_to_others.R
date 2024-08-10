# ---------- IMPORTANCE TO OTHERS ---------- #

# LOAD DATA
df <- read.csv("https://raw.githubusercontent.com/jackbhuber/datasets/main/Baylor_Religion_Survey_Wave_VI_2021.csv", header = TRUE)

# Original coding
# 1) Not at all	37	3.0
# 2) A little	218	17.6
# 3) Somewhat	626	50.6
# 4) A lot	355	28.7
# Missing	12

# Impute mean to missing  values
df$Q9A[is.na(df$Q9A)] <- round(mean(df$Q9A, na.rm=TRUE),0) # How much do you feel other people pay attention to you?
df$Q9B[is.na(df$Q9B)] <- round(mean(df$Q9B, na.rm=TRUE),0) # How much do you feel others would miss you if you went away?
df$Q9C[is.na(df$Q9C)] <- round(mean(df$Q9C, na.rm=TRUE),0) # How interested are people generally in what you say?
df$Q9D[is.na(df$Q9D)] <- round(mean(df$Q9D, na.rm=TRUE),0) # How much do other people depend on you?
df$Q9E[is.na(df$Q9E)] <- round(mean(df$Q9E, na.rm=TRUE),0) # How important do you feel you are to other people?

# Df of selected columns
# importance = c('Q9A', 'Q9B', 'Q9C', 'Q9D', 'Q9E')
importance = c('Q9A', 'Q9B', 'Q9C', 'Q9D', 'Q9E')
df_importance = df[ , importance] 

# Collapsing categories as necessary
df_importance[df_importance == 1] <- 1
df_importance[df_importance == 2] <- 1
df_importance[df_importance == 3] <- 2
df_importance[df_importance == 4] <- 3

# Frequencies of response categories
alpha(df_importance)$response.freq

# Analysis of df_paffect df
head(df_importance)
summary(df_importance)

# Correlation matrix
df.cor = cor(df_importance)
df.cor # The lowest correlation is 0.23
corrplot(df.cor, type="lower", method="color", tl.pos = 'd')
cronbach.alpha(df_importance) # Cronbach's alpha is 0.712

# -----------------------------------------------------------

# RATING SCALE ANALYSIS

# 1. Fit the model
mod1 <- tam.mml(df_importance, irtmodel = "RSM") 
# EAP reliability = 0.782

nitems <- ncol(df_importance)
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
     deltas$xsi[1:ncol(df_importance)], 
     xlim = c(0.5, 1.5), type = "n",
     xlab = "Outfit mean square", 
     ylab = "Item location in logits")

text(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:nitems],
     labels = colnames(df_importance), 
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
     labels = colnames(df_importance), 
     cex = 0.9, font = 2)

abline(v = 1)
abline(v = 1.30, col = "blue")
abline(v = 0.70, col = "blue")

# 10. Get the abilities
abil <- tam.wle(mod1) # get abilities
df$importance <- abil$theta # attach abilities to df
# df$agency <- df$agency*10 + 100
hist(df$importance) # histogram of abilities
jarque.test(df$importance) # normality test of abilities

# 11. WrightMap
IRT.WrightMap(mod1)
