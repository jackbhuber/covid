# ---------- ALTRUISM ---------- #

# LOAD DATA
df <- read.csv("https://raw.githubusercontent.com/jackbhuber/datasets/main/Baylor_Religion_Survey_Wave_VI_2021.csv", header = TRUE)

df$Q19A[is.na(df$Q19A)] <- 0 # attended event
df$Q19B[is.na(df$Q19B)] <- 0 # donated money
df$Q19C[is.na(df$Q19C)] <- 0 # worked with neighbors for justice

df$Q19A <- ifelse(df$Q19A == 1, 1, 0) # attended event
df$Q19B <- ifelse(df$Q19B == 1, 1, 0) # donated money
df$Q19C <- ifelse(df$Q19C == 1, 1, 0) # worked with neighbors for justice

# Df of selected columns
altruism = c('Q19A','Q19B','Q19C')
df_altruism = df[ , altruism] 

# Frequencies of response categories
alpha(df_altruism)$response.freq

# Analysis of df_impacts df
head(df_altruism)
summary(df_altruism)

# Correlation matrix
df.cor = cor(df_altruism)
df.cor # The lowest correlation is 0.23
corrplot(df.cor, type="lower", method="color", tl.pos = 'd')
cronbach.alpha(df_altruism) # Cronbach's alpha is 0.

# -----------------------------------------------------------

# RATING SCALE ANALYSIS

# 1. Fit the model
mod1 <- tam(df_altruism) 
# EAP reliability = 0.782

nitems <- ncol(df_altruism)
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
     deltas$xsi[1:ncol(df_altruism)], 
     xlim = c(0.5, 1.5), type = "n",
     xlab = "Outfit mean square", 
     ylab = "Item location in logits")

text(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:nitems],
     labels = colnames(df_altruism), 
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
     labels = colnames(df_altruism), 
     cex = 0.9, font = 2)

abline(v = 1)
abline(v = 1.30, col = "blue")
abline(v = 0.70, col = "blue")

# 10. Get the abilities
abil <- tam.wle(mod1) # get abilities
df$altruism <- abil$theta # attach abilities to df
# df$agency <- df$agency*10 + 100
hist(df$altruism) # histogram of abilities
jarque.test(df$altruism) # normality test of abilities

# 11. WrightMap
IRT.WrightMap(mod1)
