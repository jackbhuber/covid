# ---------- CLOSENESS TO OTHERS ---------- #

# LOAD DATA
df <- read.csv("https://raw.githubusercontent.com/jackbhuber/datasets/main/Baylor_Religion_Survey_Wave_VI_2021.csv", header = TRUE)

# Original coding
# 1) Not close at all	36	2.9
# 2) Not too close	75	6.1
# 3) Somewhat close	344	27.8
# 4) Very close	783	63.2
# Missing	10

df$Q25A[is.na(df$Q25A)] <- round(mean(df$Q25A, na.rm=TRUE),0) # family
df$Q25B[is.na(df$Q25B)] <- round(mean(df$Q25B, na.rm=TRUE),0) # friends
df$Q25C[is.na(df$Q25C)] <- round(mean(df$Q25C, na.rm=TRUE),0) # co-workers
df$Q25D[is.na(df$Q25D)] <- round(mean(df$Q25D, na.rm=TRUE),0) # neighbors
df$Q25E[is.na(df$Q25E)] <- round(mean(df$Q25E, na.rm=TRUE),0) # online community

# Df of selected columns
close = c('Q25A', 'Q25B', 'Q25C', 'Q25D', 'Q25E')
df_close <- df[ , close]

# Collapsing categories as necessary
df_close[df_close == 1] <- 1
df_close[df_close == 2] <- 1
df_close[df_close == 3] <- 2
df_close[df_close == 4] <- 3

# Frequencies of response categories
alpha(df_close)$response.freq

# Analysis of df_impacts df
head(df_close)
summary(df_close)

# Correlation matrix
df.cor = cor(df_close)
df.cor # The lowest correlation is 0.23
corrplot(df.cor, type="lower", method="color", tl.pos = 'd')
cronbach.alpha(df_close) # Cronbach's alpha is 0.624

# -----------------------------------------------------------

# RATING SCALE ANALYSIS

# 1. Fit the model
mod1 <- tam.mml(df_close, irtmodel = "RSM") 
# EAP reliability = 0.641

nitems <- ncol(df_close)
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
     deltas$xsi[1:ncol(df_close)], 
     xlim = c(0.5, 1.5), type = "n",
     xlab = "Outfit mean square", 
     ylab = "Item location in logits")

text(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:nitems],
     labels = colnames(df_close), 
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
     labels = colnames(df_close), 
     cex = 0.9, font = 2)

abline(v = 1)
abline(v = 1.30, col = "blue")
abline(v = 0.70, col = "blue")

# 10. WrightMap
IRT.WrightMap(mod1)

# 11. Get the abilities
abil <- tam.wle(mod1) # get abilities
df$closeness <- abil$theta # attach abilities to df
# df$agency <- df$agency*10 + 100
hist(df$closeness) # histogram of abilities
jarque.test(df$closeness) # normality test of abilities
