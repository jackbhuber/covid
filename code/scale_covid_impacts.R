# ---------- PERSONAL IMPACTS OF COVID ---------- #

# LOAD DATA
df <- read.csv("https://raw.githubusercontent.com/jackbhuber/datasets/main/Baylor_Religion_Survey_Wave_VI_2021.csv", header = TRUE)

# Original coding
# 1) Yes	210	17.1
# 2) No	1017	82.9
# Missing	21

# Code all as dummies
df$Q13A[is.na(df$Q13A)] <- 0
df$Q13B[is.na(df$Q13B)] <- 0
df$Q13C[is.na(df$Q13C)] <- 0
df$Q13D[is.na(df$Q13D)] <- 0
df$Q13E[is.na(df$Q13E)] <- 0
df$Q13F[is.na(df$Q13F)] <- 0
df$Q13G[is.na(df$Q13G)] <- 0
df$Q13H[is.na(df$Q13H)] <- 0
df$Q13I[is.na(df$Q13I)] <- 0
df$Q13J[is.na(df$Q13J)] <- 0
df$Q13K[is.na(df$Q13K)] <- 0
df$Q13L[is.na(df$Q13L)] <- 0
df$Q13M[is.na(df$Q13M)] <- 0
df$Q13N[is.na(df$Q13N)] <- 0
df$Q13O[is.na(df$Q13O)] <- 0

df$Q13A <- ifelse(df$Q13A == 1, 1, 0)
df$Q13B <- ifelse(df$Q13B == 1, 1, 0)
df$Q13C <- ifelse(df$Q13C == 1, 1, 0)
df$Q13D <- ifelse(df$Q13D == 1, 1, 0)
df$Q13E <- ifelse(df$Q13E == 1, 1, 0)
df$Q13F <- ifelse(df$Q13F == 1, 1, 0)
df$Q13G <- ifelse(df$Q13G == 1, 1, 0)
df$Q13H <- ifelse(df$Q13H == 1, 1, 0)
df$Q13I <- ifelse(df$Q13I == 1, 1, 0)
df$Q13J <- ifelse(df$Q13J == 1, 1, 0)
df$Q13K <- ifelse(df$Q13K == 1, 1, 0)
df$Q13L <- ifelse(df$Q13L == 1, 1, 0)
df$Q13M <- ifelse(df$Q13M == 1, 1, 0)
df$Q13N <- ifelse(df$Q13N == 1, 1, 0)
df$Q13O <- ifelse(df$Q13O == 1, 1, 0)

# Df of selected columns
# importance = c('Q9A', 'Q9B', 'Q9C', 'Q9D', 'Q9E')
impacts = c('Q13A', 'Q13B', 'Q13C', 'Q13D', 'Q13E', 'Q13F', 'Q13G',
  'Q13H', 'Q13I', 'Q13J', 'Q13K', 'Q13L', 'Q13M', 'Q13N', 'Q13O')
df_impacts = df[ , impacts] 

# Frequencies of response categories
alpha(df_impacts)$response.freq

# Analysis of df_impacts df
head(df_impacts)
summary(df_impacts)

# Correlation matrix
df.cor = cor(df_impacts)
df.cor # The lowest correlation is 0.23
corrplot(df.cor, type="lower", method="color", tl.pos = 'd')
cronbach.alpha(df_impacts) # Cronbach's alpha is 0.

# -----------------------------------------------------------

# RATING SCALE ANALYSIS

# 1. Fit the model
mod1 <- tam(df_impacts) 
# EAP reliability = 0.782

nitems <- ncol(df_impacts)
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
     deltas$xsi[1:ncol(df_impacts)], 
     xlim = c(0.5, 1.5), type = "n",
     xlab = "Outfit mean square", 
     ylab = "Item location in logits")

text(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:nitems],
     labels = colnames(df_impacts), 
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
     labels = colnames(df_impacts), 
     cex = 0.9, font = 2)

abline(v = 1)
abline(v = 1.30, col = "blue")
abline(v = 0.70, col = "blue")

# 10. Get the abilities
abil <- tam.wle(mod1) # get abilities
df$impacts <- abil$theta # attach abilities to df
# df$agency <- df$agency*10 + 100
hist(df$impacts) # histogram of abilities
jarque.test(df$impacts) # normality test of abilities

# 11. WrightMap
IRT.WrightMap(mod1)
