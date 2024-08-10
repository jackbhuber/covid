# ---------- EMOTIONAL IMPACT OF COVID ---------- #

# LOAD DATA
df <- read.csv("https://raw.githubusercontent.com/jackbhuber/datasets/main/Baylor_Religion_Survey_Wave_VI_2021.csv", header = TRUE)

# Original coding
# 1) Less often	437	35.4
# 2) About the same	668	54.2
# 3) A little more	80	6.5
# 4) Much more often	48	3.9
# Missing	15

# Reverse code
reverse = c('Q14A', 'Q14D', 'Q14F', 'Q14H')
df[ , reverse] = 5 - df[ , reverse]

# Impute mean to missing  values
df$Q14A[is.na(df$Q14A)] <- round(mean(df$Q14A, na.rm=TRUE),0) # happy R
df$Q14B[is.na(df$Q14B)] <- round(mean(df$Q14B, na.rm=TRUE),0) # sad
df$Q14C[is.na(df$Q14C)] <- round(mean(df$Q14C, na.rm=TRUE),0) # worried
df$Q14D[is.na(df$Q14D)] <- round(mean(df$Q14D, na.rm=TRUE),0) # confident R
df$Q14E[is.na(df$Q14E)] <- round(mean(df$Q14E, na.rm=TRUE),0) # tense
df$Q14F[is.na(df$Q14F)] <- round(mean(df$Q14F, na.rm=TRUE),0) # relaxed R
df$Q14G[is.na(df$Q14G)] <- round(mean(df$Q14G, na.rm=TRUE),0) # lonely
df$Q14H[is.na(df$Q14H)] <- round(mean(df$Q14H, na.rm=TRUE),0) # cared for R
df$Q14I[is.na(df$Q14I)] <- round(mean(df$Q14I, na.rm=TRUE),0) # angry

# Df of selected columns
covid_feelings = c('Q14A', 'Q14B', 'Q14C', 'Q14D', 'Q14E', 'Q14F', 'Q14G', 'Q14H', 'Q14I')
df_covid_feelings = df[ , covid_feelings] 

# Collapsing categories as necessary
# df_importance[df_importance == 1] <- 1
# df_importance[df_importance == 2] <- 1
# df_importance[df_importance == 3] <- 2
# df_importance[df_importance == 4] <- 3

# Frequencies of response categories
alpha(df_covid_feelings)$response.freq

# Analysis of df_paffect df
head(df_covid_feelings)
summary(df_covid_feelings)

# Correlation matrix
df.cor = cor(df_covid_feelings)
df.cor # The lowest correlation is 0.23
corrplot(df.cor, type="lower", method="color", tl.pos = 'd')
cronbach.alpha(df_covid_feelings) # Cronbach's alpha is 0.837

# -----------------------------------------------------------

# RATING SCALE ANALYSIS

# 1. Fit the model
mod1 <- tam.mml(df_covid_feelings, irtmodel = "RSM") 
# EAP reliability = 0.845

nitems <- ncol(df_covid_feelings)
deltas <- mod1$xsi[1:nitems, ]

# 2. Summary statistics
summary(mod1) # model summary

# 3. Inspect Yen’s Q3 and adjusted Q3 statistics
q3 <- tam.modelfit(mod1)$stat.itempair
q3 <- data.frame(q3[1:2], round(q3[5:6], 2), round(q3[7:8], 4))
q3

# Inspect Root Mean Squared Discrepancy statistics
(item.RMSD <- IRT.itemfit(mod1)$RMSD_bc)

# Find and store the standardized residuals
myTAMresid <- IRT.residuals(mod1)$stand_residuals

# Scree plot of the standardized residuals
psych::scree(myTAMresid, 
             factors = FALSE)

# Perform unrotated principal components analysis of the residuals
(pcaresid <- pca(myTAMresid,
                 nfactors = ncol(myTAMresid),
                 rotate = "none"))

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
     deltas$xsi[1:ncol(df_covid_feelings)], 
     xlim = c(0.5, 1.5), type = "n",
     xlab = "Outfit mean square", 
     ylab = "Item location in logits")

text(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:nitems],
     labels = colnames(df_covid_feelings), 
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
     labels = colnames(df_covid_feelings), 
     cex = 0.9, font = 2)

abline(v = 1)
abline(v = 1.30, col = "blue")
abline(v = 0.70, col = "blue")

# 10. Get the abilities
abil <- tam.wle(mod1) # get abilities
df$covid_feelings <- abil$theta # attach abilities to df
# df$agency <- df$agency*10 + 100
hist(df$covid_feelings) # histogram of abilities
jarque.test(df$covid_feelings) # normality test of abilities

# 11. WrightMap
IRT.WrightMap(mod1)
