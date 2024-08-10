# ---------- NEGATIVE PRAYER AFFECT SCALE ---------- #

# ----- LOAD PACKAGES
library(conflicted)
library(labelled)
library(tidyverse)
library(tibble)
library(dplyr)
library(TAM)
library(WrightMap)
library(psych)
library(ltm)
library(moments)

# LOAD DATA
df <- read.csv("https://raw.githubusercontent.com/jackbhuber/datasets/main/Baylor_Religion_Survey_Wave_VI_2021.csv", header = TRUE)

# Deal with missing data
df$Q58C[is.na(df$Q58C)] <- round(mean(df$Q58C, na.rm=TRUE),0) # sad R
df$Q58D[is.na(df$Q58D)] <- round(mean(df$Q58D, na.rm=TRUE),0) # worried R
df$Q58F[is.na(df$Q58F)] <- round(mean(df$Q58F, na.rm=TRUE),0) # tense R
df$Q58H[is.na(df$Q58H)] <- round(mean(df$Q58H, na.rm=TRUE),0) # lonely R

# Df of negative affect columns
# naffect = c('Q58C', 'Q58D', 'Q58F', 'Q58H') # select these columns
naffect = c('Q58C', 'Q58F', 'Q58H') # select these columns
df_naffect <- df[ , naffect] # a small df of these columns

# Collapsing Category 4 into 5
df_naffect[df_naffect == 5] <- 4

# Analysis of df_paffect df
head(df_naffect)
summary(df_naffect)

# Correlation matrix
df.cor = cor(df_naffect)
df.cor # The lowest correlation is 0.58
corrplot(df.cor, type="lower", method="color", tl.pos = 'd')
cronbach.alpha(df_naffect) # Cronbach's alpha is 0.904

df_paffect_sum <- df$Q58A + df$Q58B + df$Q58E + df$Q58G + df$Q58I
df_paffect_sum <- data.frame(df_paffect_sum)
summary(df_paffect_sum)
df_paffect_sum <- as.data.frame(df_paffect_sum)
hist(df_paffect_sum)

df_paffect$sum <- df$Q58A + df$Q58B + df$Q58E + df$Q58G + df$Q58I
hist(df_paffect$sum)

skewness(df_paffect$sum) # value of zero is normal
kurtosis(df_paffect$sum) # kurtosis of normal distribution is 3

jarque.test(df_paffect$sum)

df_paffect$sum <- NULL

head(df_paffect)

# RATING SCALE ANALYSIS
# 1. Fit the model
mod1 <- tam.mml(df_naffect, irtmodel = "RSM") 
# EAP reliability = 0.888

# 2. Summary statistics
summary(mod1) # model summary
# Category thresholds increase in difficulty as expected
# Q58G and Q58I are almost exactly the same difficulty

# 3. Inspect the item parameters 
# and centralized category thresholds
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
     overlay = TRUE,
     observed = FALSE)

# 5. Item fit statistics
fit <- msq.itemfit(mod1)$itemfit
(fit <- data.frame(fit[1], round(fit[3:8], 2)))
# 

# 6. Item difficulties
# hist(diffic$xsi, breaks=10) # histogram of item difficulties


### Plot a construct alley using outfit
plot(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:ncol(df_paffect)], 
     xlim = c(0.5, 1.5), type = "n",
     xlab = "Outfit mean square", 
     ylab = "Item location in logits")

text(msq.itemfit(mod1)$itemfit$Outfit, 
     deltas$xsi[1:nitems],
     labels = colnames(df_paffect), 
     cex = 0.9, 
     font = 2)

abline(v = 1)
abline(v = 1.30, col = "blue")
abline(v = 0.70, col = "blue")

# 7. Abilities
abil <- tam.wle(mod1) # get abilities
df$prayer_paffect <- abil$theta # attach abilities to df
hist(df$prayer_paffect) # histogram of abilities
jarque.test(df$prayer_paffect) # normality test of abilities



head(df_paffect)




