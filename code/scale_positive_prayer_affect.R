# ---------- POSITIVE PRAYER AFFECT SCALE ---------- #

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
df$Q58A[is.na(df$Q58A)] <- round(mean(df$Q58A, na.rm=TRUE),0) # connected to God
df$Q58B[is.na(df$Q58B)] <- round(mean(df$Q58B, na.rm=TRUE),0) # happy
df$Q58E[is.na(df$Q58E)] <- round(mean(df$Q58E, na.rm=TRUE),0) # confident 
df$Q58G[is.na(df$Q58G)] <- round(mean(df$Q58G, na.rm=TRUE),0) # relaxed
df$Q58I[is.na(df$Q58I)] <- round(mean(df$Q58I, na.rm=TRUE),0) # cared for

# Df of positive affect columns
# paffect = c('Q58A', 'Q58B', 'Q58E', 'Q58G', 'Q58I') # select these columns
paffect = c('Q58B', 'Q58E', 'Q58G') # select these columns
df_paffect <- df[ , paffect] # a small df of these columns

# Analysis of df_paffect df
head(df_paffect)
summary(df_paffect)

# Correlation matrix
df.cor = cor(df_paffect)
df.cor # The lowest correlation is 0.58
corrplot(df.cor, type="lower", method="color", tl.pos = 'd')
cronbach.alpha(df_paffect) # Cronbach's alpha is 0.904

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
mod1 <- tam.mml(df_paffect, irtmodel = "RSM") 
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
     package = "lattice", 
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

# Collapsing categories as necessary
df_paffect[df_paffect == 2] <- 1
df_paffect[df_paffect == 3] <- 2
df_paffect[df_paffect == 4] <- 3
df_paffect[df_paffect == 5] <- 4


head(df_paffect)




