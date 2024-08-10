# LOAD PACKAGES
library(conflicted)
library(labelled)
library(tidyverse)
library(tibble)
library(dplyr)
library(TAM)
library(WrightMap)
library(psych)
library(ltm)

# READ RAW DATA
df <- read.csv("https://raw.githubusercontent.com/jackbhuber/datasets/main/Baylor_Religion_Survey_Wave_VI_2021.csv", header = TRUE)

# HAPPINESS
df$Q1[is.na(df$Q1)] <- round(mean(df$Q1, na.rm=TRUE),0) 
df$happy <- df$Q1
var_label(df$happy) <- "Happiness"

# HEALTH
df$Q2[is.na(df$Q2)] <- round(mean(df$Q2, na.rm=TRUE),0)
df$health <- df$Q2
var_label(df$health) <- "Health"

# PHYSICAL IMPAIRMENT
df$Q3[is.na(df$Q3)] <- 0
df$Q3 <- ifelse(df$Q3 == 1, 1, 0)
df$d_physical <- df$Q3
var_label(df$d_physical) <- "Physical impairment"

# EMOTIONAL DISTRESS
df$Q4A[is.na(df$Q4A)] <- round(mean(df$Q4A, na.rm=TRUE),0) # sad
df$Q4B[is.na(df$Q4B)] <- round(mean(df$Q4B, na.rm=TRUE),0) # worried
df$Q4C[is.na(df$Q4C)] <- round(mean(df$Q4C, na.rm=TRUE),0) # tense
df$Q4D[is.na(df$Q4D)] <- round(mean(df$Q4D, na.rm=TRUE),0) # anxious
df$Q4E[is.na(df$Q4E)] <- round(mean(df$Q4E, na.rm=TRUE),0) # restless
df$Q4F[is.na(df$Q4F)] <- round(mean(df$Q4F, na.rm=TRUE),0) # bored
df$Q4G[is.na(df$Q4G)] <- round(mean(df$Q4G, na.rm=TRUE),0) # angry
distress = c('Q4A', 'Q4C', 'Q4E', 'Q4G')
df_distress <- df[ , distress]
mod1 <- tam.mml(df_distress, irtmodel = "RSM") # Fit the model
abil <- tam.wle(mod1) # get abilities
df$distress <- abil$theta # attach abilities to df
var_label(df$distress) <- "Distress"
rm(distress, df_distress, mod1, abil) # clean up

# AGENCY
df$Q5A[df$Q5A == 8] <- NA
df$Q5B[df$Q5B == 8] <- NA
df$Q5C[df$Q5C == 8] <- NA
df$Q5D[df$Q5D == 8] <- NA
reverse = c('Q5A', 'Q5B', 'Q5C')
df[ , reverse] = 5 - df[ , reverse]
df$Q5A[is.na(df$Q5A)] <- round(mean(df$Q5A, na.rm=TRUE),0) # I have little control over the things that happen to me.
df$Q5B[is.na(df$Q5B)] <- round(mean(df$Q5B, na.rm=TRUE),0) # There is really no way I can solve some of the problems I have.
df$Q5C[is.na(df$Q5C)] <- round(mean(df$Q5C, na.rm=TRUE),0) # I often feel helpless in dealing with problems of life.
df$Q5D[is.na(df$Q5D)] <- round(mean(df$Q5D, na.rm=TRUE),0) # I can do just about anything I really set my mind to.
agency = c('Q5A', 'Q5B', 'Q5C')
df_agency <- df[ , agency]
mod1 <- tam.mml(df_agency, irtmodel = "RSM") # Fit the model
abil <- tam.wle(mod1) # get abilities
df$agency <- abil$theta
var_label(df$agency) <- "Perceived personal agency"
rm(reverse, agency, df_agency, mod1, abil) # clean up

# IMPORTANCE TO OTHERS
df$Q9A[is.na(df$Q9A)] <- round(mean(df$Q9A, na.rm=TRUE),0) # How much do you feel other people pay attention to you?
df$Q9B[is.na(df$Q9B)] <- round(mean(df$Q9B, na.rm=TRUE),0) # How much do you feel others would miss you if you went away?
df$Q9C[is.na(df$Q9C)] <- round(mean(df$Q9C, na.rm=TRUE),0) # How interested are people generally in what you say?
df$Q9D[is.na(df$Q9D)] <- round(mean(df$Q9D, na.rm=TRUE),0) # How much do other people depend on you?
df$Q9E[is.na(df$Q9E)] <- round(mean(df$Q9E, na.rm=TRUE),0) # How important do you feel you are to other people?
importance = c('Q9A', 'Q9B', 'Q9C', 'Q9E')
df_importance = df[ , importance] 
df_importance[df_importance == 1] <- 1
df_importance[df_importance == 2] <- 1
df_importance[df_importance == 3] <- 2
df_importance[df_importance == 4] <- 3
mod1 <- tam.mml(df_importance, irtmodel = "RSM") # Fit the model
abil <- tam.wle(mod1) # get abilities
df$importance <- abil$theta # attach abilities to df
var_label(df$importance) <- "Perceived importance to others"
rm(importance, df_importance, mod1, abil)

# COVID IMPACTS
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
df$d_infected <- ifelse(df$Q13A == 1, 1, 0) #  Been infected by COVID-19
var_label(df$d_infected) <- "Personally infected by COVID-19"
df$d_other_inf <- ifelse(df$Q13B == 1, 1, 0) #  Had relative or friend infected
var_label(df$d_other_inf) <- "Relative/friend infected by COVID-19"
df$d_lost_job <- ifelse(df$Q13C == 1, 1, 0) #  Lost your job
var_label(df$d_lost_job) <- "Lost job"
df$d_reduced_hrs <- ifelse(df$Q13D == 1, 1, 0) #  Reduced your work hours
var_label(df$d_reduced_hrs) <- "Had work hrs reduced"
df$d_missed_pmt <- ifelse(df$Q13E == 1, 1, 0) #  Missed house or rent payment(s)
var_label(df$d_missed_pmt) <- "Missed house/rent payment(s)"
df$d_evicted <- ifelse(df$Q13F == 1, 1, 0) #  Been evicted
var_label(df$d_evicted) <- "Been evicted"
df$d_pay_cut <- ifelse(df$Q13G == 1, 1, 0) #  Received a pay cut
var_label(df$d_pay_cut) <- "Received pay cut"
df$d_increased_debt <- ifelse(df$Q13H == 1, 1, 0) #  Increased your debt
var_label(df$d_increased_debt) <- "Increased your debt"
df$d_started_business <- ifelse(df$Q13I == 1, 1, 0) #  Started a business
var_label(df$d_started_business) <- "Started a business"
df$d_putoff_dr <- ifelse(df$Q13J == 1, 1, 0) #  Put off doctor for routine care
var_label(df$d_putoff_dr) <- "Put off doctor for routine care"
df$d_hungry <- ifelse(df$Q13K == 1, 1, 0) #  Gone hungry
var_label(df$d_hungry) <- "Gone hungry"
df$d_gained_wt <- ifelse(df$Q13L == 1, 1, 0) #  Gained significant weight
var_label(df$d_gained_wt) <- "Gained significant weight"
df$d_lost_wt <- ifelse(df$Q13M == 1, 1, 0) #  Lost significant weight
var_label(df$d_lost_wt) <- "Lost significant weight"
df$d_relative_movein <- ifelse(df$Q13N == 1, 1, 0) #  Had a relative move into your home
var_label(df$d_relative_movein) <- "Had a relative move in"
df$d_lost_someone <- ifelse(df$Q13O == 1, 1, 0) #  Lost close relative or friend to COVID-19
var_label(df$d_lost_someone) <- "Lost someone to COVID-19"

# COVID FEELINGS
reverse = c('Q14A', 'Q14D', 'Q14F', 'Q14H')
df[ , reverse] = 5 - df[ , reverse]
df$Q14A[is.na(df$Q14A)] <- round(mean(df$Q14A, na.rm=TRUE),0) # happy R
df$Q14B[is.na(df$Q14B)] <- round(mean(df$Q14B, na.rm=TRUE),0) # sad
df$Q14C[is.na(df$Q14C)] <- round(mean(df$Q14C, na.rm=TRUE),0) # worried
df$Q14D[is.na(df$Q14D)] <- round(mean(df$Q14D, na.rm=TRUE),0) # confident R
df$Q14E[is.na(df$Q14E)] <- round(mean(df$Q14E, na.rm=TRUE),0) # tense
df$Q14F[is.na(df$Q14F)] <- round(mean(df$Q14F, na.rm=TRUE),0) # relaxed R
df$Q14G[is.na(df$Q14G)] <- round(mean(df$Q14G, na.rm=TRUE),0) # lonely
df$Q14H[is.na(df$Q14H)] <- round(mean(df$Q14H, na.rm=TRUE),0) # cared for R
df$Q14I[is.na(df$Q14I)] <- round(mean(df$Q14I, na.rm=TRUE),0) # angry
covid_feelings = c('Q14A', 'Q14B', 'Q14C', 'Q14D', 'Q14E', 'Q14F', 'Q14G', 'Q14H', 'Q14I')
df_covid_feelings = df[ , covid_feelings] 
mod1 <- tam.mml(df_covid_feelings, irtmodel = "RSM") 
abil <- tam.wle(mod1)
df$covid_feelings <- abil$theta
var_label(df$covid_feelings) <- "Emotional changes since COVID"
rm(reverse, covid_feelings, df_covid_feelings, mod1, abil)

# ALTRUISM
df$Q19A[is.na(df$Q19A)] <- 0 # attended event
df$Q19B[is.na(df$Q19B)] <- 0 # donated money
df$Q19C[is.na(df$Q19C)] <- 0 # worked with neighbors for justice
df$Q19A <- ifelse(df$Q19A == 1, 1, 0) # attended event
df$Q19B <- ifelse(df$Q19B == 1, 1, 0) # donated money
df$Q19C <- ifelse(df$Q19C == 1, 1, 0) # worked with neighbors for justice
df$d_attended_event <- df$Q19A
df$d_donated_money <- df$Q19B
df$d_worked_w_neighbors <- df$Q19C
var_label(df$d_attended_event) <- "Attended event"
var_label(df$d_donated_money) <- "Donated money"
var_label(df$d_worked_w_neighbors) <- "Worked with neighbors for justice"

# CLOSENESS TO OTHERS
df$Q25A[is.na(df$Q25A)] <- round(mean(df$Q25A, na.rm=TRUE),0) # family
df$Q25B[is.na(df$Q25B)] <- round(mean(df$Q25B, na.rm=TRUE),0) # friends
df$Q25C[is.na(df$Q25C)] <- round(mean(df$Q25C, na.rm=TRUE),0) # co-workers
df$Q25D[is.na(df$Q25D)] <- round(mean(df$Q25D, na.rm=TRUE),0) # neighbors
df$Q25E[is.na(df$Q25E)] <- round(mean(df$Q25E, na.rm=TRUE),0) # online community
df$close_to_family <- df$Q25A
df$close_to_friends <- df$Q25B
df$close_to_coworkers <- df$Q25C
df$close_to_neighbors <- df$Q25D
df$close_to_online <- df$Q25E
var_label(df$close_to_family) <- "Feel close to family"
var_label(df$close_to_friends) <- "Feel close to friends"
var_label(df$close_to_coworkers) <- "Feel close to coworkers"
var_label(df$close_to_neighbors) <- "Feel close to neighbors"
var_label(df$close_to_online) <- "Feel close to online community"

# RELIGIOUS IDENTITY
df$d_evangelical <- ifelse(df$RELTRAD == 1, 1, 0)
df$d_evangelical[is.na(df$d_evangelical)] <- 0
df$d_mainline <- ifelse(df$RELTRAD == 2, 1, 0)
df$d_mainline[is.na(df$d_mainline)] <- 0
df$d_blackprot <- ifelse(df$RELTRAD == 3, 1, 0)
df$d_blackprot[is.na(df$d_blackprot)] <- 0
df$d_catholic <- ifelse(df$RELTRAD == 4, 1, 0)
df$d_catholic[is.na(df$d_catholic)] <- 0
other <- c(5,6)
df$d_other <- ifelse(df$RELTRAD == other, 1, 0)
df$d_other[is.na(df$d_other)] <- 0
var_label(df$d_evangelical) <- "Evangelical"
var_label(df$d_mainline) <- "Mainline"
var_label(df$d_blackprot) <- "Black Protestant"
var_label(df$d_catholic) <- "Catholic"
var_label(df$d_other) <- "Other religious tradition"

# RELIGIOSITY
df$Q47[df$Q47 == 8] <- NA
df$Q47[is.na(df$Q47)] <- round(mean(df$Q47, na.rm=TRUE), 0)
df$religiosity <- df$Q47
var_label(df$religiosity) <- "How religious"

# SPIRITUALITY
df$Q48[df$Q48 == 8] <- NA
df$Q48[is.na(df$Q48)] <- round(mean(df$Q48, na.rm=TRUE), 0)
df$spirituality <- df$Q48
var_label(df$spirituality) <- "How spiritual"

# FREQUENCY OF SACRED READING
df$Q49[is.na(df$Q49)] <- round(mean(df$Q49, na.rm=TRUE),0)
df$reading <- df$Q49
var_label(df$reading) <- "Frequency of sacred reading"

# VIEWS OF THE BIBLE
df$Q50[is.na(df$Q50)] <- 0
df$Q50[df$Q50 == 8] <- 0
df$d_bible_literal <- ifelse(df$Q50 == 1, 1, 0)
df$d_bible_true_interpret <- ifelse(df$Q50 == 2, 1, 0)
df$d_bible_human_error <- ifelse(df$Q50 == 3, 1, 0)
df$d_bible_legends <- ifelse(df$Q50 == 4, 1, 0)
var_label(df$d_bible_literal) <- "Bible exact literal"
var_label(df$d_bible_true_interpret) <- "Bible true but must interpret"
var_label(df$d_bible_human_error) <- "Bible has human error"
var_label(df$d_bible_legends) <- "Bible is history and legends"

# VIEWS OF GOD
df$Q51[is.na(df$Q51)] <- 0
df$Q51[df$Q51 == 7] <- 0
df$d_god_no_doubt <- ifelse(df$Q51 == 1, 1, 0)
df$d_god_some_doubt <- ifelse(df$Q51 == 2, 1, 0)
df$d_god_sometimes <- ifelse(df$Q51 == 3, 1, 0)
df$d_god_higher_power <- ifelse(df$Q51 == 4, 1, 0)
df$d_god_dont_know <- ifelse(df$Q51 == 5, 1, 0)
df$d_god_dont_believe <- ifelse(df$Q51 == 6, 1, 0)
var_label(df$d_god_no_doubt) <- "No doubt God exists"
var_label(df$d_god_some_doubt) <- "Believe in God with some doubt"
var_label(df$d_god_sometimes) <- "Believe in God sometimes"
var_label(df$d_god_higher_power) <- "Believe in a higher power / cosmic force"
var_label(df$d_god_dont_know) <- "God dont know and cant know"
var_label(df$d_god_dont_believe) <- "God dont believe"

# FREQUENCY OF PRAYER
df$Q54A[is.na(df$Q54A)] <- 0
df$Q54B[is.na(df$Q54B)] <- 0
df$Q54C[is.na(df$Q54C)] <- 0
df$Q54D[is.na(df$Q54D)] <- 0
df$Q54E[is.na(df$Q54E)] <- 0
df$Q54A <- df$pray_lthan_five
df$Q54B <- df$pray_five
df$Q54C <- df$pray_w_others
df$Q54D <- df$pray_meditate
df$Q54E <- df$pray_grace
#var_label(df$pray_lthan_five) <- "Pray alone - less than five"
#var_label(df$pray_five) <- "Pray alone - at least five"
#var_label(df$pray_w_others) <- "Pray with others"
#var_label(df$pray_meditate) <- "Meditate"
#var_label(df$pray_grace) <- "Say grace at meals"

# DURATION OF PRAYER
df$Q55[is.na(df$Q55)] <- round(mean(df$Q55, na.rm=TRUE),0)
df$prayer_duration <- df$Q55
var_label(df$prayer_duration) <- "Duration of prayer"

# DEMOGRAPHICS
df$Q71[is.na(df$Q71)] <- 0 # Gender
df$Q71[df$Q71 == 3] <- 0 # Gender
df$d_female <- ifelse(df$Q71 == 2, 1, 0) # Gender
df$Q72[is.na(df$Q72)] <- round(mean(df$Q72, na.rm=TRUE),0) # Age
df$age <- df$Q72 # Age
df$RACE[is.na(df$RACE)] <- 0
df$d_nonwhite <- ifelse(df$RACE == 1, 0, 1) # Nonwhite
df$Q76[is.na(df$Q76)] <- 0 # Marital status
df$d_never_married <- ifelse(df$Q76 == 1, 1, 0) # Single / Never married
df$d_separated <- ifelse(df$Q76 == 3, 1, 0) # Separated
df$d_divorced <- ifelse(df$Q76 == 4, 1, 0) # Divorced
df$d_widowed <- ifelse(df$Q76 == 5, 1, 0) # Widowed
df$d_cohabitating <- ifelse(df$Q76 == 6, 1, 0) # Domestic partnership/Living with partner
df$Q77A[is.na(df$Q77A)] <- round(mean(df$Q77A, na.rm=TRUE),0) # Education
df$education <- df$Q77A # Education
df$Q79[is.na(df$Q79)] <- round(mean(df$Q79, na.rm=TRUE),0)
df$hrs_worked <- df$Q79 # Hours worked past week
df$Q80[is.na(df$Q80)] <- round(mean(df$Q80, na.rm=TRUE),0)
df$d_lgbtq <- ifelse(df$Q80 == 3, 0, 1) # LGBTQ
df$Q81[is.na(df$Q81)] <- round(mean(df$Q81, na.rm=TRUE),0)
df$n_children <- df$Q81 # Number of children
df$Q82[is.na(df$Q82)] <- round(mean(df$Q82, na.rm=TRUE),0)
df$n_children_house <- df$Q82 # Number of children in household
df$Q85[is.na(df$Q85)] <- round(mean(df$Q85, na.rm=TRUE),0)
df$income <- df$Q85 # Total household income
var_label(df$d_female) <- "Female"
var_label(df$age) <- "Age"
var_label(df$d_nonwhite) <- "Nonwhite"
var_label(df$d_never_married) <- "Single / never married"
var_label(df$d_separated) <- "Separated"
var_label(df$d_divorced) <- "Divorced"
var_label(df$d_widowed) <- "Widowed"
var_label(df$d_cohabitating) <- "Cohabitating"
var_label(df$education) <- "Education"
var_label(df$hrs_worked) <- "Hours worked last week"
var_label(df$d_lgbtq) <- "LGBTQ"
var_label(df$n_children) <- "Number of children"
var_label(df$n_children_house) <- "Number of children in household"
var_label(df$income) <- "Total household income"

# NEW DATA FRAME
vars = c(
  'health',
  'happy',
  'd_physical',
  'distress',
  'agency',
  'importance',
  'covid_feelings',
  'd_attended_event',
  'd_donated_money',
  'd_worked_w_neighbors',
  'close_to_family',
  'close_to_friends',
  'close_to_coworkers',
  'close_to_neighbors',
  'close_to_online',
  'religiosity',
  'spirituality',
  'd_evangelical',
  'd_mainline',
  'd_blackprot',
  'd_catholic',
  'd_other',
  'reading',
  'd_bible_literal',
  'd_bible_true_interpret',
  'd_bible_human_error',
  'd_bible_legends',
  #'pray_lthan_five',
  #'pray_five',
  #'pray_w_others',
  #'pray_meditate',
  #'pray_grace',
  #'prayer_duration',
  'd_god_no_doubt',
  'd_god_some_doubt',
  'd_god_sometimes',
  'd_god_higher_power',
  'd_god_dont_know',
  'd_god_dont_believe',
  'age',
  'd_female',
  'd_nonwhite',
  'd_never_married',
  'd_separated',
  'd_divorced',
  'd_widowed',
  'd_cohabitating',
  'education',
  'hrs_worked',
  'd_lgbtq',
  'n_children',
  'n_children_house',
  'income'
)
df2 <- df[ , vars]
