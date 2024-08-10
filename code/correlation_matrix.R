vars2 = c(
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
  'close_to_online'
  )
df3 <- df2[ , vars2]

library(corrplot)
df.cor <- cor(df3)
corrplot(df.cor, type="lower", method="color", tl.pos = 'd')

