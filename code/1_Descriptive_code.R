# clear environment and load core tables ----------------------------------
# clear environment
rm(list = ls())

# megatable 
megatable <- read.csv('../data/megatable.csv')
megatable_long <- read.csv('../data/megatable_long.csv')

# remove useless first columns
megatable <- megatable[,-1]
megatable_long <- megatable_long[,-1]


# Descriptive statistics --------------------------------------------------

# number of males vs. females
table(megatable$gender)

# age information
min(megatable$age)
max(megatable$age)

mean(megatable$age[megatable$gender == "Feminin"])
mean(megatable$age[megatable$gender == "Masculin"])

sd(megatable$age[megatable$gender == "Feminin"])
sd(megatable$age[megatable$gender == "Masculin"])


