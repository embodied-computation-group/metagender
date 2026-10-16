# prep data for hierarchical modelling ------------------------------------
rm(list = ls())

library(dplyr)

# load data

megatable <- read.csv('../data/megatable.csv')
data_meta <- read.csv('../data/metacognition_TrialData_master.csv')

model_data <- data_meta[,c(1:5,7)]
colnames(model_data)[c(1,2)] <- cbind("subj", "mod")

incl <- megatable$subj
unq <- unique(data_meta$subj)
excl <- unq[!unq %in% incl]

for (i in excl){
  model_data <- filter(model_data, subj!=i)
}

# check subj numbers
excl %in% model_data$subj
unique(model_data$subj)
# nope nice

# edit in genders
model_data_fin <- inner_join(megatable[,c(2,4)], model_data, by = "subj")

# write as table
write.csv(model_data_fin, file = "../data/Hmetad_data.csv")
write.csv(model_data_fin, file = "../metad/Matlab/Hmetad_data.csv")
