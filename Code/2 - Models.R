
# Packages ----------------------------------------------------------------

library("dplyr")
library("caret")
library("MASS")

# Data --------------------------------------------------------------------

load(file="Data/data_train_discrete.RData")

# Il faut enlever les variables avec des NA. 
ind <- sapply(data_train_discrete,any.na)
ind <- which(ind==TRUE)
attr(ind,"names")

data_train_discrete <- select(data_train_discrete,-ind)


# Modele logistic ---------------------------------------------------------

logit <- glm(CIBLE_NUM ~ ., data=data_train_discrete, family="binomial")
stepAIC(logit)


