
# Packages ----------------------------------------------------------------

library("ggplot2")
library("dplyr")
library("data.table")
library("Hmisc")
source("Code/Functions.R")


# Data --------------------------------------------------------------------

data <- fread("Data/Base scoring.csv")

attach(data)

data$CIBLE <- as.factor(CIBLE)
data$FLAG_CLIENT <- as.factor(FLAG_CLIENT)
data$FLAG_CLIENT_M6 <- as.factor(FLAG_CLIENT_M6)
data$FLAG_CDD <- as.factor(FLAG_CDD)
data$FLAG_CARTE_RETRAIT <- as.factor(FLAG_CARTE_RETRAIT)
data$FLAG_CARTE_PAIEMENT <- as.factor(FLAG_CARTE_PAIEMENT)
data$FLAG_CBI <- as.factor(FLAG_CBI)
data$FLAG_VISA1 <- as.factor(FLAG_VISA1)
data$FLAG_DEBIT_DIFFERE <- as.factor(FLAG_DEBIT_DIFFERE)
data$FLAG_FORFAIT <- as.factor(FLAG_FORFAIT)
data$FLAG_LVA <- as.factor(FLAG_LVA)
data$FLAG_LVB <- as.factor(FLAG_LVB)
data$FLAG_CDV <- as.factor(FLAG_CDV)
data$FLAG_LVJ <- as.factor(FLAG_LVJ)
data$FLAG_LEP <- as.factor(FLAG_LEP)
data$FLAG_LEL <- as.factor(FLAG_LEL)
data$FLAG_PEL <- as.factor(FLAG_PEL)
data$FLAG_PEP <- as.factor(FLAG_PEP)
data$FLAG_CAT <- as.factor(FLAG_CAT)
data$FLAG_CTO <- as.factor(FLAG_CTO)
data$FLAG_PEA <- as.factor(FLAG_PEA)
data$FLAG_ASV <- as.factor(FLAG_ASV)
data$FLAG_AUTO <- as.factor(FLAG_AUTO)
data$FLAG_MRH <- as.factor(FLAG_MRH)
data$FLAG_GAV <- as.factor(FLAG_GAV)
data$FLAG_GU <- as.factor(FLAG_GU)
data$FLAG_GF <- as.factor(FLAG_GF)
data$FLAG_SANTE <- as.factor(FLAG_SANTE)
data$FLAG_CONSO <- as.factor(FLAG_CONSO)
data$FLAG_IMMO <- as.factor(FLAG_IMMO)
data$FLAG_REVOL <- as.factor(FLAG_REVOL)
data$NB_PRODUITS_ACT <- as.factor(NB_PRODUITS_ACT)

summary(data)

describe(data)


# Valeurs aberrantes ----------------------------------------------------

# Pour les indicatrices.

datatest1 <- select(data,matches("FLAG_"))
sapply(datatest1,table)

# Variable à supprimer:
# FLAG_CLIENT (50 000), 
# FLAG_CLIENT_M6 (203), FLAG_VISA1 (8), FLAG_DEBIT_DIFFERE (40),
# => indicatrice carte avec les autres vaiables concernées.
# FLAG_LVJ (0) => SF_LVJ et SF_LVJ_M6
# Les assurances sont aggrégées entre elles (sauf avec FLAG_ASV). 
# FLAG_SANTE (0).

data <- select(data,-c(FLAG_CLIENT,FLAG_CLIENT_M6,FLAG_LVJ,FLAG_SANTE,SF_LVJ,SF_LVJ_M6))

# Pour les nombres de mouvements.

datatest2 <- select(data,matches("NB_"))
sapply(datatest2,any.negative)
# Tous les nombres de mouvement sont positifs.

# Pour les montants

datatest3 <- select(data,matches("MT_"))
sapply(datatest3,any.negative)
# MT_EP_DISPO et MT_PAIEMENT_CARTE ont des valeurs négatives.

filter(data,MT_PAIEMENT_CARTE<0)
# ID_CLIENT = 37 384.

data <- filter(data,ID_CLIENT!=37384)


# Données manquantes ------------------------------------------------------

# Indicatrices

sapply(datatest1,any.na)
# Pas de valeurs manquantes dans les indicatrices.

# CDD

datatest4 <- select(data,matches("_CDD"))

names(datatest4)

all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(SF_CDD)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(NB_JOURS_CRE_CDD)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(NB_JOURS_DEB_CDD)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(NB_MVT_CRE_CDD_M)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(NB_MVT_DEB_CDD_M)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(MT_MVT_CRE_CDD_M)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(MT_MVT_DEB_CDD_M)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(NB_MVT_CRE_CDD_3M)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(MT_MVT_CRE_CDD_3M)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(NB_MVT_CDD_12M)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(MOY_NB_MVT_CDD_12M)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(MOY_MVT_CRE_CDD_12M)))
all.equal(filter(datatest4,FLAG_CDD==0),filter(datatest4,is.na(MOY_MVT_DEB_CDD_12M)))
# Les NA concordent avec le fait de ne pas avoir de CDD.

# Les autres produits

datatest5 <- select(data,ID_CLIENT,matches("_LVA"))
all.equal(filter(datatest5,FLAG_LVA==0),filter(datatest5,is.na(SF_LVA)))

datatest5 <- select(data,ID_CLIENT,matches("_LVB"))
all.equal(filter(datatest5,FLAG_LVB==0),filter(datatest5,is.na(SF_LVB)))

datatest5 <- select(data,ID_CLIENT,matches("_CDV"))
all.equal(filter(datatest5,FLAG_CDV==0),filter(datatest5,is.na(SF_CDV)))

datatest5 <- select(data,ID_CLIENT,matches("_LEP"))
all.equal(filter(datatest5,FLAG_LEP==0),filter(datatest5,is.na(SF_LEP)))

datatest5 <- select(data,ID_CLIENT,matches("_LEL"))
all.equal(filter(datatest5,FLAG_LEL==0),filter(datatest5,is.na(SF_LEL)))

datatest5 <- select(data,ID_CLIENT,matches("_PEL"))
all.equal(filter(datatest5,FLAG_PEL==0),filter(datatest5,is.na(SF_PEL)))

datatest5 <- select(data,ID_CLIENT,matches("_PEP"))
all.equal(filter(datatest5,FLAG_PEP==0),filter(datatest5,is.na(SF_PEP)))

datatest5 <- select(data,ID_CLIENT,matches("_CAT"))
all.equal(filter(datatest5,FLAG_CAT==0),filter(datatest5,is.na(SF_CAT)))

datatest5 <- select(data,ID_CLIENT,matches("_CTO"))
all.equal(filter(datatest5,FLAG_CTO==0),filter(datatest5,is.na(SF_CTO)))

datatest5 <- select(data,ID_CLIENT,matches("_PEA"))
all.equal(filter(datatest5,FLAG_PEA==0),filter(datatest5,SF_PEA==0))

datatest5 <- select(data,ID_CLIENT,matches("_ASV"))
all.equal(filter(datatest5,FLAG_ASV==0),filter(datatest5,is.na(SF_ASV)))

# OK

# Les cartes

datatest6 <- select(data,ID_CLIENT,FLAG_CBI,FLAG_VISA1,FLAG_DEBIT_DIFFERE,contains("_CARTE"))
all.equal(filter(datatest6,FLAG_CARTE_RETRAIT==0),filter(datatest6,is.na(NB_RETRAIT_CARTE)))
all.equal(filter(datatest6,FLAG_CARTE_RETRAIT==0),filter(datatest6,is.na(MT_RETRAIT_CARTE)))
all.equal(filter(datatest6,FLAG_CARTE_PAIEMENT==0),filter(datatest6,is.na(NB_PAIEMENT_CARTE)))
all.equal(filter(datatest6,FLAG_CARTE_PAIEMENT==0),filter(datatest6,is.na(MT_PAIEMENT_CARTE)))

# Différences peut-être dues au fait qu'une carte de paiement permet de retirer

# Age, Anc,Anc dernier mvt, NB_PRODUIT et MT decouvert autorisé
filter(data,is.na(AGE))
filter(data,is.na(ANC))
filter(data,is.na(NB_PRODUITS_ACT))
filter(data,is.na(ANC_DER_MVT))
filter(data,is.na(MT_DECOUVERT))

# OK


# Cohérence des variables -------------------------------------------------

# SF_TOT
datatest7 <- data %>% select(ID_CLIENT,SF_TOT,SF_CDD,SF_HORS_CDD) %>% 
  mutate(DIFF=SF_CDD+SF_HORS_CDD-SF_TOT) %>% filter(DIFF!=0)


# MT_EP_DISPO
data %>% select(ID_CLIENT,MT_EP_DISPO,SF_CDD,SF_LEL,SF_LEP,SF_LVA,SF_LVB,SF_CDV) %>% 
  mutate(DIFF=SF_CDD+SF_LEL+SF_LEP+SF_LVA+SF_LVB+SF_CDV-MT_EP_DISPO) %>% filter(DIFF!=0)
# ID_CLIENT: 377 251

data <- filter(data,ID_CLIENT!=377251)


# Gestion de l'âge --------------------------------------------------------

describe(AGE)

data <- filter(data,AGE<80)


# Récupération de la base nettoyée ------------------------------------------------

save(data,file="Data/data.RData")
