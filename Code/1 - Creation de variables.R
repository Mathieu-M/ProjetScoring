
# Packages ----------------------------------------------------------------

library("ggplot2")
library("dplyr")
library("data.table")
library("Hmisc")
source("Code/Functions.R")


# Data --------------------------------------------------------------------

load(file="Data/data.RData")
attach(data)

# Pour les cartes -------------------------------------------------

# Pour les cartes de paiement
data <- mutate(data,NB_CARTE=as.numeric(FLAG_CARTE_PAIEMENT)+
                 as.numeric(FLAG_CBI)+
                 as.numeric(FLAG_VISA1)+
                 as.numeric(FLAG_DEBIT_DIFFERE)+
                 as.numeric(FLAG_CARTE_RETRAIT)-5,
               NB_CB_PAIEMENT=as.numeric(FLAG_CARTE_PAIEMENT)+
                 as.numeric(FLAG_CBI)+
                 as.numeric(FLAG_VISA1)+
                 as.numeric(FLAG_DEBIT_DIFFERE)-4)

data <- mutate(data,FLAG_CARTE=as.numeric(NB_CARTE>=1))
data <- mutate(data,FLAG_CB_PAIEMENT=as.numeric(NB_CB_PAIEMENT>=1))

data$FLAG_CARTE <- as.factor(data$FLAG_CARTE)
data$FLAG_CB_PAIEMENT <- as.factor(data$FLAG_CB_PAIEMENT)


# Pour les assurances -----------------------------------------------------

data <- mutate(data,NB_ASSURANCE=as.numeric(FLAG_AUTO)+
                 as.numeric(FLAG_MRH)+
                 as.numeric(FLAG_GAV)+
                 as.numeric(FLAG_GU)+
                 as.numeric(FLAG_GF)-5)

data <- mutate(data,FLAG_ASSURANCE=as.numeric(NB_ASSURANCE>=1))
data$FLAG_ASSURANCE <- as.factor(data$FLAG_ASSURANCE)


# Pour les crédits --------------------------------------------------------

data <- mutate(data,NB_CREDIT=as.numeric(FLAG_CONSO)+
                 as.numeric(FLAG_IMMO)+
                 as.numeric(FLAG_REVOL)-3)

data <- mutate(data,FLAG_CREDIT=as.numeric(NB_CREDIT>=1))
data$FLAG_CREDIT <- as.factor(data$FLAG_CREDIT)


# Pour les livrets --------------------------------------------------------

data <- mutate(data,NB_LIVRETS=as.numeric(FLAG_LEL)+as.numeric(FLAG_LEP)+as.numeric(FLAG_LVA)+
                 as.numeric(FLAG_LVB)-4,
               NB_LIV_DIFF_A=as.numeric(FLAG_LEL)+as.numeric(FLAG_LEP)+as.numeric(FLAG_LVB)-3)

data <- mutate(data,FLAG_LIVRETS=as.factor(as.numeric(NB_LIVRETS>=1)),
               FLAG_LIV_DIFF_A=as.factor(as.numeric(NB_LIV_DIFF_A>=1)))


# Pour l'épargne financière -----------------------------------------------

data <- mutate(data,NB_EP_FIN=as.numeric(FLAG_CTO)+as.numeric(FLAG_CAT)+as.numeric(FLAG_PEL)+
                 as.numeric(FLAG_PEP)+as.numeric(FLAG_PEA)+as.numeric(FLAG_ASV)-6)

data <- mutate(data,FLAG_EP_FIN=as.factor(as.numeric(NB_EP_FIN>=1)))


# Variables d'évolution des encours ---------------------------------------

data <- mutate(data,EV_SF_TOT=SF_TOT/SF_TOT_M6,
               EV_SF_HORS_CDD=SF_HORS_CDD/SF_HORS_CDD_M6,
               EV_SF_CDD=SF_CDD/SF_CDD_M6,
               EV_SF_LVA=SF_LVA/SF_LVA_M6,
               EV_SF_LVB=SF_LVB/SF_LVB_M6,
               EV_SF_CDV=SF_CDV/SF_CDV_M6,
               EV_SF_LEL=SF_LEL/SF_LEL_M6,
               EV_SF_LEP=SF_LEP/SF_LEP_M6,
               EV_SF_PEL=SF_PEL/SF_PEL_M6,
               EV_SF_PEP=SF_PEP/SF_PEP_M6,
               EV_SF_CAT=SF_CAT/SF_CAT_M6,
               EV_SF_CTO=SF_CTO/SF_CTO_M6,
               EV_SF_PEA=SF_PEA/SF_PEA_M6,
               EV_SF_ASV=SF_ASV/SF_ASV_M6)


# Pour les mouvements de cartes -------------------------------------------

data <- mutate(data,NB_MVT_M=NB_MVT_CRE_CDD_M+NB_MVT_DEB_CDD_M,
               NB_MVT_3M=NB_MVT_CRE_CDD_3M+NB_MVT_DEB_CDD_3M,
               MT_MOY_M=(MT_MVT_CRE_CDD_M*NB_MVT_CRE_CDD_M+MT_MVT_DEB_CDD_M*NB_MVT_DEB_CDD_M)/NB_MVT_M,
               NB_UTIL_CARTE=NB_RETRAIT_CARTE+NB_PAIEMENT_CARTE,
               MT_MOY_CARTE=(MT_RETRAIT_CARTE*NB_RETRAIT_CARTE+MT_PAIEMENT_CARTE*NB_PAIEMENT_CARTE)/NB_UTIL_CARTE)

