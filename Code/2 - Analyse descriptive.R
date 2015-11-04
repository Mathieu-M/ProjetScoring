
# Packages ----------------------------------------------------------------

library("dplyr")
library("ggplot2")
source("Functions.r")


# Etude bivariée des corrélations avec CIBLE ----------------------------------------------------


# AGE ---------------------------------------------------------------------




# NB_PRODUITS_ACT

ggplot(aes(CIBLE, AGE, fill=NB_PRODUITS_ACT), data = data) + geom_boxplot() +
  ggtitle("Age en fonction de cible et NB_PRODUITS_ACT")


ggplot(data=data, aes(x=AGE, fill=CIBLE)) + 
  geom_histogram(binwidth=10,color='black',position='fill') + facet_grid(NB_PRODUITS_ACT~.) + 
  ggtitle("Cible par age suivant le nombre de produits detenus") + geom_hline(yintercept=0.8,size=0.8) + 
  coord_cartesian(ylim = c(0.4, 1)) + xlab("Age") + ylab("Pourcentage") + scale_fill_brewer(palette="Dark2")



ggplot(data=data, aes(x=AGE)) +
  geom_histogram(color="black",binwidth = 10) + facet_grid(CIBLE ~ .) + 
  ggtitle("Nombre de CIBLE suivant l'âge")

ggplot(data=data, aes(x=ANC, fill=CIBLE)) +
  geom_histogram(color="black",binwidth = 5) + 
  ggtitle("Histogramme de CIBLE suivant ANC")

ggplot(data=data, aes(x=ANC,fill=NB_PRODUITS_ACT)) +
  geom_histogram(color="black",binwidth = 5) + facet_grid(CIBLE ~ .) + 
  ggtitle("Histogramme de CIBLE suivant ANC et NB_PRODUITS_ACT")

ggplot(aes(CIBLE, ANC, fill=NB_PRODUITS_ACT), data = data) + geom_boxplot() +
  ggtitle("ANC en fonction de CIBLE et NB_PRODUITS_ACT")

ggplot(aes(CIBLE, ANC, fill=FLAG_LVA), data = data) + geom_boxplot() +
  ggtitle("ANC en fonction de CIBLE et FLAG_LVA")

ggplot(aes(CIBLE, SF_TOT), data = data) + geom_boxplot() +
  ggtitle("SF_TOT en fonction de cible")

describe(data$SF_TOT)

ggplot(data=filter(data,SF_TOT<44700,SF_TOT>0), aes(x=SF_TOT, fill=CIBLE)) +
  geom_histogram(color="black",binwidth = 1000) + 
  ggtitle("Nombre de CIBLE suivant SF_TOT (0<SF_TOT<44700")

describe(data$SF_HORS_CDD)

ggplot(data=filter(data,SF_HORS_CDD<44700), aes(x=SF_HORS_CDD, fill=CIBLE)) +
  geom_histogram(color="black",binwidth = 1000) + 
  ggtitle("Nombre de CIBLE suivant SF_HORS_CDD")

describe(data$MT_EP_DISPO)

ggplot(data=filter(data,MT_EP_DISPO<28000,MT_EP_DISPO>0), aes(x=MT_EP_DISPO, fill=CIBLE)) +
  geom_histogram(color="black",binwidth = 1000) + 
  ggtitle("Nombre de CIBLE suivant MT_EP_DISPO")


# Crédit ------------------------------------------------------------------

# FLAG_REVOL

tablecont <- table("cible"=data$CIBLE,"FLAG_REVOL"=data$FLAG_REVOL)
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_REVOL))

# FLAG_IMMO

tablecont <- table("cible"=data$CIBLE,"FLAG_IMMO"=data$FLAG_IMMO)
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_IMMO))

# FLAG_CONSO

tablecont <- table("cible"=data$CIBLE,"FLAG_CONSO"=data$FLAG_CONSO)
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_CONSO))

# Somme

describe(data$FLAG_CONSO)

data <- mutate(data,somme_credit=(as.numeric(FLAG_REVOL)+as.numeric(FLAG_IMMO)+
                                    as.numeric(FLAG_CONSO))-3)

tablecont <- table("cible"=data$CIBLE,"somme_credit"=data$somme_credit)
sqrt(chisq.test(tablecont)$statistic/length(data$somme_credit))

pourcentage <- tablecont[2,]/(tablecont[1,]+tablecont[2,])

# Indicatrice 

data <- mutate(data,FLAG_credit=!(somme_credit==0))

tablecont <- table("cible"=data$CIBLE,"FLAG_credit"=data$FLAG_credit)
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_credit))

pourcentage <- tablecont[2,]/(tablecont[1,]+tablecont[2,])


# Assurance ---------------------------------------------------------------

# FLAG_SANTE

describe(data$FLAG_SANTE)

# FLAG_GF

describe(data$FLAG_GF)

# FLAG_GU

describe(data$FLAG_GU)

# FLAG_GAV

describe(data$FLAG_GAV)

# FLAG_MRH

describe(data$FLAG_MRH)

# FLAG_AUTO

describe(data$FLAG_AUTO)

# FLAG_ASV

describe(data$FLAG_ASV)

tablecont <- table("cible"=data$CIBLE,"FLAG_ASV"=data$FLAG_ASV)
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_ASV))

pourcentage <- tablecont[2,]/(tablecont[1,]+tablecont[2,])

# Indicatrice

data <- mutate(data,somme_assurance=(as.numeric(FLAG_SANTE)+as.numeric(FLAG_GF)+
                                       as.numeric(FLAG_GU)+as.numeric(FLAG_GAV)+
                                       as.numeric(FLAG_MRH)+as.numeric(FLAG_AUTO)+
                                       as.numeric(FLAG_ASV))-7)
data <- mutate(data,FLAG_assurance=!(somme_assurance==0))

describe(data$FLAG_assurance)

tablecont <- table("cible"=data$CIBLE,"FLAG_assurance"=data$FLAG_assurance)
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_assurance))

pourcentage <- tablecont[2,]/(tablecont[1,]+tablecont[2,])


# FLAG_PEA ----------------------------------------------------------------

tablecont <- table("cible"=data$CIBLE,"FLAG_PEA"=data$FLAG_PEA)
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_PEA))

pourcentage <- tablecont[2,]/(tablecont[1,]+tablecont[2,])


# FLAG_CTO ----------------------------------------------------------------

tablecont <- table("cible"=data$CIBLE,"FLAG_CTO"=data$FLAG_CTO)
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_CTO))

pourcentage <- tablecont[2,]/(tablecont[1,]+tablecont[2,])


# FLAG_CAT ----------------------------------------------------------------

tablecont <- table("cible"=data$CIBLE,"FLAG_CAT"=data$FLAG_CAT)
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_CAT))

(pourcentage <- tablecont[2,]/(tablecont[1,]+tablecont[2,]))


# FLAG_PEP ----------------------------------------------------------------

tablecont <- table("cible"=data$CIBLE,"FLAG_PEP"=data$FLAG_PEP)
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_PEP))

(pourcentage <- tablecont[2,]/(tablecont[1,]+tablecont[2,]))


# FLAG_PEL ----------------------------------------------------------------

(tablecont <- table("cible"=data$CIBLE,"FLAG_PEL"=data$FLAG_PEL))
sqrt(chisq.test(tablecont)$statistic/length(data$FLAG_PEL))

(pourcentage <- tablecont[2,]/(tablecont[1,]+tablecont[2,]))

cor.test(as.numeric(data$FLAG_CAT),as.numeric(data$FLAG_PEL))
