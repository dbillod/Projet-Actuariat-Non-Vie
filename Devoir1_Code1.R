
###################################
## On charge les données ##########
###################################
rm(list=ls())
download.file(url="http://freakonometrics.free.fr/base_ensae_1.RData",destfile="base.RData")
load("base.RData")

base_ensae_1 = read.csv2("C:/Users/David/Documents/ENSAE/3A/Actuariat Non-Vie/base_ensae_1.csv")
base_ensae_1 = base_ensae_1[,-1]

#si besoin d'écrire : 
#regarder le wd : getwd()
#écrire : write.csv2(pricing, file='blabla.csv')

pricing <- read.csv2("http://freakonometrics.free.fr/pricing.csv")
dim(pricing)
##La base a 36311 lignes et 15 colonnes

pricing = read.csv2("C:/Users/David/Documents/ENSAE/3A/Actuariat Non-Vie/pricing.csv")
pricing = pricing[,-1]

## La base s'appelle base_ensae_1

summary(base_ensae_1)
dim(base_ensae_1)
# La base a 100 021 lignes, pour 20 colonnes. La 1ère colonne est le numéro de police

#Copions la pour avoir une base de travail
db1 = base_ensae_1
n_db1 = dim(base_ensae_1)[1]
attach(db1)


##Idée : 1) fitter un logit, puis tester sa qualité -> déviance, résidus de Pearson
## 2) Utiliser des splines ou une base polynomiale : illustrer le choix du nombre 
##de splines ou de degré optimal


#########################################
## Commençons par recoder la base #######
#########################################

#####################################
#################    1    ###########
#####################################

##Variable 1 : PolNum : numéro de la police d'assurance
#PolNum = db1$PolNum
summary(PolNum)


#####################################
#################    2    ###########
#####################################

##Variable 2 : CalYear : année calendaire de souscription 
#CalYear = db1$CalYear
summary(CalYear)
table(CalYear)


#####################################
#################    3    ###########
#####################################

##Variable 3 : Gender : sexe de l'individu
#Gender = db1$Gender
summary(Gender)
table(Gender)
100*table(Gender)/n_db1

#On va recoder cette variable : 0 = homme, 1 = Femme

a = rep(0,n_db1)


for (i in 1:n_db1){
	if (db1$Gender[i] == "Female"){
		a[i] = 1
	} 
}

Gender = a
db1$Gender = a

######################################
#################    4    ############
######################################

##Variable 4 : Type de véhicule
#Type = db1$Type
summary(Type)
plot(Type)

#F a peu d'effectif : peut-être à grouper avec E par exemple



#######################################
#################    5    #############
#######################################

##Variable 5 : Category : Catégorie du véhicule
#Category = db1$Category
summmary(Category)



########################################
################    6    ###############
########################################

##Variable 6 : Occupation : Employé, Chômeur, Femme au foyer, Auto-entrepreneur ou prof libérale, Retraité
#Occupation = db1$Occupation
summary(Occupation)


########################################
###############    7     ###############
########################################

##Variable 7 : Age : Âge du conducteur
#Age = db1$Age
summary(Age)
hist(Age)

#Eventuellement faire des classes d'âge


########################################
###############    8     ###############
########################################

##Variable 8 : Group1 : Groupe du véhicule
#Group1 = db1$Group1
summary(Group1)
hist(Group1)

#Eventuellement réduire le nombre de groupe


#########################################
###############    9     ################
#########################################

##Variable 9 : Bonus : Bonus Malus
#Bonus= db1$Bonus
summary(Bonus)
hist(Bonus)

#Forte présence d'un malus de 50% : peut-être regrouper, surtout les boni



##########################################
###############   10    ##################
##########################################

##Variable 10 : Poldur : Ancienneté du contrat (en années)
#Poldur = db1$Poldur
summary(Poldur)
hist(Poldur)

#Bcp de nouveaux contrats : refaire des tranches

##########################################
##############   11    ###################
##########################################

##Variable 11 : Value : Valeur du véhicule (en €)
#Value = db1$Value
summary(Value)
hist(Value)

#Regroupement au-delà de 30 000?


##########################################
##############   12    ###################
##########################################

##Variable 12 : Adind : Indicateur d'une garantie dommages (indicatrice 0-1)
#Adind = db1$Adind
summary(Adind)
hist(Adind)

100*table(Adind)/n_db1
#51.2% ont une garantie DO, 48.8% n'en ont pas


##########################################
##############   13    ###################
##########################################

##Variable 13 : SubGroup2 sous-région d'habitation
#SubGroup2=db1$SubGroup2
summary(SubGroup2)

#Peu interprétable en tant que tel


##########################################
##############   14    ###################
##########################################

##Variable 14 : Group2 région d'habitation
#Group2 = db1$Group2
summary(Group2)
plot(Group2)

#L,Q et R très dominants : regroupements?


##########################################
##############   15    ###################
##########################################

##Variable 15 : Density : densité de population
#Density = db1$Density
summary(Density)
hist(Density)



##########################################
##############   16    ###################
##########################################

##Variable 16 : Expdays : Exposition en jours (type = interger)

#Exppdays = db1$Exppdays
summary(Exppdays)
hist(Exppdays)


###########################################
##############   17    ####################
###########################################

##Variable 17 : Nb1 Nombre de sinsitres RC matériels
#Nb1 = db1$Nb1
summary(Nb1)
hist(Nb1)

#Eventuelle séparation entre ceux qui n'en ont jamais eu et les autres

###########################################
#############   18    #####################
###########################################

##Variable 18 : Nb2 Nombre de sinsitres RC corporels
#Nb2 = db1$Nb2
summary(Nb2)
hist(Nb2)

#Eventuelle séparation entre ceux qui n'en ont jamais eu et les autres


###########################################
############   19    ######################
###########################################

##Variable 19 : Surv1 : Survenance de sinsitres RC matériels (indicatrice 0-1)
#Surv1 = db1$Surv1
summary(Surv1)
hist(Surv1)

#12.3 % des polices ont connu un sinistre


###########################################
###############   20    ###################
###########################################

##Variable 20 : Surv2 : Survenance de sinsitres RC corporels (indicatrice 0-1)
#Surv2 = db1$Surv2
summary(Surv2)
hist(Surv2)


##################################################################################################
##----------------------------------------------------------------------------------------------##
## 					Fin du retraitement de la base db1                                ##
##----------------------------------------------------------------------------------------------##
##################################################################################################


##########################################################################
##----------------------------------------------------------------------##
##			Premier Modèle : Regression logistique			##
##----------------------------------------------------------------------##
##########################################################################

logit1 = glm(Surv1~Gender+Type+Age+Bonus+Adind+offset(log(Exppdays)), data = db1, family = binomial(link="logit"))
summary(logit1)
AIC(logit1)
BIC(logit1)
Dev1 = logit1$dev

#Procédure stepwise pour choisir les variables explicatives

step(glm(Surv1~Gender+Type+Category+Occupation+Age+Group1+Bonus+Poldur+Value+Adind+Group2+Density+Nb1+Nb2+offset(log(Exppdays)), data = db1, family = binomial(link="logit")))

#Prédiction
pred_logit1 = predict(logit1,newdata = db1, type ="response")
summary(pred_logit1)
hist(pred_logit1)


#Endogénéité de l'exposition
with(db1,table(Surv1,Exppdays))
cor(Surv1,Expddays)


#Tests sur les résidus ?

ResPearson_logit1 = residuals(logit1,type="response")
plot(ResPearson_logit1)
hist(ResPearson_logit1)

ResDeviance_logit1 = residuals(logit1, type = "deviance")
plot(ResDeviance_logit1)
hist(ResDeviance_logit1)

#Tester la non linéarité par des splines sur l'âge, ou le bonus...
#Reg locale

#Test de non linéarité de Fisher


#Test du modèle -> ROC,....

#Lissages multivariés
library(mgcv)

gam1 = gam(Surv1~s(Age,Bonus)+offset(log(Exppdays))+Gender,data= db1,family=binomial)
summary(gam1)

##########################################################################
##----------------------------------------------------------------------##
##			Deuxième Modèle : Arbres					##
##----------------------------------------------------------------------##
##########################################################################



##Améliorartion 1 -> Bagging

##Amélioriation 2 -> Random Forest

##Tracé des partial response plots

##Amélioration 3 -> Boosting