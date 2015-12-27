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
ind_a  = which(Gender == "Female")
a[ind_a] = 1


#for (i in 1:n_db1){
#	if (db1$Gender[i] == "Female"){
#		a[i] = 1
#	} 
#}

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

Group1 = as.factor(Group1)

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


Value_class = rep(0,n_db1)
medianValue = median(Value)
Q1 = summary(Value)[[2]]
Q3 = summary(Value)[[5]]
for ( i in 1: n_db1){
	if (Value[i]<= medianValue){
		if (Value[i]>Q1){
			Value_class[i] = 1
		}else{
			Value_class[i] = 0
		}

	}else{
		if(Value[i]>Q3){
			Value_class[i] = 3
		}else{
			Value_class[i] = 2
		}
	}
}

##On va incorporer cette variable dans la base
db1$Value_class = Value_class
attach(db1)

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

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# On va consdérer l'exposition comme fraction d'année  #
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
Exppdays = Exppdays/365
db1$Exppdays = Exppdays
attach(db1)

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



# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #
#    Séparation de la base     #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! #

#On sépare entre base test et apprentissage
ind = sample(1:n_db1, floor(n_db1*0.65))
db1a = db1[ind,]
db1t = db1[-ind,]
###########################

## Séparation variable 

Nb1a = Nb1[ind]
Nb1t = Nb1[-ind]




#============================#
#  Elimination par Chi 2     #
#============================#

name = names(db1)
name = name[-which(name == "Nb1" | name == "Nb2" | name=="Surv1"| name == "Surv2" )] #| name=="PolNum" |name=="CalYear")]
test_indep  = data.frame(nom_var = name, pval = rep(2,length(name)), pourcent5 = rep(2,length(name)))#,errmessage = rep(0,length(name)))

#taillewarnings = length(warnings())
for (i in 1:length(name)){
	var_act = test_indep[i,1]
	table_act = table(db1a[,var_act],Nb1[ind])
	chisqtest_act = chisq.test(table_act)
	
	test_indep[i,2] = chisqtest_act$p.value
	#if (length(warnings())> taillewarnings){
	#	taillewarnings = taillewarnings +1
	#	test_indep[i,4] = 1 
	#}

}
test_indep[,3] = test_indep[,2]>0.05
test_indep

#######
## -> On retire Value et Adind. ATTENTION : il y a presque tjs un effectif < 5 pour un nombre de sinistres supérieurs à 7
#######


##Idées modèles : 1) Reg de Poisson
#				utilisation de splines
#				effets marginaux moyens
#				changer fcts de liens, fct de variance	
#			2) Reg bin neg
#			   Quasi Poisson, Géométrique
#			   Modèle à inflation de zéros
#				Tester la sur-dispersion, graphiquement	
#				faire des arbes avec une seule variable pour la rendre catégorielle




####
##	-> Packages à charger
####

library (hmeasure)
library (ggplot2)
library(splines)
library(Rcmdr)
library(MASS)
library(AER)
library(pscl)
library(gamlss)
library(rpart)
library(rpart.plot)
library(rattle)