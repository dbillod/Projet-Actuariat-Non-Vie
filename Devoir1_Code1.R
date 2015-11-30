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

##Pour sélectionner les variables : test d'indépendance du khi-deux  entre Surv1 et d'autres variables

#Construisons un tableau avec le nom des variables, puis la p-value associé au tst d'indépendance du khi2
#On met dans le tableau des pval des 2 pour mieux voir une éventuelle erreur

name = names(db1)
name = name[-which(name == "Nb1" | name == "Nb2" | name=="Surv1"| name == "Surv2" )] #| name=="PolNum" |name=="CalYear")]
test_indep  = data.frame(nom_var = name, pval = rep(2,length(name)), pourcent5 = rep(2,length(name)))#,errmessage = rep(0,length(name)))

#taillewarnings = length(warnings())
for (i in 1:length(name)){
	var_act = test_indep[i,1]
	table_act = table(db1[,var_act],Surv1)
	chisqtest_act = chisq.test(table_act)
	
	test_indep[i,2] = chisqtest_act$p.value
	#if (length(warnings())> taillewarnings){
	#	taillewarnings = taillewarnings +1
	#	test_indep[i,4] = 1 
	#}

}
test_indep[,3] = test_indep[,2]>0.05
test_indep
#Prob avec : Value -> Value_class : on rejette l'indépendance
#On ne considère plus Adind

if (FALSE){
chisq.test(table(Gender,Surv1))
chisq.test(table(Type,Surv1))
chisq.test(table(Category,Surv1))
chisq.test(table(Occupation,Surv1))
chisq.test(table(Age,Surv1))
chisq.test(table(Group1,Surv1))
chisq.test(table(Bonus,Surv1))
chisq.test(table(Poldur,Surv1))
chisq.test(table(Value,Surv1))
chisq.test(table(Adind,Surv1))
chisq.test(table(SubGroup2,Surv1))
chisq.test(table(Group2,Surv1))
chisq.test(table(Density,Surv1))
chisq.test(table(Exppdays,Surv1))
}


#Procédure stepwise pour choisir les variables explicatives
#On ne garde pas les variables de groupe


##---------------------------------##
##	Selection stepwise	     ##
##---------------------------------##

###### !! Pb : compte log(Exppdays) ou Exppdays ? !! ######
###### -> Il semble qu'il faille prendre le log, mais en ayant recodé la variable pour avoir E dans [0,1]
###### Semble changer le fait de prendre adind (log) ou Category (pas log) 

#================#
# 	AIC	     #
#================#
step1=step(glm(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density
+offset(log(Exppdays)), 
data = db1, family = binomial(link="logit")))
#Cette sélection avec l'AIC n'enlève rien

#================#
#	BIC	     #
#================#

library(Rcmdr)

step2 = stepwise(glm(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2
+offset(log(Exppdays)), 
data = db1, family = binomial(link="logit")))

summary(step2)

#On enlève simplement Category et Value

#Les sorties suggèrent certaines fusions dans les modalités de Group1 et Group2 

##Group1 : 1 = 2 = 3 = 4

Group1_2 = Group1
indiceGroup1_2 = which(Group1 == 1 | Group1 == 2 | Group1 == 3 | Group1 == 4)
Group1_2[indiceGroup1_2] = 1

##Group2 : L = S = T = U
Group2_2 = Group2
indiceGroup2_2 = which(Group2 == "L" | Group2 == "S" | Group2 == "T" | Group2 == "U" )
Group2_2[indiceGroup2_2] = "L"

#--------------------#
## -> Nouveau modèle #
#--------------------#


step3 = stepwise(glm(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1_2)+Group2_2
+offset(log(Exppdays)), 
data = db1, family = binomial(link="logit")))

summary(step3)

#On enlève Category et Value


##-------------------------------##
## On retient un certain modèle  ##
##-------------------------------##

modele1 = step3

#Prédiction
pred_logit1 = predict(logit1,newdata = db1, type ="response")
summary(pred_logit1)
hist(pred_logit1)

pred_step3 = predict(step3, newdata = db1, type = 'response')


#Endogénéité de l'exposition
with(db1,table(Surv1,Exppdays))
cor(Surv1,Exppdays)


#Tests sur les résidus ?

ResPearson_logit1 = residuals(logit1,type="response")
plot(ResPearson_logit1)
hist(ResPearson_logit1)

ResDeviance_logit1 = residuals(logit1, type = "deviance")
plot(ResDeviance_logit1)
hist(ResDeviance_logit1)
#permet de voir s'il n'y a pas d'individus qui contribuent plus que d'autres à la déviance du modèle

#Tester la non linéarité par des splines sur l'âge, ou le bonus...
#Reg locale

#Test de non linéarité de Fisher
library(lmtest)

resettest(modele1,type="fitted")
resettest(modele1,type="regressor")
resettest(modele1,type="princomp")
##Les 3 tests rejettent l'hyp nulle : influence des puissances
## ATTENTION : les variables factors sont enlevées

#============================#
# Test du modèle -> ROC,.... #
#============================#
library (hmeasure)

s1 = predict(modele1,type='response')
library(ROCR)
predict1 = prediction(s1,Surv1)	
plot(performance(predict1,"tpr","fpr"))
abline(c(0,1))

HMeasure(Surv1,s1)$metrics[,1:5]

############# Très long, et fait souvent planter R ############
library(pROC)
roc = plot.roc(Surv1,s1,main="",percent= TRUE, ci=TRUE)
roc.se = ci.se(roc,specificities =seq(0,5,1))
plot(roc.se,type="shape",col="light blue")
###############################################################

#Lissages multivariés
library(mgcv)

gam1 = gam(Surv1~
s(Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1_2)+Group2_2)
+offset(log(Exppdays))
,data= db1,family=binomial)
summary(gam1)

##########################################################################
##----------------------------------------------------------------------##
##			Deuxième Modèle : Arbres					##
##----------------------------------------------------------------------##
##########################################################################

library(rpart)
library(rpart.plot)

## Prendre en compte ou pas l'exposition dans les arbres?
arbre1 = rpart(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2
, data = db1)
plotcp(arbre1)
prp(arbre1,type=2,extra=1)
arbre1_resp = predict(arbre1)


##Optimisation à faire sur cp
arb2 = rpart(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2
, data = db1, cp = 4e-3)
prp(arb2,type=2,extra=1)
library(rattle)
fancyRpartPlot(arb2, sub="")

plotcp(arb2)
arb2_resp = predict(arb2)

#http://scg.sdsu.edu/ctrees_r/
arb2$cptable
rel_err = arb2$cptable[,4]
which(rel_err == min(rel_err))
cpopt = arb2$cptable[which(rel_err==min(rel_err)),1]
cpopt

arb3 = rpart(Surv1~Age+Gender+ Bonus + Adind, data = db1, minsplit = 5)
fancyRpartPlot(arb3, sub="")
arb3_resp = predict(arb3)

##############################
##Améliorartion 1 -> Bagging##
##############################
library(ipred)
bag1 = bagging(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2
,data = db1,coob=TRUE, nbagg = 25)
bag1
summary(bag1)
fancyRpartPlot(bag1, sub="")
prp(bag1,type=2,extra=1)
bag1_resp = predict(bag1)
summary(bag1_resp)
hist(bag1_resp)

####################################
##Amélioriation 2 -> Random Forest##
####################################
library(randomForest)

rf1 = randomForest(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2
,data  = db1)


rf2 = randomForest(Surv1~
Gender+Type+Occupation+Age+Bonus+Poldur+Density+as.factor(Group1)+Group2
,data  = db1)



##Tracé des partial response plots


##############################
##Amélioration 3 -> Boosting##
##############################

v = 0.05

library(freeknotsplines)
residus_boosting = Surv1 - mean(Surv1)

YP = c()
for (k in 1:100){
	fit_boosting = rpart(residus_boosting ~ Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2)
	residus_boosting = Surv1 - v*predict(fit_boosting)
	YP = cbind(YP, v*predict(fit_boosting))
}

pred_boosting = apply(YP,1,sum)

#PB : prédictions >1 !!!





modele2 = bag1

##Tests du 2eme modèle : Courbes ROC

s2 = predict(modele2)
library(ROCR)
predict2 = prediction(s2,Surv1)	
plot(performance(predict2,"tpr","fpr"))
abline(c(0,1))

HMeasure(Surv1,s2)$metrics[,1:5]