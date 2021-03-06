###################################
## On charge les donn�es ##########
###################################
rm(list=ls())
download.file(url="http://freakonometrics.free.fr/base_ensae_1.RData",destfile="base.RData")
load("base.RData")

base_ensae_1 = read.csv2("C:/Users/David/Documents/ENSAE/3A/Actuariat Non-Vie/base_ensae_1.csv")
base_ensae_1 = base_ensae_1[,-1]

#si besoin d'�crire : 
#regarder le wd : getwd()
#�crire : write.csv2(pricing, file='blabla.csv')

pricing <- read.csv2("http://freakonometrics.free.fr/pricing.csv")
dim(pricing)
##La base a 36311 lignes et 15 colonnes

pricing = read.csv2("C:/Users/David/Documents/ENSAE/3A/Actuariat Non-Vie/pricing.csv")
pricing = pricing[,-1]

## La base s'appelle base_ensae_1

summary(base_ensae_1)
dim(base_ensae_1)
# La base a 100 021 lignes, pour 20 colonnes. La 1�re colonne est le num�ro de police

#Copions la pour avoir une base de travail
db1 = base_ensae_1
n_db1 = dim(base_ensae_1)[1]
attach(db1)


##Id�e : 1) fitter un logit, puis tester sa qualit� -> d�viance, r�sidus de Pearson
## 2) Utiliser des splines ou une base polynomiale : illustrer le choix du nombre 
##de splines ou de degr� optimal


#########################################
## Commen�ons par recoder la base #######
#########################################

#####################################
#################    1    ###########
#####################################

##Variable 1 : PolNum : num�ro de la police d'assurance
#PolNum = db1$PolNum
summary(PolNum)


#####################################
#################    2    ###########
#####################################

##Variable 2 : CalYear : ann�e calendaire de souscription 
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

##Variable 4 : Type de v�hicule
#Type = db1$Type
summary(Type)
plot(Type)

#F a peu d'effectif : peut-�tre � grouper avec E par exemple



#######################################
#################    5    #############
#######################################

##Variable 5 : Category : Cat�gorie du v�hicule
#Category = db1$Category
summmary(Category)



########################################
################    6    ###############
########################################

##Variable 6 : Occupation : Employ�, Ch�meur, Femme au foyer, Auto-entrepreneur ou prof lib�rale, Retrait�
#Occupation = db1$Occupation
summary(Occupation)


########################################
###############    7     ###############
########################################

##Variable 7 : Age : �ge du conducteur
#Age = db1$Age
summary(Age)
hist(Age)

#Eventuellement faire des classes d'�ge


########################################
###############    8     ###############
########################################

##Variable 8 : Group1 : Groupe du v�hicule
#Group1 = db1$Group1
summary(Group1)
hist(Group1)

Group1 = as.factor(Group1)

#Eventuellement r�duire le nombre de groupe


#########################################
###############    9     ################
#########################################

##Variable 9 : Bonus : Bonus Malus
#Bonus= db1$Bonus
summary(Bonus)
hist(Bonus)

#Forte pr�sence d'un malus de 50% : peut-�tre regrouper, surtout les boni



##########################################
###############   10    ##################
##########################################

##Variable 10 : Poldur : Anciennet� du contrat (en ann�es)
#Poldur = db1$Poldur
summary(Poldur)
hist(Poldur)

#Bcp de nouveaux contrats : refaire des tranches

##########################################
##############   11    ###################
##########################################

##Variable 11 : Value : Valeur du v�hicule (en �)
#Value = db1$Value
summary(Value)
hist(Value)

#Regroupement au-del� de 30 000?


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

##Variable 13 : SubGroup2 sous-r�gion d'habitation
#SubGroup2=db1$SubGroup2
summary(SubGroup2)

#Peu interpr�table en tant que tel


##########################################
##############   14    ###################
##########################################

##Variable 14 : Group2 r�gion d'habitation
#Group2 = db1$Group2
summary(Group2)
plot(Group2)

#L,Q et R tr�s dominants : regroupements?


##########################################
##############   15    ###################
##########################################

##Variable 15 : Density : densit� de population
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
# On va consd�rer l'exposition comme fraction d'ann�e  #
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
Exppdays = Exppdays/365


###########################################
##############   17    ####################
###########################################

##Variable 17 : Nb1 Nombre de sinsitres RC mat�riels
#Nb1 = db1$Nb1
summary(Nb1)
hist(Nb1)

#Eventuelle s�paration entre ceux qui n'en ont jamais eu et les autres

###########################################
#############   18    #####################
###########################################

##Variable 18 : Nb2 Nombre de sinsitres RC corporels
#Nb2 = db1$Nb2
summary(Nb2)
hist(Nb2)

#Eventuelle s�paration entre ceux qui n'en ont jamais eu et les autres


###########################################
############   19    ######################
###########################################

##Variable 19 : Surv1 : Survenance de sinsitres RC mat�riels (indicatrice 0-1)
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
##			Premier Mod�le : Regression logistique			##
##----------------------------------------------------------------------##
##########################################################################

logit1 = glm(Surv1~Gender+Type+Age+Bonus+Adind+offset(log(Exppdays)), data = db1, family = binomial(link="logit"))
summary(logit1)
AIC(logit1)
BIC(logit1)
Dev1 = logit1$dev

##Pour s�lectionner les variables : test d'ind�pendance du khi-deux  entre Surv1 et d'autres variables

#Construisons un tableau avec le nom des variables, puis la p-value associ� au tst d'ind�pendance du khi2
#On met dans le tableau des pval des 2 pour mieux voir une �ventuelle erreur

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
#Prob avec : Value -> Value_class : on rejette l'ind�pendance
#On ne consid�re plus Adind

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


#Proc�dure stepwise pour choisir les variables explicatives
#On ne garde pas les variables de groupe


##---------------------------------##
##	Selection stepwise	     ##
##---------------------------------##

###### !! Pb : compte log(Exppdays) ou Exppdays ? !! ######
###### -> Il semble qu'il faille prendre le log, mais en ayant recod� la variable pour avoir E dans [0,1]
###### Semble changer le fait de prendre adind (log) ou Category (pas log) 

#================#
# 	AIC	     #
#================#
step1=step(glm(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density
+offset(log(Exppdays)), 
data = db1, family = binomial(link="logit")))
#Cette s�lection avec l'AIC n'enl�ve rien

#================#
#	BIC	     #
#================#

library(Rcmdr)

step2 = stepwise(glm(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2
+offset(log(Exppdays)), 
data = db1, family = binomial(link="logit")))

summary(step2)

#On enl�ve simplement Category et Value

#Les sorties sugg�rent certaines fusions dans les modalit�s de Group1 et Group2 

##Group1 : 1 = 2 = 3 = 4

Group1_2 = Group1
indiceGroup1_2 = which(Group1 == 1 | Group1 == 2 | Group1 == 3 | Group1 == 4)
Group1_2[indiceGroup1_2] = 1

##Group2 : L = S = T = U
Group2_2 = Group2
indiceGroup2_2 = which(Group2 == "L" | Group2 == "S" | Group2 == "T" | Group2 == "U" )
Group2_2[indiceGroup2_2] = "L"

#--------------------#
## -> Nouveau mod�le #
#--------------------#


step3 = stepwise(glm(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1_2)+Group2_2
+offset(log(Exppdays)), 
data = db1, family = binomial(link="logit")))

summary(step3)

#On enl�ve Category et Value


##-------------------------------##
## On retient un certain mod�le  ##
##-------------------------------##

modele1 = step3

#Pr�diction
pred_logit1 = predict(logit1,newdata = db1, type ="response")
summary(pred_logit1)
hist(pred_logit1)

pred_step3 = predict(step3, newdata = db1, type = 'response')


#Endog�n�it� de l'exposition
with(db1,table(Surv1,Exppdays))
cor(Surv1,Exppdays)


#Tests sur les r�sidus ?

ResPearson_logit1 = residuals(logit1,type="response")
plot(ResPearson_logit1)
hist(ResPearson_logit1)

ResDeviance_logit1 = residuals(logit1, type = "deviance")
plot(ResDeviance_logit1)
hist(ResDeviance_logit1)
#permet de voir s'il n'y a pas d'individus qui contribuent plus que d'autres � la d�viance du mod�le

#Tester la non lin�arit� par des splines sur l'�ge, ou le bonus...
#Reg locale

#Test de non lin�arit� de Fisher
library(lmtest)

resettest(modele1,type="fitted")
resettest(modele1,type="regressor")
resettest(modele1,type="princomp")
##Les 3 tests rejettent l'hyp nulle : influence des puissances
## ATTENTION : les variables factors sont enlev�es

#============================#
# Test du mod�le -> ROC,.... #
#============================#
library (hmeasure)
library (ggplot2)


s1 = predict(modele1,type='response')
library(ROCR)
predict1 = prediction(s1,Surv1)	
#qplot(performance(predict1,"tpr","fpr"))
plot(performance(predict1,"tpr","fpr"))
abline(c(0,1))

HMeasure(Surv1,s1)$metrics[,1:5]

############# Tr�s long, et fait souvent planter R ############
library(pROC)
roc = plot.roc(Surv1,s1,main="",percent= TRUE, ci=TRUE)
roc.se = ci.se(roc,specificities =seq(0,5,1))
plot(roc.se,type="shape",col="light blue")
###############################################################

#Lissages multivari�s
library(mgcv)

gam1 = gam(Surv1~
s(Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1_2)+Group2_2)
+offset(log(Exppdays))
,data= db1,family=binomial)
summary(gam1)

##########################################################################
##----------------------------------------------------------------------##
##			Deuxi�me Mod�le : Arbres					##
##----------------------------------------------------------------------##
##########################################################################




###########################
## S�paration de la base ##
###########################
##
#On s�pare entre base test et apprentissage
ind = sample(1:n_db1, floor(n_db1/1000))
db1a = db1[ind,]
db1t = db1[-ind,]
###########################



library(rpart)
library(rpart.plot)

## Prendre en compte ou pas l'exposition dans les arbres?
arbre1 = rpart(Surv1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2
, data = db1)
plotcp(arbre1)
prp(arbre1,type=2,extra=1)
arbre1_resp = predict(arbre1)


##Optimisation � faire sur cp
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




predict_arb = prediction(arb2_resp,Surv1)
plot(performance(predict_arb,"tpr","fpr"))
abline(c(0,1))

library(hmeasure)
HMeasure(Surv1,arb2_resp)$metrics[,1:5]
## AUC = 0.689





##############################
##Am�liorartion 1 -> Bagging##
##############################
library(ipred)
bag1 = bagging(as.factor(Surv1)~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2
,data = db1,coob=TRUE, nbagg = 75)
bag1
bag1_resp = predict(bag1, type = 'prob')

predict_bagging = prediction(bag1_resp[,2],Surv1)
plot(performance(predict_bagging,"tpr","fpr"))
abline(c(0,1))

library(hmeasure)
HMeasure(Surv1,bag1_resp[,2])$metrics[,1:5]
## AUC : 0.676 pour nbagg = 25
##	   0.680 pour nbagg = 50
##	   0.679 pour nbagg = 75
##       0.677 pour nbagg = 100



## !! Nveaux r�sultats avec as.factor et type = 'prob'
## AUC : 0.626 pour nbagg = 10
## 	   0.702 pour nbagg = 50
##       0.716 pour nbagg = 75


summary(bag1_resp)
hist(bag1_resp)

summary(bag1)
fancyRpartPlot(bag1, sub="")
prp(bag1,type=2,extra=1)

#entre 10 et 100
data_bagg = data.frame( n_b = seq(10,100,by=10),AUC_bag=seq(10))
for (  i in 1:10){
	nbagg_loop =  data_bagg[i,1]

	bag_loop = bagging(Surv1~
	Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2
	,data = db1,coob=TRUE, nbagg = nbagg_loop)

	bag_resp = predict(bag_loop)
	data_bagg[i,2] = HMeasure(Surv1,bag_resp)$metrics[,3]

	
}

data_bagg


####################################
##Am�lioriation 2 -> Random Forest##
####################################
library(randomForest)


rf1 = randomForest(as.factor(Surv1)~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+Group1+Group2
,data  = db1
, ntree = 50)
, mtry = 15)
rf1


#Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+Group1+Group2


rf2 = randomForest(Surv1~
Gender+Type+Occupation+Age+Bonus+Poldur+Density+as.factor(Group1)+Group2
,data  = db1)

varImpPlot(rf1, main="")
importance(rf1)

library(ROCR)

rf1_resp = as.numeric(predict(rf1, newdata = db1, type = 'prob'))
#Surv1t = Surv1[-ind]
predict_rf = prediction(rf1_resp,Surv1)
plot(performance(predict_rf,"tpr","fpr"))
abline(c(0,1))

library(hmeasure)
HMeasure(Surv1,rf1_resp)$metrics[,1:5]



##Trac� des partial response plots


##############################
##Am�lioration 3 -> Boosting##
##############################

library(freeknotsplines)

v = 0.05
residus_boosting = Surv1 - mean(Surv1)

YP = c()
for (k in 1:1000){
	fit_boosting = rpart(residus_boosting ~ Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2)
	residus_boosting = residus_boosting - v*predict(fit_boosting)
	YP = cbind(YP, v*predict(fit_boosting))
}

pred_boosting = apply(YP,1,sum)
#pred_boosting
max(pred_boosting)
min(pred_boosting)

predict_boosting = prediction(pred_boosting,Surv1)
plot(performance(predict_boosting,"tpr","fpr"))
abline(c(0,1))

library(hmeasure)
HMeasure(Surv1,pred_boosting)$metrics[,1:5]

#PB : pr�dictions >1 !!!
##Boosting sans cp, avec 100 it� : AUC = 0.677


######################################
## Boosting avec splines lin�raires ##
######################################

#library (freeknotsplines)
#res_boost = Surv1 - mean(Surv1)
#for (k in 1:100){
#	noeuds = freelsgen(Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+as.factor(Group1)+Group2
#	, res_boost, degree = 1 , numknot = 2)
#	fit_boost = lm(res_boost~bs())
#}	


modele2 = bag1

##Tests du 2eme mod�le : Courbes ROC

s2 = predict(modele2)
library(ROCR)
predict2 = prediction(s2,Surv1)	
plot(performance(predict2,"tpr","fpr"))
abline(c(0,1))

HMeasure(Surv1,s2)$metrics[,1:5]













#Test �ge

Age2 = Age^2

reg_car = glm(Surv1~(Age+Age2), data = db1, family = binomial)
pred_car = predict(reg_car, type = 'response')
plot(pred_car~Age,db1)


reg_age1 = glm(Surv1~(Age), data = db1, family = binomial)
pred_age1 = predict(reg_age1, type = 'response')
plot(pred_age1~Age,db1)
qplot(pred_age1,Age, geom = c("point", "smooth"))+ geom_line(size=1, color = 'blue')


library(splines)
reg_age = glm(Surv1~bs(Age), data = db1, family = binomial)
pred_age = predict(reg_age, type = 'response')
plot(pred_age~Age,db1)

smoothingSpline = smooth.spline(Age, Surv1, spar=0.35)
plot(Age,Surv1)
lines(smoothingSpline)


AGE = c(17,25 ,35,50,65,80,100)
reg_cut = glm(Surv1~cut(Age,breaks =AGE), data = db1, family = binomial)
pred_cut = predict(reg_cut,type = 'response')

plot(pred_cut~Age,db1)


reg_factor = glm(Surv1~as.factor(Age), data = db1, family = binomial)
pred_factor = predict(reg_factor,type = 'response')
plot(pred_factor~Age,db1)

mat_age = matrix(1, nrow = n_db1, ncol = 2)
mat_age[,1] = pred_factor
mat_age[,2] = pred_age1
#matplot(Age,mat_age)


data_age = as.data.frame(mat_age)
data_age[,3] = Age
ggplot(data_age,aes(x = Age, y=pred_factor,colour = "pred_factor"))+
	geom_line()+
	geom_line(data = data_age , aes(x=Age,y=pred_age,color= "pred_age1"))+
	ylab("Pr�diction")+
	scale_colour_manual("",breaks = c("pred_factor","pred_age1"),values = c("black", "red"))


mat_age = matrix(1, nrow = n_db1, ncol = 2)
mat_age[,1] = pred_factor
mat_age[,2] = pred_age

data_age = as.data.frame(mat_age)
data_age[,3] = Age
ggplot(data_age,aes(x = Age, y=pred_factor,colour = "pred_factor"))+
	geom_line()+
	geom_line(data = data_age , aes(x=Age,y=pred_age,color= "pred_age"))+
	ylab("Pr�diction")+
	scale_colour_manual("",breaks = c("pred_factor","pred_age"),values = c("black", "red"))

# #             # #
## ESSAI SPLINES ##
# #             # #

ess_age_sp = glm(Surv1~as.factor(Age), data = db1, family = binomial)
(


############################################
## -> CCL : pas de regroupement ############
############################################



#Test densit�


Density_arrond<-round(Density,0)
#table(Density_arrond)
#hist(Density_arrond)

Density_dix<-ceiling(Density_arrond/10) * 10
#table(Density_dix)
#hist(Density_dix)


reg_dens = glm(Surv1~(Density_dix), data = db1, family = binomial)
pred_dens = predict(reg_dens, type ='response')
plot(pred_dens~Density,db1)

reg_dens_sp = glm(Surv1~bs(Density_dix), data = db1, family = binomial)
pred_dens_sp = predict(reg_dens_sp, type ='response')
plot(pred_dens_sp~Density,db1)

reg_dens_fac = glm(Surv1~as.factor(Density), data = db1, family = binomial)
pred_dens_fac = predict(reg_dens_fac, type ='response')
plot(pred_dens_fac~Density,db1)

reg_dens_fac = glm(Surv1~as.factor(Density_dix), data = db1, family = binomial)
pred_dens_fac = predict(reg_dens_fac, type ='response')
plot(pred_dens_fac~Density,db1)


mat_dens = matrix(1, nrow = n_db1, ncol = 2)
mat_dens[,1] = pred_dens_fac
mat_dens[,2] = pred_dens

#matplot(Density_dix,mat_dens)


data_dens = as.data.frame(mat_dens)
data_dens[,3] = Density_dix
ggplot(data_dens,aes(x = Density_dix, y=pred_dens_fac,colour = "A pred_dens_fac"))+
	geom_line()+
	geom_line(data = data_dens, aes(x=Density_dix,y=pred_dens,color= "B pred_dens"))+
	ylab("Pr�diction")+
	scale_colour_manual("",breaks = c("A pred_dens_fac","B pred_dens"),values = c("black", "red"))


mat_dens = matrix(1, nrow = n_db1, ncol = 2)
mat_dens[,1] = pred_dens_fac
mat_dens[,2] = pred_dens_sp

data_dens = as.data.frame(mat_dens)
data_dens[,3] = Density_dix
ggplot(data_dens,aes(x = Density_dix, y=pred_dens_fac,colour = "pred_dens_fac"))+
	geom_line()+
	geom_line(data = data_dens , aes(x=Density_dix,y=pred_dens_sp,color= "pred_dens_sp"))+
	ylab("Pr�diction")+
	scale_colour_manual("",breaks = c("pred_dens_fac","pred_dens_sp"),values = c("black", "red"))









#################################
## -> CCL : Mettre des splines ##
#################################



#Test bonus

reg_bonus  = glm(Surv1~(Bonus), data = db1, family = binomial)
pred_bonus = predict(reg_bonus, type ='response')
plot(pred_bonus~Bonus,db1)

reg_bonus_sp = glm(Surv1~bs(Bonus), data = db1, family = binomial)
pred_bonus_sp = predict(reg_bonus_sp, type ='response')
plot(pred_bonus_sp~Bonus,db1)

reg_bonus_fac = glm(Surv1~as.factor(Bonus), data = db1, family = binomial)
pred_bonus_fac = predict(reg_bonus_fac, type ='response')
plot(pred_bonus_fac~Bonus,db1)


mat_bonus = matrix(1, nrow = n_db1, ncol = 2)
mat_bonus[,1] = pred_bonus_fac
mat_bonus[,2] = pred_bonus_sp

matplot(Bonus,mat_bonus)

####################################
## -> CCL : Mettre des splines #####
####################################


#Test Anciennet�

reg_dur = glm(Surv1~(Poldur), data = db1, family = binomial)
pred_dur = predict(reg_dur, type ='response')
plot(pred_dur~Poldur,db1)

reg_dur_sp = glm(Surv1~bs(Poldur), data = db1, family = binomial)
pred_dur_sp = predict(reg_dur_sp, type ='response')
plot(pred_dur_sp~Poldur,db1)

reg_dur_fac = glm(Surv1~as.factor(Poldur), data = db1, family = binomial)
pred_dur_fac = predict(reg_dur_fac, type ='response')
plot(pred_dur_fac~Poldur,db1)

mat_dur = matrix(1, nrow = n_db1, ncol = 2)
mat_dur[,1] = pred_dur_fac
mat_dur[,2] = pred_dur_sp

matplot(Poldur,mat_dur)

###################################
## -> Mettre des splines ##########
###################################




####BONUS
Bonus_positif<-rep(0,n_db1)

for (i in 1:n_db1){
if (Bonus[i]>=0){Bonus_positif[i]<-Bonus[i]}
}

Bonus_negatif<-rep(0,n_db1)
for (i in 1:n_db1){
if (Bonus[i]<0){Bonus_negatif[i]<-Bonus[i]}
}

#Sans discretisation

step = glm(Surv1~
Gender+Type+Occupation+Age+Bonus_positif+Bonus_negatif+Poldur+Density+as.factor(Group1_2)+Group2_2
+offset(log(Exppdays)), 
data = db1, family = binomial(link="logit"))

summary(step)

#test �galit� bonus
test_bonus  = (step$coeff[[13]]-step$coeff[[14]])/sqrt( vcov(step)[13,13]+ vcov(step)[14,14] - 2*vcov(step)[13,14]  )
test_bonus
##-> Dif�rence significative

vcov(step)

#------#
#Bonus #
#------#

M_max = 20

res_bonus_sp_opt1 = data.frame(M_bonus_sp_opt1 = c(1:M_max),MSE_bonus_sp_opt1 = c(1:M_max))
for ( i in 1 : M_max){
	reg_bonus_sp_opt1 = glm(Surv1~bs(Bonus, df = i, degree = 1), data = db1, family = binomial)
	#pred_bonus_sp_opt1 = predict(reg_bonus_sp_opt1, type ='response')
	#plot(pred_bonus_sp_opt1~Bonus,db1)
	res_bonus_sp_opt1 [i,2] = mean(reg_bonus_sp_opt1$residuals^2)
}
plot(res_bonus_sp_opt1[,1], res_bonus_sp_opt1[,2])


res_bonus_sp_opt2 = data.frame(M_bonus_sp_opt2 = c(1:M_max),MSE_bonus_sp_opt2 = c(1:M_max))
for ( i in 1 : M_max){
	reg_bonus_sp_opt2 = glm(Surv1~bs(Bonus, df = i, degree = 2), data = db1, family = binomial)
	#pred_bonus_sp_opt2 = predict(reg_bonus_sp_opt2, type ='response')
	#plot(pred_bonus_sp_opt2~Bonus,db1)
	res_bonus_sp_opt2 [i,2] = mean(reg_bonus_sp_opt2$residuals^2)
}
plot(res_bonus_sp_opt1[,1], res_bonus_sp_opt2[,2])


res_bonus_sp_opt3 = data.frame(M_bonus_sp_opt3 = c(1:M_max),MSE_bonus_sp_opt3 = c(1:M_max))
for ( i in 1 : M_max){
	reg_bonus_sp_opt3 = glm(Surv1~bs(Bonus, df = i, degree = 3), data = db1, family = binomial)
	#pred_bonus_sp_opt3 = predict(reg_bonus_sp_opt3, type ='response')
	#plot(pred_bonus_sp_opt3~Bonus,db1)
	res_bonus_sp_opt3 [i,2] = mean(reg_bonus_sp_opt3$residuals^2)
}
plot(res_bonus_sp_opt3[,1], res_bonus_sp_opt3[,2])

liste_min = c(min(res_bonus_sp_opt1[,2]), min(res_bonus_sp_opt2[,2]),min(res_bonus_sp_opt3[,2]))



#---------#
# Density #
#---------#



M_max = 20

res_dens_sp_opt1 = data.frame(M_dens_sp_opt1 = c(1:M_max),MSE_dens_sp_opt1 = c(1:M_max))
for ( i in 1 : M_max){
	reg_dens_sp_opt1 = glm(Surv1~bs(Density, df = i, degree = 1), data = db1, family = binomial)
	#pred_dens_sp_opt1 = predict(reg_dens_sp_opt1, type ='response')
	#plot(pred_dens_sp_opt1~Density,db1)
	res_dens_sp_opt1 [i,2] = mean(reg_dens_sp_opt1$residuals^2)
}
plot(res_dens_sp_opt1[,1], res_dens_sp_opt1[,2])


res_dens_sp_opt2 = data.frame(M_dens_sp_opt2 = c(1:M_max),MSE_dens_sp_opt2 = c(1:M_max))
for ( i in 1 : M_max){
	reg_dens_sp_opt2 = glm(Surv1~bs(Density, df = i, degree = 2), data = db1, family = binomial)
	#pred_dens_sp_opt2 = predict(reg_dens_sp_opt2, type ='response')
	#plot(pred_dens_sp_opt2~Density,db1)
	res_dens_sp_opt2 [i,2] = mean(reg_dens_sp_opt2$residuals^2)
}
plot(res_dens_sp_opt2[,1], res_dens_sp_opt2[,2])


res_dens_sp_opt3 = data.frame(M_dens_sp_opt3 = c(1:M_max),MSE_dens_sp_opt3 = c(1:M_max))
for ( i in 1 : M_max){
	reg_dens_sp_opt3 = glm(Surv1~bs(Density, df = i, degree = 3), data = db1, family = binomial)
	#pred_dens_sp_opt3 = predict(reg_dens_sp_opt3, type ='response')
	#plot(pred_dens_sp_opt3~Density,db1)
	res_dens_sp_opt3 [i,2] = mean(reg_dens_sp_opt3$residuals^2)
}
plot(res_dens_sp_opt3[,1], res_dens_sp_opt3[,2])

liste_min = c( min(res_dens_sp_opt1[,2]), min(res_dens_sp_opt2[,2]), min(res_dens_sp_opt3[,2]) )
liste_min


#---------#
# Poldur  #
#---------#



M_max = 20

res_dur_sp_opt1 = data.frame(M_dur_sp_opt1 = c(1:M_max),MSE_dur_sp_opt1 = c(1:M_max))
for ( i in 1 : M_max){
	reg_dur_sp_opt1 = glm(Surv1~bs(Poldur, df = i, degree = 1), data = db1, family = binomial)
	#pred_dur_sp_opt1 = predict(reg_dur_sp_opt1, type ='response')
	#plot(pred_dur_sp_opt1~Poldur,db1)
	res_dur_sp_opt1 [i,2] = mean(reg_dur_sp_opt1$residuals^2)
	res_dur_sp_opt1 [i,3] = BIC(reg_dur_sp_opt1)
}
plot(res_dur_sp_opt1[,1], res_dur_sp_opt1[,2])


res_dur_sp_opt2 = data.frame(M_dur_sp_opt2 = c(1:M_max),MSE_dur_sp_opt2 = c(1:M_max))
for ( i in 1 : M_max){
	reg_dur_sp_opt2 = glm(Surv1~bs(Poldur, df = i, degree = 2), data = db1, family = binomial)
	#pred_dur_sp_opt2 = predict(reg_dur_sp_opt2, type ='response')
	#plot(pred_dur_sp_opt2~Poldur,db1)
	res_dur_sp_opt2 [i,2] = mean(reg_dur_sp_opt2$residuals^2)
	res_dur_sp_opt2 [i,3] = BIC(reg_dur_sp_opt2)

}
plot(res_dur_sp_opt2[,1], res_dur_sp_opt2[,2])


res_dur_sp_opt3 = data.frame(M_dur_sp_opt3 = c(1:M_max),MSE_dur_sp_opt3 = c(1:M_max))
for ( i in 1 : M_max){
	reg_dur_sp_opt3 = glm(Surv1~bs(Poldur, df = i, degree = 3), data = db1, family = binomial)
	#pred_dur_sp_opt3 = predict(reg_dur_sp_opt3, type ='response')
	#plot(pred_dur_sp_opt3~Poldur,db1)
	res_dur_sp_opt3 [i,2] = mean(reg_dur_sp_opt3$residuals^2)
	res_dur_sp_opt3 [i,3] = BIC(reg_dur_sp_opt3)

}
plot(res_dur_sp_opt3[,1], res_dur_sp_opt3[,2])

liste_min = c( min(res_dur_sp_opt1[,2]), min(res_dur_sp_opt2[,2]), min(res_dur_sp_opt3[,2]) )
liste_min

min_BIC = c(min(res_dur_sp_opt1[,3]), min(res_dur_sp_opt2[,3]), min(res_dur_sp_opt3[,3]) )
min_BIC


Poldura = Poldur[ind]
Surv1a = Surv1[ind]

fit_ess = fit.search.numknots(Poldura,Surv1a,degree = 1,minknot = 1 , maxknot = 5 )
fit_ess
fit_dur = freelsgen(Poldura, Surv1a,degree = 3, numknot = 1)
summary(fit_dur)