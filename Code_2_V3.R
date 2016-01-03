###################################
	## On charge les données ##########
	###################################
	#rm(list=ls())
	#download.file(url="http://freakonometrics.free.fr/base_ensae_1.RData",destfile="base.RData")
	#load("base.RData")
	
	base_ensae_1 = read.csv2("H:/ENSAE/3A/Actuariat_Non_Vie/base_ensae_1.csv")
	base_ensae_1 = base_ensae_1[,-1]
	
	#si besoin d'écrire : 
	#regarder le wd : getwd()
	#écrire : write.csv2(pricing, file='blabla.csv')
	
	#pricing <- read.csv2("http://freakonometrics.free.fr/pricing.csv")
	#dim(pricing)
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
	
	########################################
	###############    8     ###############
	########################################
	
	##Variable 8 : Group1 : Groupe du véhicule
	
	Group1 = as.factor(Group1)
	
	#Eventuellement réduire le nombre de groupe
	
	##############################################
	##############   Region    ###################
	##############################################

Group2_bis=Group2
indiceGroup2 = which(Group2 == "L" | Group2 == "S" | Group2 == "T" | Group2 == "U"| Group2 == "M" )
Group2_bis[indiceGroup2] = "L"
indiceGroup2_1= which(Group2 == "O" | Group2 == "P" )
Group2_bis[indiceGroup2_1] = "P"

db1$Group2_bis = Group2_bis
	attach(db1)

Group2_ter=Group2
indiceGroup2 = which(Group2 == "L" | Group2 == "S" | Group2 == "T" | Group2 == "U"| Group2 == "M" )
Group2_ter[indiceGroup2] = "L"

db1$Group2_ter = Group2_ter
	attach(db1)

Group2_4=Group2
indiceGroup2 = which(Group2 == "L" | Group2 == "S" | Group2 == "T" | Group2 == "U" )
Group2_4[indiceGroup2] = "L"
#indiceGroup2= which(Group2 == "O" | Group2 == "M" )
#Group2_4[indiceGroup2] = "O"

db1$Group2_4 = Group2_4
	attach(db1)
	##########################################
	##############   11    ###################
	##########################################
	
	##Variable 11 : Value : Valeur du véhicule (en €)

	
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
	##############   16    ###################
	##########################################
	
	##Variable 16 : Expdays : Exposition en jours (type = interger)
	
	
	#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
	# On va consdérer l'exposition comme fraction d'année  #
	#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
	Exppdays = Exppdays/365
	db1$Exppdays = Exppdays
	


      ################################################
	############Discretisation   ###################
	################################################

Age_Tr=rep(1,n_db1)
Age_Tr[which(Age>=30)]=2
Age_Tr[which(Age>=50)]=3
Age_Tr=as.factor(Age_Tr)

Density_Tr=rep(3,n_db1)
Density_Tr[which(Density<152)]=2
Density_Tr[which(Density<84)]=1
Density_Tr=as.factor(Density_Tr)	
	
Bonus_Tr=rep(1,n_db1)
Bonus_Tr[which(Bonus>=-15)]=2
Bonus_Tr=as.factor(Bonus_Tr)


Poldur_Tr = rep(1,n_db1)
Poldur_Tr[which(Poldur>0.5)]=2
Poldur_Tr[which(Poldur>7.5)]=3
Poldur_Tr = as.factor(Poldur_Tr)
	
db1$Age_Tr = Age_Tr
db1$Density_Tr = Density_Tr
db1$Bonus_Tr = Bonus_Tr	
db1$Poldur_Tr = Poldur_Tr
	
	
attach(db1)
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




	##----------------------------------------##
	## 		Modèle de Poisson			##
	##----------------------------------------##
	
	step1 = stepwise(glm(Nb1~
	Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+(Group1)+Group2+Adind
	+offset(log(Exppdays)),
	data = db1a, family = poisson))
	
	summary(step1)
	BIC(step1)

res.step1.P=residuals(step1,type="pearson")
res.step1.D=residuals(step1,type="deviance")

	

step1_4= stepwise(glm(Nb1~
	Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+(Group1)+Group2_4+Adind
	+offset(log(Exppdays)),
	data = db1a, family = poisson))
	
	summary(step1_4)
	BIC(step1_4)

res.step1_4.P=residuals(step1_4,type="pearson")
res.step1_4.D=residuals(step1_4,type="deviance")
	

#vuong(step1,step1_4)
	
	########################################
	## Traitement par arbres  ##############
	########################################
	
	
	#BUT : obtenir des catégories pour certaines variables continues
	

#http://scg.sdsu.edu/ctrees_r/
arb_age = rpart(Nb1~
	Age,
	data = db1a, method = "poisson",
	cp = 3e-3)
	fancyRpartPlot(arb_age)

		
	########################################
	## Traitement par splines ##############
	########################################
	
	
	#=============#
	# 	AGE	  #
	#=============#
	
	
	
	
	reg_age_fac =x glm(Nb1~as.factor(Age), data = db1a, family = poisson)
	pred_age_fac = predict(reg_age_fac,type = 'response', newdata = db1a)
	
	
	reg_age = glm(Nb1~(Age), data = db1a, family = poisson(link='log'))
	pred_age = predict(reg_age, type = 'response', newdata = db1a)
	
	
	reg_age_sp = glm(Nb1~bs(Age, degree = 3, knots = c(30,50)), data = db1a, family = poisson)
	pred_age_sp = predict(reg_age_sp, type = 'response', newdata = db1a)
	
	BIC(reg_age_sp)
	
	##Graphe Catégorielle/Linéaire
	
	mat_age1 = matrix(1, nrow = length(ind), ncol = 2)
	mat_age1[,1] = pred_age_fac
	mat_age1[,2] = pred_age
	
	data_age1 = as.data.frame(mat_age1)
	data_age1[,3] = Age[ind]
	
	
	ggplot(data_age1,aes(x = Age[ind], y=pred_age_fac,colour = "Catégorielle"))+
		geom_line()+
		geom_line(data = data_age1 , aes(x=Age[ind],y=pred_age,color= "Linéaire"))+
		ylab("Prédiction")+
		scale_colour_manual("",breaks = c("Catégorielle","Linéaire", "Spline"),values = c("black", "red", "blue"))+
		ggtitle("Age")+
		xlab("Age")+
		geom_line(data = data_age1 , aes(x=Age[ind],y=pred_age_sp,color= "Spline"))+
		geom_vline(data = data_age1, aes(xintercept = 30), colour = "purple")+
		geom_vline(data = data_age1, aes(xintercept = 50), colour = "purple")
	
	#-> Noeuds : vers 32

	#============#
	#  Density   #
	#============#


###BIEN
arb_ds = rpart(Nb1~
	Density,
	data = db1a, method = "poisson",
	cp = 3e-3)
	fancyRpartPlot(arb_ds, main = "Variable explicative : Densité")
	
arb_ds$cptable
rel_err = arb_ds$cptable[,4]
which(rel_err == min(rel_err))
cpopt = arb_ds$cptable[which(rel_err==min(rel_err)),1]
cpopt	


	reg_dens_fac = glm(Nb1~as.factor(Density), data = db1a, family = poisson)
	pred_dens_fac = predict(reg_dens_fac,type = 'response', newdata = db1a)
	
	
	reg_dens = glm(Nb1~(Density), data = db1a, family = poisson(link='log'))
	pred_dens = predict(reg_dens, type = 'response', newdata = db1a)
	
	
	reg_dens_sp = glm(Nb1~bs(Density, degree = 1, knots = c(35,65)), data = db1a, family = poisson)
	pred_dens_sp = predict(reg_dens_sp, type = 'response', newdata = db1a)
	
	BIC(reg_dens_sp)
	
	##Graphe Catégorielle/Linéaire
	
	mat_dens1 = matrix(1, nrow = length(ind), ncol = 2)
	mat_dens1[,1] = pred_dens_fac
	mat_dens1[,2] = pred_dens
	
	data_dens1 = as.data.frame(mat_dens1)
	data_dens1[,3] = Density[ind]
	
	
	ggplot(data_dens1,aes(x = Density[ind], y=pred_dens_fac,colour = "Catégorielle"))+
		geom_line()+
		geom_line(data = data_dens1 , aes(x=Density[ind],y=pred_dens,color= "Linéaire"))+
		ylab("Prédiction")+
		scale_colour_manual("",breaks = c("Catégorielle","Linéaire"),values = c("black", "red"))+
		ggtitle("Density")+
		xlab("Density")
	##-> Pas de noeuds évidents

	#===========#
	#  Poldur   #
	#===========#

arb_p = rpart(Nb1~
	Poldur,
	data = db1a, method = "poisson",
	cp = 0.65e-3)
	fancyRpartPlot(arb_p)

arb_p$cptable
rel_err = arb_p$cptable[,4]
which(rel_err == min(rel_err))
cpopt = arb_p$cptable[which(rel_err==min(rel_err)),1]
cpopt	


####NE PAS LA TRANSFORMER
	
	reg_dur_fac = glm(Nb1~as.factor(Poldur), data = db1a, family = poisson)
	pred_dur_fac = predict(reg_dur_fac,type = 'response', newdata = db1a)
	
	
	reg_dur = glm(Nb1~(Poldur), data = db1a, family = poisson(link='log'))
	pred_dur = predict(reg_dur, type = 'response', newdata = db1a)
	
	
	reg_dur_sp = glm(Nb1~bs(Poldur, degree = 1, knots = c(1)), data = db1a, family = poisson)
	pred_dur_sp = predict(reg_dur_sp, type = 'response', newdata = db1a)
	
	BIC(reg_dur_sp)
	
	##Graphe Catégorielle/Linéaire
	
	mat_dur1 = matrix(1, nrow = length(ind), ncol = 2)
	mat_dur1[,1] = pred_dur_fac
	mat_dur1[,2] = pred_dur
	
	data_dur1 = as.data.frame(mat_dur1)
	data_dur1[,3] = Poldur[ind]
	
	
	ggplot(data_dur1,aes(x = Poldur[ind], y=pred_dur_fac,colour = "Catégorielle"))+
		geom_line()+
		geom_line(data = data_dur1 , aes(x=Poldur[ind],y=pred_dur,color= "Linéaire"))+
		ylab("Prédiction")+
		scale_colour_manual("",breaks = c("Catégorielle","Linéaire", "Spline"),values = c("black", "red", "blue"))+
		ggtitle("Ancienneté")+
		xlab("Ancienneté")+
		geom_line(data = data_dur1 , aes(x=Poldur[ind],y=pred_dur_sp,color= "Spline"))+
		geom_vline(data = data_dur1, aes(xintercept = 1), colour = "purple")

	
	
	## Noeuds : vers 2
	
	
	#===========#
	#   Bonus   #
	#===========#
arb_b = rpart(Nb1~
	Bonus,
	data = db1a, method = "poisson",
	cp = 5e-3)
	fancyRpartPlot(arb_b)


arb_b$cptable
rel_err = arb_b$cptable[,4]
which(rel_err == min(rel_err))
cpopt = arb_b$cptable[which(rel_err==min(rel_err)),1]
cpopt	

	
	
	reg_bonus_fac = glm(Nb1~as.factor(Bonus), data = db1a, family = poisson)
	pred_bonus_fac = predict(reg_bonus_fac,type = 'response', newdata = db1a)
	
	
	reg_bonus = glm(Nb1~(Bonus), data = db1a, family = poisson(link='log'))
	pred_bonus = predict(reg_bonus, type = 'response', newdata = db1a)
	
	
	reg_bonus_sp = glm(Nb1~bs(Bonus, degree = 3, knots = c(-15,55)), data = db1a, family = poisson)
	pred_bonus_sp = predict(reg_bonus_sp, type = 'response', newdata = db1a)
	
	BIC(reg_bonus_sp)
	
	##Graphe Catégorielle/Linéaire
	
	mat_bonus1 = matrix(1, nrow = length(ind), ncol = 2)
	mat_bonus1[,1] = pred_bonus_fac
	mat_bonus1[,2] = pred_bonus
	
	data_bonus1 = as.data.frame(mat_bonus1)
	data_bonus1[,3] = Bonus[ind]
	

	reg_bonus_sp1 = glm(Nb1~bs(Bonus, degree = 1, knots = c(0)), data = db1a, family = poisson)
	pred_bonus_sp1 = predict(reg_bonus_sp, type = 'response', newdata = db1a)
	
	
	ggplot(data_bonus1,aes(x = Bonus[ind], y=pred_bonus_fac,colour = "Catégorielle"))+
		geom_line()+
		geom_line(data = data_bonus1 , aes(x=Bonus[ind],y=pred_bonus,color= "Linéaire"))+
		ylab("Prédiction")+
		scale_colour_manual("",breaks = c("Catégorielle","Linéaire", "Spline"),values = c("black", "red", "blue"))+
		ggtitle("Bonus")+
		xlab("Bonus")+
		geom_line(data = data_bonus1 , aes(x=Bonus[ind],y=pred_bonus_sp,color= "Spline"))


		#geom_vline(data = data_bonus1, aes(xintercept = 1), colour = "purple")
	
	
	## Noeuds : 0 et vers 125
	

	
test_sp1= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 3, knots = c(30,50))+Bonus+Poldur
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp1)

test_sp2= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 3, knots = c(30,50))+Poldur
+bs(Bonus, degree = 3, knots = c(-15,55))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp2)

test_sp3= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 3, knots = c(30,50))+Poldur+bs(Bonus, degree = 1, knots = c(0))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp3)


#Age : D3;30,50#
#Poldur : D1;0.5,7.5#
test_sp4= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 3, knots = c(30,50))+bs(Poldur, degree = 1, knots = c(0.5,7.5))+Bonus
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp4)

test_sp5= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 3, knots = c(30,50))+bs(Bonus, degree = 3, knots = c(-15,55))+bs(Poldur, degree = 1, knots = c(0.5,7.5))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp5)

test_sp6= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 3, knots = c(30,50))+bs(Bonus, degree = 1, knots = c(0))+bs(Poldur, degree = 1, knots = c(0.5,7.5))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp6)


#Age : D3;30,50#
#Poldur : D1;1#

test_sp7= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 3, knots = c(30,50))+bs(Poldur, degree = 1, knots = c(1))+Bonus
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp7)


test_sp8= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 3, knots = c(30,50))+bs(Bonus, degree = 3, knots = c(-15,55))+bs(Poldur, degree = 1, knots = c(1))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp8)

test_sp9= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 3, knots = c(30,50))+bs(Bonus, degree = 1, knots = c(0))+bs(Poldur, degree = 1, knots = c(1))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp9)


#AGE : D1; 35# 
#Bonus : RIEN#

test_sp10= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 1, knots = c(35))+Bonus+Poldur
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp10)

test_sp11= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 1, knots = c(35))+Poldur
+bs(Bonus, degree = 3, knots = c(-15,55))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp11)

test_sp12= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 1, knots = c(35))+Poldur+bs(Bonus, degree = 1, knots = c(0))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp12)


#Age : D3;30,50#
#Poldur : D1;0.5,7.5#
test_sp13= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 1, knots = c(35))+bs(Poldur, degree = 1, knots = c(0.5,7.5))+Bonus
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp13)

test_sp14= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 1, knots = c(35))+bs(Bonus, degree = 3, knots = c(-15,55))+bs(Poldur, degree = 1, knots = c(0.5,7.5))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp14)

test_sp15= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 1, knots = c(35))+bs(Bonus, degree = 1, knots = c(0))+bs(Poldur, degree = 1, knots = c(0.5,7.5))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp15)


#Age : D3;30,50#
#Poldur : D1;1#

test_sp16= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 1, knots = c(35))+bs(Poldur, degree = 1, knots = c(1))+Bonus
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp16)


test_sp17= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 1, knots = c(35))+bs(Bonus, degree = 3, knots = c(-15,55))+bs(Poldur, degree = 1, knots = c(1))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp17)

test_sp18= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+bs(Age, degree = 1, knots = c(35))+bs(Bonus, degree = 1, knots = c(0))+bs(Poldur, degree = 1, knots = c(1))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp18)


#AGE : RIEN# 
#Bonus : RIEN#

test_sp19= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+Age+Bonus+Poldur
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp19)

test_sp20= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+Age+Poldur
+bs(Bonus, degree = 3, knots = c(-15,55))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp20)

test_sp21= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+Age+Poldur+bs(Bonus, degree = 1, knots = c(0))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp21)


#Age : D3;30,50#
#Poldur : D1;0.5,7.5#
test_sp22= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+Age+bs(Poldur, degree = 1, knots = c(0.5,7.5))+Bonus
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp22)

test_sp23= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+Age+bs(Bonus, degree = 3, knots = c(-15,55))+bs(Poldur, degree = 1, knots = c(0.5,7.5))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp23)

test_sp24= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+Age+bs(Bonus, degree = 1, knots = c(0))+bs(Poldur, degree = 1, knots = c(0.5,7.5))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp24)


#Age : D3;30,50#
#Poldur : D1;1#

test_sp25= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+Age+bs(Poldur, degree = 1, knots = c(1))+Bonus
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp25)


test_sp26= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+Age+bs(Bonus, degree = 3, knots = c(-15,55))+bs(Poldur, degree = 1, knots = c(1))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp26)

test_sp27= glm(Nb1~
Gender+Type+Occupation+Density+Group1+Group2_4+Adind+Age+bs(Bonus, degree = 1, knots = c(0))+bs(Poldur, degree = 1, knots = c(1))
+offset(log(Exppdays)), 
data = db1a, family = poisson)

summary(test_sp27)


BIC_test_sp = rep(-1,27)

BIC_test_sp[1] = BIC(test_sp1)
BIC_test_sp[2] = BIC(test_sp2)
BIC_test_sp[3] = BIC(test_sp3)
BIC_test_sp[4] = BIC(test_sp4)
BIC_test_sp[5] = BIC(test_sp5)
BIC_test_sp[6] = BIC(test_sp6)
BIC_test_sp[7] = BIC(test_sp7)
BIC_test_sp[8] = BIC(test_sp8)
BIC_test_sp[9] = BIC(test_sp9)
BIC_test_sp[10] = BIC(test_sp10)
BIC_test_sp[11] = BIC(test_sp11)
BIC_test_sp[12] = BIC(test_sp12)
BIC_test_sp[13] = BIC(test_sp13)
BIC_test_sp[14] = BIC(test_sp14)
BIC_test_sp[15] = BIC(test_sp15)
BIC_test_sp[16] = BIC(test_sp16)
BIC_test_sp[17] = BIC(test_sp17)
BIC_test_sp[18] = BIC(test_sp18)
BIC_test_sp[19] = BIC(test_sp19)
BIC_test_sp[20] = BIC(test_sp20)
BIC_test_sp[21] = BIC(test_sp21)
BIC_test_sp[22] = BIC(test_sp22)
BIC_test_sp[23] = BIC(test_sp23)
BIC_test_sp[24] = BIC(test_sp24)
BIC_test_sp[25] = BIC(test_sp25)
BIC_test_sp[26] = BIC(test_sp26)
BIC_test_sp[27] = BIC(test_sp27)



which(BIC_test_sp == min(BIC_test_sp))
BIC_test_sp

plot(BIC_test_sp)

BIC_9 = rep(1,9)
BIC_9[1] = BIC(test_sp1)
BIC_9[2] = BIC(test_sp2)
BIC_9[3] = BIC(test_sp3)
BIC_9[4] = BIC(test_sp4)
BIC_9[5] = BIC(test_sp5)
BIC_9[6] = BIC(test_sp6)
BIC_9[7] = BIC(test_sp7)
BIC_9[8] = BIC(test_sp8)
BIC_9[9] = BIC(test_sp9)

which(BIC_9 == min(BIC_9))
BIC_9

plot(BIC_9)



#############################
## Matrice de confusion    ##
#############################

pred_conf1 = predict(test_sp1, newdata = db1t,type ='response')
pred_conf7 = predict(test_sp7, newdata = db1t,type ='response')
pred_conf10 = predict(test_sp10, newdata = db1t,type ='response')

mat_conf1 = matrix(1,ncol = 8, nrow = 8)
mat_conf7 = matrix(1,ncol = 8, nrow = 8)
mat_conf10 = matrix(1,ncol = 8, nrow = 8)

mat_poids_err_lin = matrix(1,8,8)
mat_poids_err_quad = matrix(1,8,8)



for (i in 0:7){
	for (j in 0:7){
		mat_conf1[i+1,j+1] = length(which((Nb1t == i) & (round(pred_conf1) == j)))
		mat_conf7[i+1,j+1] = length(which((Nb1t == i) & (round(pred_conf7) == j)))
		mat_conf10[i+1,j+1] = length(which((Nb1t == i) & (round(pred_conf10) == j)))
		
		mat_poids_err_lin[i+1,j+1] = abs(i-j)
		mat_poids_err_quad[i+1,j+1] = (i-j)^2
	}
}

mat_conf1
mat_conf7
mat_conf10

mat_poids_err_lin
mat_poids_err_quad

sum(diag(mat_conf1))/length(Nb1t)
sum(diag(mat_conf7))/length(Nb1t)
sum(diag(mat_conf10))/length(Nb1t)

sum(mat_poids_err_lin*mat_conf1)
sum(mat_poids_err_lin*mat_conf7)
sum(mat_poids_err_lin*mat_conf10)

sum(mat_poids_err_quad*mat_conf1)
sum(mat_poids_err_quad*mat_conf7)
sum(mat_poids_err_quad*mat_conf10)

	#---------------------------------------------------#
	##		Regression finale				   ##
	#---------------------------------------------------#
	


	#=================#
	# Reg avec arbres #
	#=================#
		
	#Ici nous faisons reg avec les variables catégorielles issues de l'étude des arbres
	#Ensuite nous allons comparer les BIC de cette régression avec l'ancienne (step1_4) puis
	#avec la reg splinée

	step_arb = stepwise(glm(Nb1~
	Gender+Type+Category+Occupation+Age_Tr+Bonus_Tr+Poldur_Tr+Value+Density_Tr+(Group1)+Group2_4+Adind
	+offset(log(Exppdays))
	,data=db1a, family=poisson))

	summary(step_arb)
	BIC(step_arb)

	Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+(Group1)+Group2_4+Adind


	#step_fin = stepwise(glm(Nb1~
	#Gender+Type+Category+Occupation+Age_Tr+Bonus_Tr+Poldur+Value+Density_Tr+(Group1)+Group2_4
	#+offset(log(Exppdays)),
	#data = db1a, family = poisson))
	
	#summary(step_fin)

	step_opt=glm(Nb1~Gender+Type+Occupation+Age_Tr+Bonus_Tr+Poldur+Density_Tr+(Group1)+Group2_4
	+offset(log(Exppdays)),data=db1a, family=poisson)

	summary(step_opt)
	BIC(step_opt)	
 
#REGROUPER O P Q R AVEC L ?

	#step_fin_bis = stepwise(glm(Nb1~
	#Gender+Type+Category+Occupation+Age_Tr+Bonus_Tr+Poldur+Value+Density_Tr+(Group1)+Group2
	#+offset(log(Exppdays)),
	#data = db1a, family = poisson))
	
	summary(step_fin_bis)


step_opt_bis=glm(Nb1~Gender+Type+Occupation+Age_Tr+Bonus_Tr+Poldur+Density_Tr+(Group1)+Group2
	+offset(log(Exppdays)),data=db1a, family=poisson)

	summary(step_opt_bis)

#REGROUPER O P Q R T U 
	

	#===================#
	# Reg avec splines  #
	#===================#
	
	step_opt_bis=glm(Nb1~
	Gender+Type+Occupation+
	bs(Age, degree = 3, knots  = c(30,50))+
	Bonus+
	bs(Poldur, degree = 1, knots  = c(1))+
	Density+
	(Group1)+Group2_4
	+offset(log(Exppdays))
	,data=db1a, family=poisson)

	summary(step_opt_bis)
	BIC(step_opt_bis)

	pred_step_opt_bis = predict(step_opt_bis, type = 'response', newdata = db1t)

	data_step_opt_bis = as.data.frame(pred_step_opt_bis)
	
	ggplot(data_step_opt_bis, aes(pred_step_opt_bis)) +
	geom_histogram(binwidth = 0.05, fill = 'light blue', color = 'red')+	
	ggtitle("Prévisions : Modèle Quasi-Poisson")+
	xlab("Prédictions")+
	ylab("Fréquence")+
	xlim(c(0,2))
	
	BIC(step_opt_bis) - BIC(step1_4)

	###################################
	## 	EFFETS MARGINAUX MOYENS    ##
	###################################
	
	modele1 = step_opt_bis
	
	
	pred_marg = predict(modele1, type = 'response', newdata = db1a)
	summary(pred_marg)
	eff_marg_moy = mean(pred_marg)*coef(modele1)
	eff_marg_moy
	
	
	#=================#
	# Surdispersion   #
	#=================#
	
	
	##
	#  Test de surdispersion
	##
	
	dispersiontest(modele1)
	
	
	############################
	
	
	
	esperance = mean(Nb1)/mean(Exppdays)
	esperance
	
	variance = var(Nb1)/mean(Exppdays)
	variance 
	variance/esperance
	
	
	step_fin2 = glm(Nb1~
	Gender+Type+Occupation+
	bs(Age, degree = 3, knots  = c(30,50))+
	Bonus+
	bs(Poldur, degree = 1, knots  = c(1))+
	Density+
	(Group1)+Group2_4
	+offset(log(Exppdays)),
	data = db1a, family = quasipoisson)
	
	pred_step_fin2 = predict(step_fin2, type = 'response', newdata = db1t)	

	summary(step_fin2)
	BIC(step_fin2)
	
	data_step_fin2 = as.data.frame(pred_step_fin2)
	
	ggplot(data_step_fin2, aes(pred_step_fin2)) +
	geom_histogram(binwidth = 0.05, fill = 'light blue', color = 'red')+	
	ggtitle("Prévisions : Modèle Quasi-Poisson")+
	xlab("Prédictions")+
	ylab("Fréquence")+
	xlim(c(0,2))
	

	mat_conf_fin2 = matrix(1,ncol = 8, nrow = 8)

	mat_poids_err_lin = matrix(1,8,8)
	mat_poids_err_quad = matrix(1,8,8)



for (i in 0:7){
	for (j in 0:7){
		mat_conf_fin2[i+1,j+1] = length(which((Nb1t == i) & (round(pred_fin2) == j)))
		mat_poids_err_lin[i+1,j+1] = abs(i-j)
		mat_poids_err_quad[i+1,j+1] = (i-j)^2
	}
}
	
	mat_conf_fin2
	sum(diag(mat_conf_fin2))/length(Nb1t)
	sum(mat_poids_err_lin*mat_conf_fin2)
	sum(mat_poids_err_quad*mat_conf_fin2)

mat_zero = matrix(0,8,8)

for (i in 0:7){
	for (j in 0:7){
		mat_zero[i+1,j+1] = length(which((Nb1t == i) & (0 == j)))
		mat_poids_err_lin[i+1,j+1] = abs(i-j)
		mat_poids_err_quad[i+1,j+1] = (i-j)^2
	}
}
mat_zero
	sum(diag(mat_zero))/length(Nb1t)
	sum(mat_poids_err_lin*mat_zero)
	sum(mat_poids_err_quad*mat_zero)



	##	Modèle 2 : Reg binom   ##
	##---------------------------##
	
	
#	reg_neg_bin1 = glm.nb(Nb1~
#	Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
#	+offset(log(Exppdays)),
#	data = db1a)
	
#	summary(reg_neg_bin1)
	
	
	##On fait varier le paramètre alpha
	
#	reg_neg_bin1 = glm(Nb1~
#	Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
#	+offset(log(Exppdays)),
#	data = db1a, family = negative.binomiale())
	
#	summary(reg_neg_bin1)
	
	
	##---------------------------##
	##	Modèle 2 : Zero Infl   ##
	##---------------------------##
	
	
#REG POISSON#######	

reg_zero_inf_ps = zeroinfl(Nb1~
	Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
	,offset = (log(Exppdays)),
	data = db1a, dist = 'poisson', link = 'logit')

summary(reg_zero_inf_ps)
vuong(modele1,reg_zero_inf_ps)





reg_zero_inf_ps_1 = zeroinfl(Nb1~
	Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
	|Occupation+Age+Bonus+Density+Group2+Adind,offset = (log(Exppdays)),
	data = db1a, dist = 'poisson', link = 'logit')

summary(reg_zero_inf_ps_1)
vuong(step1,reg_zero_inf_ps_1)


reg_zero_inf_ps_2 = zeroinfl(Nb1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
|Gender+Type+Category+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind,
	offset = (log(Exppdays)),
	data = db1a, dist = 'poisson', link = 'logit')

summary(reg_zero_inf_ps_2)


reg_zero_inf_ps_3 = zeroinfl(Nb1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
|Gender+Occupation+Age+Bonus+Group2,
	offset = (log(Exppdays)),
	data = db1a, dist = 'poisson', link = 'logit')

summary(reg_zero_inf_ps_3)



pred_zero_inf_ps1 = predict(reg_zero_inf_ps_3, type = 'response', newdata = db1t)


	mat_conf_zero_p= matrix(1,ncol = 8, nrow = 8)

	mat_poids_err_lin = matrix(1,8,8)
	mat_poids_err_quad = matrix(1,8,8)



for (i in 0:7){
	for (j in 0:7){
		mat_conf_zero_p[i+1,j+1] = length(which((Nb1t == i) & (round(pred_zero_inf_ps1) == j)))
		mat_poids_err_lin[i+1,j+1] = abs(i-j)
		mat_poids_err_quad[i+1,j+1] = (i-j)^2
	}
}
	

	sum(diag(mat_conf_zero_p))/length(Nb1t)
	sum(mat_poids_err_lin*mat_conf_zero_p)
	sum(mat_poids_err_quad*mat_conf_zero_p)




#######REG BIN NEG#########"
reg_zero_inf_neg_bin = zeroinfl(Nb1~
	Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
	,offset = (log(Exppdays)),
	data = db1a, dist = 'negbin', link = 'logit')

summary(reg_zero_inf_neg_bin)
#vuong(reg_neg_bin1,reg_zero_inf_neg_bin)

reg_zero_inf_neg_bin_1 = zeroinfl(Nb1~
	Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
	|Occupation+Age+Bonus+Density+Group2+Adind,offset = (log(Exppdays)),
	data = db1a, dist = 'negbin', link = 'logit')

summary(reg_zero_inf_neg_bin_1)
#vuong(reg_neg_bin1,reg_zero_inf_neg_bin_1)

reg_zero_inf_neg_bin_2 = zeroinfl(Nb1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+(Group1)+Group2+Adind
|Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+(Group1)+Group2+Adind
,offset = (log(Exppdays)),
	data = db1a, dist = 'negbin', link = 'logit')

summary(reg_zero_inf_neg_bin_2)

reg_zero_inf_neg_bin_3 = zeroinfl(Nb1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
|Gender+Category+Occupation+Age+Bonus+Adind
,offset = (log(Exppdays)),
	data = db1a, dist = 'negbin', link = 'logit')

summary(reg_zero_inf_neg_bin_3)


## CCL -> Pour le zero, on change Occupation pour n'avoir que retraités ou autres

Retraite_zero_nb1 = rep(0,n_db1)
Retraite_zero_nb1[which(Occupation == "Retired")] = 1
db1$Retraite_zero_nb1 = Retraite_zero_nb1

Group2_5 = Group2
Group2_5[which(Group2 == "L" | Group2 == "S" | Group2 == "T" | Group2 == "U")] = "L"
db1$Group2_5 = Group2_5



attach(db1)


reg_zero_inf_neg_bin_4 = zeroinfl(Nb1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2_5+Adind
|Gender+Retraite_zero_nb1+Age+Bonus
,offset = (log(Exppdays)),
	data = db1a, dist = 'negbin', link = 'logit')

summary(reg_zero_inf_neg_bin_4)

res.nb4 = residuals(reg_zero_inf_neg_bin_4, type = 'pearson', newdata = db1t)

hist(res.nb4)
summary(res.nb4)



reg_zero_inf_neg_bin_5 = zeroinfl(Nb1~
Gender+Type+Category+Occupation+bs(Age, degree = 3, knots  = c(30,50))+Bonus+bs(Poldur, degree = 1, knots  = c(1))+Density+(Group1)+Group2_5+Adind
|Gender+Retraite_zero_nb1+bs(Age, degree = 1, knots = c(35,65))+bs(Bonus, degree = 2, knots = c(-20,0,60))
,offset = (log(Exppdays)),
	data = db1a, dist = 'negbin', link = 'logit')

summary(reg_zero_inf_neg_bin_5)


pred_zero_inf_nb2 = predict(reg_zero_inf_neg_bin_5, type = 'response', newdata = db1t)

	mat_conf_zero_nb2= matrix(1,ncol = 8, nrow = 8)

	mat_poids_err_lin = matrix(1,8,8)
	mat_poids_err_quad = matrix(1,8,8)



for (i in 0:7){
	for (j in 0:7){
		mat_conf_zero_nb2[i+1,j+1] = length(which((Nb1t == i) & (round(pred_zero_inf_nb2) == j)))
		mat_poids_err_lin[i+1,j+1] = abs(i-j)
		mat_poids_err_quad[i+1,j+1] = (i-j)^2
	}
}
	
mat_conf_zero_nb2
	sum(diag(mat_conf_zero_nb2))/length(Nb1t)
	sum(mat_poids_err_lin*mat_conf_zero_nb2)
	sum(mat_poids_err_quad*mat_conf_zero_nb2)



pred_zero_inf_nb1 = predict(reg_zero_inf_neg_bin_4, type = 'response', newdata = db1t)


	mat_conf_zero_nb1= matrix(1,ncol = 8, nrow = 8)

	mat_poids_err_lin = matrix(1,8,8)
	mat_poids_err_quad = matrix(1,8,8)



for (i in 0:7){
	for (j in 0:7){
		mat_conf_zero_nb1[i+1,j+1] = length(which((Nb1t == i) & (round(pred_zero_inf_nb1) == j)))
		mat_poids_err_lin[i+1,j+1] = abs(i-j)
		mat_poids_err_quad[i+1,j+1] = (i-j)^2
	}
}
	
mat_conf_zero_nb1
	sum(diag(mat_conf_zero_nb1))/length(Nb1t)
	sum(mat_poids_err_lin*mat_conf_zero_nb1)
	sum(mat_poids_err_quad*mat_conf_zero_nb1)

hist(Nb1t)
hist(pred_zero_inf_nb1)
hist(pred_fin2, add = T)

data_zero_inf_nb1 = as.data.frame(pred_zero_inf_nb1)
	
	ggplot(data_zero_inf_nb1, aes(pred_zero_inf_nb1)) +
	geom_histogram(binwidth = 0.05, fill = 'light blue', color = 'red')+	
	ggtitle("Prévisions : Modèle à inflation de zéros")+
	xlab("Prédictions")+
	ylab("Fréquence")+
	xlim(c(0,2))
	
data_tot = data.frame(data_step_fin2[,1],data_zero_inf_nb1[,1])


	ggplot(data_tot, aes(data_tot[,1]) +
	geom_histogram(binwidth = 0.05, fill = 'light blue', color = 'red')+	
	ggtitle("Prévisions : Modèle à inflation de zéros")+
	xlab("Prédictions")+
	ylab("Fréquence")+
	xlim(c(0,2))+
	ggplot(data_tot, aes(data_tot[,2]))
	




#!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
####ECRITURE PREDICTIONS######
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!#





n_pricing = dim(pricing)[1]
#Gender
a_pricing = rep(0,n_pricing)
ind_a_pricing = which(pricing$Gender == 'Female')
a_pricing[ind_a_pricing] = 1

pricing$Gender = a_pricing

pricing$Exppdays = rep(1,n_pricing)

pricing$Density = as.numeric(as.character(pricing$Density))


pricing$Group2_4 = pricing$Group2
ind_Group2_4_pricing = which(pricing$Group2 == "L" | pricing$Group2 == "S" | pricing$Group2 == "T" | pricing$Group2 == "U" )
pricing$Group2_4[ind_Group2_4_pricing] = "L"

new2 = db1a[1:n_pricing,]
new2$Group2_4 = Group2_4[1:n_pricing]


new2$PolNum = pricing$PolNum
new2$CalYear = pricing$CalYear
new2$Gender = pricing$Gender
new2$Type = pricing$Type
new2$Category = pricing$Category
new2$Occupation = pricing$Occupation
new2$Age = pricing$Age
new2$Group1 = pricing$Group1
new2$Bonus = pricing$Bonus
new2$Poldur = pricing$Poldur
new2$Value = pricing$Value
new2$Adind = pricing$Adind
new2$SubGroup2 = pricing$SubGroup2
new2$Group2 = pricing$Group2
new2$Density = pricing$Density
new2$Exppdays = pricing$Exppdays
new2$Group2_4 = pricing$Group2_4

#pred_step_fin2
s_fin1 = predict(step_fin2, type = 'response', newdata = new2)
summary(s_fin1)


s_fin2 = predict(reg_zero_inf_neg_bin_4, type = 'response', newdata = new2)
summary(s_fin2)

summary(s_fin1 - s_fin2)
hist(s_fin1 - s_fin2)

pricing_Toullet_Billod = data.frame(pricing$PolNum, Freq1 = rep(0,n_pricing), Freq2= rep(0,n_pricing))

pricing_Toullet_Billod[,2] = s_fin1
pricing_Toullet_Billod[,3] = s_fin2

write.csv2(ind, file  = 'ind_tir.csv')

getwd()
write.csv2(pricing_Toullet_Billod, file='Projet2_Toullet_Billod_pricing.csv')