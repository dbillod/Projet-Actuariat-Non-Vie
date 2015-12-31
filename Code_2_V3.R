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
	
	#pricing = read.csv2("C:/Users/David/Documents/ENSAE/3A/Actuariat Non-Vie/pricing.csv")
	#pricing = pricing[,-1]
	
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
Age_Tr[which(Age>=48)]=3
Age_Tr=as.factor(Age_Tr)

Density_Tr=rep(3,n_db1)
Density_Tr[which(Density<163)]=2
Density_Tr[which(Density<84)]=1
Density_Tr=as.factor(Density_Tr)	
	
Bonus_Tr=rep(1,n_db1)
Bonus_Tr[which(Bonus>=15)]=2
Bonus_Tr=as.factor(Bonus_Tr)

	
db1$Age_Tr = Age_Tr
db1$Density_Tr = Density_Tr
db1$Bonus_Tr=Bonus_Tr	

	
	
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
	
	
	
	
	reg_age_fac = glm(Nb1~as.factor(Age), data = db1a, family = poisson)
	pred_age_fac = predict(reg_age_fac,type = 'response', newdata = db1a)
	
	
	reg_age = glm(Nb1~(Age), data = db1a, family = poisson(link='log'))
	pred_age = predict(reg_age, type = 'response', newdata = db1a)
	
	
	reg_age_sp = glm(Nb1~bs(Age, degree = 1, knots = c(35,65)), data = db1a, family = poisson)
	pred_age_sp = predict(reg_age_sp, type = 'response', newdata = db1a)
	
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
		scale_colour_manual("",breaks = c("Catégorielle","Linéaire"),values = c("black", "red"))+
		ggtitle("Age")+
		xlab("Age")
	
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
	cp = 1e-3)
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
	
	
	reg_dur_sp = glm(Nb1~bs(Poldur, degree = 1, knots = c(35,65)), data = db1a, family = poisson)
	pred_dur_sp = predict(reg_dur_sp, type = 'response', newdata = db1a)
	
	
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
		scale_colour_manual("",breaks = c("Catégorielle","Linéaire"),values = c("black", "red"))+
		ggtitle("Ancienneté")+
		xlab("Ancienneté")
	
	
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
	
	
	reg_bonus_sp = glm(Nb1~bs(Bonus, degree = 1, knots = c(35,65)), data = db1a, family = poisson)
	pred_bonus_sp = predict(reg_bonus_sp, type = 'response', newdata = db1a)
	
	
	
	##Graphe Catégorielle/Linéaire
	
	mat_bonus1 = matrix(1, nrow = length(ind), ncol = 2)
	mat_bonus1[,1] = pred_bonus_fac
	mat_bonus1[,2] = pred_bonus
	
	data_bonus1 = as.data.frame(mat_bonus1)
	data_bonus1[,3] = Bonus[ind]
	
	
	ggplot(data_bonus1,aes(x = Bonus[ind], y=pred_bonus_fac,colour = "Catégorielle"))+
		geom_line()+
		geom_line(data = data_bonus1 , aes(x=Bonus[ind],y=pred_bonus,color= "Linéaire"))+
		ylab("Prédiction")+
		scale_colour_manual("",breaks = c("Catégorielle","Linéaire"),values = c("black", "red"))+
		ggtitle("Bonus")+
		xlab("Bonus")
	
	
	## Noeuds : 0 et vers 125
	

	#---------------------------------------------------#
	##		Regression finale				   ##
	#---------------------------------------------------#
	
	#step_fin = stepwise(glm(Nb1~
	#Gender+Type+Category+Occupation+Age_Tr+Bonus_Tr+Poldur+Value+Density_Tr+(Group1)+Group2_4
	#+offset(log(Exppdays)),
	#data = db1a, family = poisson))
	
	#summary(step_fin)

	step_opt=glm(Nb1~Gender+Type+Occupation+Age_Tr+Bonus_Tr+Poldur+Density_Tr+(Group1)+Group2_4
	+offset(log(Exppdays)),data=db1a, family=poisson)

	summary(step_opt)
 
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
	
	
	###################################
	## 	EFFETS MARGINAUX MOYENS    ##
	###################################
	
	modele1 = step_opt
	
	
	pred_marg = predict(modele1, type = 'response', newdata = db1a)
	eff_marg_moy = mean(pred_marg)*coef(modele1)
	
	
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
	Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2_4+Adind
	+offset(log(Exppdays)),
	data = db1a, family = quasipoisson)
	
	summary(step_fin2)




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
vuong(step1,reg_zero_inf_ps)

reg_zero_inf_ps_1 = zeroinfl(Nb1~
	Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
	|Occupation+Age+Bonus+Density+Group2+Adind,offset = (log(Exppdays)),
	data = db1a, dist = 'poisson', link = 'logit')

summary(reg_zero_inf_ps_1)
vuong(step1,reg_zero_inf_ps_1)


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


####ETUDIER DES REGROUPEMENTS DE MODALITES######
