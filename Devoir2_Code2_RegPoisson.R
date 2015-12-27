

##----------------------------------------##
## 		Modèle de Poisson			##
##----------------------------------------##

step1 = stepwise(glm(Nb1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+(Group1)+Group2+Adind
+offset(log(Exppdays)),
data = db1a, family = poisson))

summary(step1)



########################################
## Traitement par arbres  ##############
########################################


#BUT : obtenir des catégories pour certaines variables continues

arb_age = rpart(Nb1~
Age,
data = db1a, method = "poisson",
cp = 4e-3)
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


##Graphe Catégorielle/Linéaire/Splines








#============#
#  Density   #
#============#


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


## Noeuds : pas de noeuds a posteriori, trop de bruits



##Graphe Catégorielle/Linéaire/Splines


#===========#
#  Poldur   #
#===========#

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

step_fin = glm(Nb1~
Gender+Type+Category+Occupation+Age+Bonus+Poldur+Value+Density+(Group1)+Group2
+offset(log(Exppdays)),
data = db1a, family = poisson)

summary(step1)


###################################
## 	EFFETS MARGINAUX MOYENS    ##
###################################

modele1 = step_fin


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
Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
+offset(log(Exppdays)),
data = db1a, family = quasipoisson)

summary(step_fin2)



