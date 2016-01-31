library(readODS)
library(ChainLadder)
library(ggplot2)
library(MASS)
library(AER)
library(pscl)
library(gamlss)

##########################################
## RECUPERATION DES DONNEES ##############
##########################################

data = read.ods("C:/Users/David/Documents/ENSAE/3A/Actuariat Non-Vie/triangle_ensae.ods")
data = data[[1]]
class(data)
data

triangle = data[4:17,3:16]
triangle

## On va remplacer les valeurs vides par des NA pour faire tourner le chain ladder

for (i in 1:dim(triangle)[1]){
	newcolonne = vector("numeric",dim(triangle)[2])
	for (j in 1:dim(triangle)[2]){
		newcolonne[j] = as.numeric(gsub(",",".",triangle[j,i]))
	}
	triangle[,i] = newcolonne
}
triangle
rownames(triangle) = 1:14
class(triangle[1,1])


# On va créer le triangle  des incréments de paiements
incr = triangle
for (i in 1: dim(triangle)[1]){
	for (j in 2:dim(triangle)[2]){
		incr[i,j] = triangle[i,j] - triangle[i,j-1]
	}
}
incr

chain_std1 = MackChainLadder(triangle)
chain_std1

plot(chain_std1)
plot(chain_std1,  which = 4 )
plot(chain_std1, lattice = T)


#On va construire le vecteur des cadences de paiements
cad_CH1 = rep(1,length(chain_std1$f))
for (i in 1:length(chain_std1$f)){
	cad_CH1[i] = 1/prod(chain_std1$f[i:length(chain_std1$f)])
}
cad_CH1


# Ultimate Loss and Tail Factor
chain_std2 = MackChainLadder(triangle, tail = T)
chain_std2


#Méthode avec pondération différente

#On construit le triangle des link ratios
tri_facteurs = triangle
tri_facteurs = triangle[,-1]
tri_facteurs

for (j in 1:(dim(triangle)[2]-1)){
	for (i in  1:(dim(triangle)[1]-j)){
		tri_facteurs[i,j] = triangle[i,j+1]/triangle[i,j]
	}
}
tri_facteurs = tri_facteurs[-dim(tri_facteurs)[1],]
triangle
tri_facteurs


#On va maintenant introduire le poids omega_i,j = i+j+1 et calculer notre vecteur de coeff de passage

omega = matrix(0, nrow = dim(triangle)[1]-1, ncol = dim(triangle)[2]-1)
for (i in 1:(dim(triangle)[1]-1)){
	for (j in 1:(dim(triangle)[2]-1)){
		omega[i,j] = i+j-1
	}	
}
omega

coeff_passage = rep(1,dim(tri_facteurs)[2])
for (j in 1:length(coeff_passage)){
	coeff_passage[j] = (omega[1:(dim(tri_facteurs)[1]-j+1),j]%*% tri_facteurs[1:(dim(tri_facteurs)[1]-j+1),j]) / (omega[1:(dim(tri_facteurs)[1]-j+1),j] %*% rep(1,dim(tri_facteurs)[1]-j+1))
}
coeff_passage = c(coeff_passage,1)
#On rajoute 1 pour avoir la même taille que chain_std1$f
length(coeff_passage) == length(chain_std1$f)

cad_CL2 = coeff_passage
for (i in 1:length(cad_CL2)){
	cad_CL2[i] = 1/prod(coeff_passage[i:length(coeff_passage)])
}
cad_CL2

#Maintenant on va projeter la partie inférieure du triangle de charge grâce à ce coeff_passage

triangle_CL2 = triangle
for (i in 1:dim(triangle)[1]){
	for (j in 1:dim(triangle)[2]){
		if (is.na(triangle[i,j])){
			triangle_CL2[i,j] = triangle_CL2[i,j-1] * coeff_passage[j-1]
		}
	}
}
triangle_CL2

#Calcul des IBNR
vec_paiements = rep(1, dim(triangle)[1]-1)
for (i in 1:length(vec_paiements)){
	vec_paiements[i] = triangle_CL2[i+1,dim(triangle_CL2)[2]] - triangle_CL2[i+1,dim(triangle_CL2)[2]-i]
}

vec_paiements
sum(vec_paiements)



#-------------------------------------------------#
# On teste l'hypothèse Chain Ladder en plottant 
# les charges cumulées d'une année 
# de survenance contre celle d'avant
#-------------------------------------------------#

#vec_test_CLx = matrix(0, nrow = dim(triangle)[1]-1 , ncol = dim(triangle)[2]-1)
#vec_test_CLy = matrix(0, nrow = dim(triangle)[1]-1 , ncol = dim(triangle)[2]-1)


#for (annee_surv in 1:(dim(triangle)[2]-1)){
	vec_test_CLx[1:(dim(triangle)[1]-(anne_surv-1)),annee_surv] = triangle[1:(dim(triangle)[1]-(anne_surv-1)),annee_surv]
	vec_test_CLy[1:(dim(triangle)[1]-(anne_surv-1)),annee_surv] = triangle[1:(dim(triangle)[1]-(anne_surv-1)),annee_surv]
#}



colonne_test = 2
vec_x = as.numeric(triangle[1:(dim(triangle)[1]-colonne_test),colonne_test])
vec_y = as.numeric(triangle[1:(dim(triangle)[1]-colonne_test),colonne_test+1])
data_vec = data.frame(ordo = vec_y,abs = vec_x)
data_vec

ggplot(data_vec,aes(x = data_vec[,1], y=data_vec[,2]))+
	geom_point(shape = 1)+
	geom_smooth(method=lm)+
	xlab("Paiements cumulés - Année J")+
	ylab("Paiements cumulés - Année J+1")+
	ggtitle("Vérification hypothèse Chain Ladder")

# -> L'hypothèse centrale de Chain Ladder semble vérifiée

CDR(chain_std1)


#######################################
## Faire une régression de Poisson ####
#######################################

#On va empiler les incréments de paiements en 1 vecteur
incr_emp = as.vector(as.matrix(incr))
ligne = as.factor(rep(1:dim(triangle)[2],  times = dim(triangle)[2]))
colonne = as.factor( rep(1:dim(triangle)[2], each = dim(triangle)[2]))

ligne
colonne

data_reg = data.frame(incr_emp, ligne, colonne)
reg_poiss1 = glm(incr_emp~ligne+colonne, data = data_reg, family = 'poisson')
summary(reg_poiss1)

dispersiontest(reg_poiss1)

reg_quasi_poiss = glm(incr_emp~ligne+colonne, data = data_reg, family = 'quasipoisson')
summary(reg_quasi_poiss)



#On recode pour les modalités non significatives
ligne_quasi = ligne
colonne_quasi = colonne

colonne_quasi[which(colonne == 12 | colonne == 14)] = "1"
ligne_quasi[which(ligne == 2 | ligne == 3 | ligne == 4 | ligne == 5 |ligne == 6)] = "1"

data_reg2 = data.frame(incr_emp, ligne_quasi, colonne_quasi)

reg_quasi_poiss2 = glm(incr_emp~ligne_quasi+colonne_quasi, data = data_reg2, family = 'quasipoisson')
summary(reg_quasi_poiss2)

reg_quasi_poiss3 = glm(incr_emp~ligne+colonne, data = data_reg2, family = 'quasipoisson')
summary(reg_quasi_poiss3)


# Neg binomiale

reg_nb = glm.nb(incr_emp~ligne+colonne, data = data_reg)
summary(reg_nb)


length(incr_emp)
length(pred_poiss1)

annee_new = rep(1:dim(triangle)[1],times = dim(triangle)[1])
annee_new

dvpt_new = rep(1:dim(triangle)[2],each = dim(triangle)[2])
dvpt_new

newbase = data.frame(as.factor(annee_new), as.factor(dvpt_new))

pred_poiss1 = predict(reg_poiss1, type = 'response', newdata = newbase)
summary(pred_poiss1)

pred_quasi_poiss = predict(reg_quasi_poiss, type = 'response', newdata = newbase)
summary(pred_quasi_poiss)

#On retrouve les mêmes résultats que sans surdispersion : c'est normal
prod(pred_poiss1 == pred_quasi_poiss)

annee_new2 = rep(1:dim(triangle)[1],times = dim(triangle)[1])
dvpt_new2 = rep(1:dim(triangle)[2],each = dim(triangle)[2])

dvpt_new2[which(dvpt_new == 12 | dvpt_new == 14)] = "1"
annee_new2[which(annee_new ==2 | annee_new == 3 | annee_new == 4 | annee_new == 5 | annee_new ==6)] = "1"

newbase2 = data.frame(as.factor(annee_new2), as.factor(dvpt_new2))
head(newbase2, 8)
pred_quasi_poiss2 = predict(reg_quasi_poiss2, type = 'response', newdata = newbase2)
summary(pred_quasi_poiss2)

pred_quasi_poiss3 = predict(reg_quasi_poiss3, type = 'response', newdata = newbase2)
summary(pred_quasi_poiss3)

prod(pred_quasi_poiss2 == pred_quasi_poiss3)

pred_nb = predict(reg_nb, type = 'response', newdata = newbase)
summary(pred_nb)

hist(pred_poiss1)
hist(pred_nb, add = T, color = 'blue')
type_pred = rep(0, length(pred_nb))



data_pred = data.frame(pred_poiss1, pred_quasi_poiss2, pred_nb)
names(data_pred)

ggplot(data_pred, aes(x = cbind(pred_poiss1, pred_quasi_poiss2, pred_nb))) + 
	geom_histogram(data_pred, aes(pred_poiss1), fill ='red')+
	geom_histogram(data_pred, aes(x = pred_quasi_poiss2), fill ='blue')+
	geom_histogram(data_pred, aes(x = pred_nb), fill ='green')


triangle_pred = matrix(pred_poiss1, dim(triangle)[1], dim(triangle)[2])
triangle_pred
incr

#Il faut alors prédire tous les incréments inconnus, puis sommer ligne par ligne les incréments inconnus
#On a alors l'incrément inconnu total ligne par ligne, puis en sommant ces incréments on obtient notre PSAP




#Contruisons triangle_pred2, avec les incréments connus pour moitié et les prédits pour autre moitié
triangle_pred2 = incr
for (i in 1:dim(triangle)[1]){
	for (j in 1:dim(triangle)[2]){
		if (is.na(incr[i,j])){
			triangle_pred2[i,j] = triangle_pred[i,j]
		}
	}
}
triangle_pred2
incr
triangle_pred

########
#Triangle prédit avec surdispersion et regroupement de modalités
########


triangle_pred_quasi2 = matrix(pred_quasi_poiss2, dim(triangle)[1], dim(triangle)[2])
triangle_pred_quasi2
incr


#Contruisons triangle_pred_quasi3, avec les incréments connus pour moitié et les prédits pour autre moitié
triangle_pred_quasi3 = incr
for (i in 1:dim(triangle)[1]){
	for (j in 1:dim(triangle)[2]){
		if (is.na(incr[i,j])){
			triangle_pred_quasi3[i,j] = triangle_pred_quasi2[i,j]
		}
	}
}
triangle_pred_quasi3
incr
triangle_pred


#Seconde tentative pour quasipoisson

triangle_pred_quasi22 = matrix(pred_quasi_poiss3, dim(triangle)[1], dim(triangle)[2])
triangle_pred_quas22
incr


#Contruisons triangle_pred_quasi3, avec les incréments connus pour moitié et les prédits pour autre moitié
triangle_pred_quasi32 = incr
for (i in 1:dim(triangle)[1]){
	for (j in 1:dim(triangle)[2]){
		if (is.na(incr[i,j])){
			triangle_pred_quasi32[i,j] = triangle_pred_quasi22[i,j]
		}
	}
}
triangle_pred_quasi32
incr
triangle_pred






## Negative binomiale

triangle_pred_nb = matrix(pred_nb, dim(triangle)[1], dim(triangle)[2])
triangle_pred_nb
incr

#Il faut alors prédire tous les incréments inconnus, puis sommer ligne par ligne les incréments inconnus
#On a alors l'incrément inconnu total ligne par ligne, puis en sommant ces incréments on obtient notre PSAP




#Contruisons triangle_pred2, avec les incréments connus pour moitié et les prédits pour autre moitié
triangle_pred_nb2 = incr
for (i in 1:dim(triangle)[1]){
	for (j in 1:dim(triangle)[2]){
		if (is.na(incr[i,j])){
			triangle_pred_nb2[i,j] = triangle_pred_nb[i,j]
		}
	}
}
triangle_pred_nb2
incr
triangle_pred_nb







#################################################
# Construction des vecteurs de paiements finaux #
#################################################

#====================#
# Pour Poisson 	   #
#====================#

vec_paiements_totaux = rep(1:dim(triangle)[1])
for (i in 1:dim(triangle)[1]){
	vec_paiements_totaux[i] = sum(triangle_pred2[i,])
}

vec_paiements_totaux
length(vec_paiements_totaux)


vec_reserve = rep(1:dim(triangle)[1])
for (i in 1: dim(triangle)[1]){
	vec_reserve[i] = vec_paiements_totaux[i] - triangle[i,dim(triangle)[2]-i+1]
}
vec_reserve
sum(vec_reserve)



# On essaie avec une quasi poisson et une binomiale négative pour voir la sensibilité de l'évualtion des IBNR


#=====================#
# Quasi Poisson 	    #
#=====================#

vec_paiements_totaux_quasi = rep(1:dim(triangle)[1])
for (i in 1:dim(triangle)[1]){
	vec_paiements_totaux_quasi[i] = sum(triangle_pred_quasi3[i,])
}

vec_paiements_totaux_quasi
length(vec_paiements_totaux_quasi)


vec_reserve_quasi = rep(1:dim(triangle)[1])
for (i in 1: dim(triangle)[1]){
	vec_reserve_quasi[i] = vec_paiements_totaux_quasi[i] - triangle[i,dim(triangle)[2]-i+1]
}
vec_reserve_quasi
sum(vec_reserve_quasi)


##############
# Avec modif #
##############


vec_paiements_totaux_quasi2 = rep(1:dim(triangle)[1])
for (i in 1:dim(triangle)[1]){
	vec_paiements_totaux_quasi2[i] = sum(triangle_pred_quasi32[i,])
}

vec_paiements_totaux_quasi2
length(vec_paiements_totaux_quasi2)


vec_reserve_quasi2 = rep(1:dim(triangle)[1])
for (i in 1: dim(triangle)[1]){
	vec_reserve_quasi2[i] = vec_paiements_totaux_quasi2[i] - triangle[i,dim(triangle)[2]-i+1]
}
vec_reserve_quasi2
sum(vec_reserve_quasi2)




#===============#
# Neg binomiale #
#===============#

vec_paiements_totaux_nb = rep(1:dim(triangle)[1])
for (i in 1:dim(triangle)[1]){
	vec_paiements_totaux_nb[i] = sum(triangle_pred_nb2[i,])
}

vec_paiements_totaux_nb
length(vec_paiements_totaux_nb)


vec_reserve_nb = rep(1:dim(triangle)[1])
for (i in 1: dim(triangle)[1]){
	vec_reserve_nb[i] = vec_paiements_totaux_nb[i] - triangle[i,dim(triangle)[2]-i+1]
}
vec_reserve_nb
sum(vec_reserve_nb)


#---------------------------------------------#
# 			Calcul des MSE 		    #
#---------------------------------------------#


#====================#
# Pour Poisson 	   #
#====================#




tri_cumul_poisson = triangle_pred
for (j in 2:dim(triangle)[2]){
	tri_cumul_poisson[,j] = tri_cumul_poisson[,j-1] + triangle_pred[,j]
}
tri_cumul_poisson
tri_MSE_poisson = triangle - tri_cumul_poisson



vec_res_poisson  = as.vector(as.matrix(tri_MSE_poisson))
vec_res_poisson = vec_res_poisson[-which(is.na(vec_res_poisson))]
vec_res_poisson
summary(vec_res_poisson)

hist(vec_res_poisson)

data_res_poisson = data.frame(vec_res_poisson)

ggplot(data_res_poisson, aes(x = data_res_poisson[,1])) + 
	geom_histogram(aes(y = ..density..),color ='blue', fill = 'blue')+
	geom_density(alpha = 0.5,fill = 'red')


ggplot(data_res_poisson, aes(x = data_res_poisson[,1])) + 
	geom_histogram(color ='blue', fill = 'sky blue')+
	xlab("Résidus")+
	ylab("Fréquence")+
	ggtitle("Histogramme des résidus avec une loi de Poisson")

tri_MSE_poisson
MSE_poisson = sum(tri_MSE_poisson^2, na.rm = T)
MSE_poisson
MSE_poisson/105
sqrt(MSE_poisson)
sqrt(MSE_poisson/105)




#===============#
# Neg binomiale #
#===============#


tri_cumul_nb = triangle_pred_nb
for (j in 2:dim(triangle)[2]){
	tri_cumul_nb[,j] = tri_cumul_nb[,j-1] + triangle_pred_nb[,j]
}
tri_cumul_nb
tri_MSE_nb = triangle - tri_cumul_nb
tri_MSE_nb
MSE_nb = sum(tri_MSE_nb^2, na.rm = T)
MSE_nb
MSE_nb/105
sqrt(MSE_nb)
sqrt(MSE_nb/105)


vec_res_nb  = as.vector(as.matrix(tri_MSE_nb))
vec_res_nb = vec_res_nb[-which(is.na(vec_res_nb))]
vec_res_nb

summary(vec_res_nb)

data_res_nb = data.frame(vec_res_nb)
ggplot(data_res_nb, aes(x = data_res_nb[,1])) + 
	geom_histogram(color ='blue', fill = 'sky blue')+
	xlab("Résidus")+
	ylab("Fréquence")+
	ggtitle("Histogramme des résidus avec une loi Négative Binomiale")




#################################################################
## On va utiliser le bootstrap pour obtneir notre quantile 99.5%
BCL1 = BootChainLadder ( Triangle = triangle, R = 999 , process.distr = "od.pois")
BCL1
plot(BCL1, title = "Bootstrap avec loi de Poisson")

BCL2 = BootChainLadder ( Triangle = triangle, R = 999 , process.distr = "gamma")
BCL2
plot(BCL2, title = "Bootstrap avec loi Gamma")




#################################################################
