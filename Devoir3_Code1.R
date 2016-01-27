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


# Ultimate Loss and Tail Factor
chain_std2 = MackChainLadder(triangle, tail = T)
chain_std2


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



colonne_test = 8
vec_x = as.numeric(triangle[1:(dim(triangle)[1]-colonne_test),colonne_test])
vec_y = as.numeric(triangle[1:(dim(triangle)[1]-colonne_test),colonne_test+1])
data_vec = data.frame(ordo = vec_y,abs = vec_x)
data_vec

ggplot(data_vec,aes(x = data_vec[,1], y=data_vec[,2]))+
	geom_point(shape = 1)+
	geom_smooth(method=lm)

# -> L'hypothèse centrale de Chain Ladder semble vérifiée

CDR(chain_std1)


#######################################
## Faire une régression de Poisson ####
#######################################

#On va empiler les incréments de paiements en 1 vecteur
incr_emp = as.vector(as.matrix(incr))
ligne = as.factor(rep(1:dim(triangle)[2], each = dim(triangle)[2]))
colonne = as.factor( rep(1:dim(triangle)[2], dim(triangle)[2]))

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

ligne_quasi[which(ligne == 12 | ligne == 14)] = "1"
colonne_quasi[which(colonne == 2 | colonne == 3 | colonne == 4 | colonne == 5 |colonne == 6)] = "1"

reg_quasi_poiss2 = glm(incr_emp~ligne_quasi+colonne_quasi, data = data_reg, family = 'quasipoisson')
summary(reg_quasi_poiss2)


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

pred_quasi_poiss2 = predict(reg_quasi_poiss2, type = 'response', newdata = newbase)
summary(pred_quasi_poiss2)

pred_nb = predict(reg_nb, type = 'response', newdata = newbase)


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







#################################################################
## On va utiliser le bootstrap pour obtneir notre quantile 99.5%
BCL1 = BootChainLadder ( Triangle = triangle, R = 999 , process.distr = "od.pois")
BCL1
plot(BCL1)
#################################################################
