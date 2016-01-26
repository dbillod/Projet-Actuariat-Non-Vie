library(readODS)
library(ChainLadder)
library(ggplot2)

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

#Il faut alors prédire tous les incréments inconnus, puis sommer ligne par ligne les incréments inconnus
#On a alors l'incrément inconnu total ligne par ligne, puis en sommant ces incréments on obtient notre PSAP

#################################################################
## On va utiliser le bootstrap pour obtneir notre quantile 99.5%
BCL1 = BootChainLadder ( Triangle = triangle, R = 999 , process.distr = "od.pois")
BCL1
plot(BCL1)
#################################################################
