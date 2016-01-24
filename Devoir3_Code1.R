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

chain_std1 = MackChainLadder(triangle)
chain_std1

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



colonne_test = 12
vec_x = as.numeric(triangle[1:(dim(triangle)[1]-colonne_test),colonne_test])
vec_y = as.numeric(triangle[1:(dim(triangle)[1]-colonne_test),colonne_test+1])
data_vec = data.frame(ordo = vec_y,abs = vec_x)
data_vec

ggplot(data_vec,aes(x = data_vec[,1], y=data_vec[,2]))+
	geom_point(shape = 1)+
	geom_smooth(method=lm)

# -> L'hypothèse centrale de Chain Ladder semble vérifiée