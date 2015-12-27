
##---------------------------##
##	Modèle 2 : Reg binom   ##
##---------------------------##


reg_neg_bin1 = glm.nb(Nb1~
Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
+offset(log(Exppdays)),
data = db1a)

summary(reg_neg_bin1)


##On fait varier le paramètre alpha

reg_neg_bin1 = glm(Nb1~
Gender+Type+Occupation+Age+Bonus+Poldur+Density+(Group1)+Group2+Adind
+offset(log(Exppdays)),
data = db1a, family = negative.binomiale())

summary(reg_neg_bin1)


##---------------------------##
##	Modèle 2 : Zero Infl   ##
##---------------------------##



reg_zero_inf1 = zeroinfl(Nb1~
Gender|Type|Occupation|Age|Bonus|Poldur|Density|(Group1)|Group2|Adind
offset = (log(Exppdays)),
data = db1a, dist = 'poisson', link = 'logit')
