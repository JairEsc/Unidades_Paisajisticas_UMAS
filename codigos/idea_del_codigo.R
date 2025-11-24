
##El código no apareció. 

##La idea del código es definir el número de grupos usando criterios de información Bayesiana. 

##Nos salieron 15 así que se ejecuta un clustering gaussiano (mclust)


install.packages('mclust')
library(mclust)

mclust::mclustBIC()
mclust::Mclust()
