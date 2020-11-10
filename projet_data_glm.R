
# On importe la base de données, attention au séparateur du fichier csv. 
base <- read.table("/Users/lacroixarthur/OneDrive - UCL/Master Actu 2/Data science for finance /projet/DBtrain.csv", sep=",", header=TRUE)

# on définit la durée minimale et maximale des contrats des assurés.
# la fonction floor permet d'arrondir vers le bas (peu importe le premier chiffre décimal) ceiling arrondi de la même manière mais vers le haut
minExposure <- floor(min(base$Exposure, na.rm = TRUE))
maxExposure <- ceiling(max(base$Exposure, na.rm = TRUE))
nExposure   <-  (maxExposure-minExposure)+1
Exposure    <- c(minExposure:maxExposure)

base$Gender <-as.factor(base$Gender)
base$Area <-  as.factor(base$Area)
base$Power <- as.factor(base$Power)
base$Leasing <- as.factor(base$Leasing)
base$Fract <- as.factor(base$Fract)
base$Contract <- as.factor(base$Contract)


# Ici, nous n'avons pas de Exposure nulles, si jamais nous en avons, nous devons les supprimer de cette façon :

# base<-base[is.na(base$Exposure)==0,]
# base<-base[base$Exposure>0,]
# range(base$Exposure)

# on rajoute la fréquence des sinistres dans la base de données 
Nbexposure<-base$Nbclaims/base$Exposure
base <- data.frame(base,Nbexposure)

#On sépare les variables en plusieurs catégories
#Pour l'âge des véhicules on n'oublie pas de mettre include.lowest =TRUE afin de ne pas exclure les véhicules d'age 0 
# on peut aussi commencer le cut -1 
seuilDriver      = c(15,28,31,35,44,51,61,100)
seuilCar   = c(0,5,100)
base$CarAge <- cut(base$CarAge, breaks = seuilCar, include.lowest = TRUE)
base$DriverAge   <- cut(base$DriverAge, breaks = seuilDriver)

#On fit un modèle glm 
modnbclaims<-glm(Nbclaims~DriverAge+Gender+Area+Power+
                   CarAge+Fract+Leasing+Contract,data=base, offset = log(Exposure), family=poisson(link="log"))

#Summary
summary(modnbclaims)
# slow but OK
##confint(modnbclaims)

#Recalculation of the deviance
deviance(modnbclaims)

#Permet de connaître l'AIC du modele 
modlightclaims<-step(modnbclaims)

test <- read.table("/Users/lacroixarthur/OneDrive - UCL/Master Actu 2/Data science for finance /projet/DBtest.csv", sep=",", header=TRUE)

test$Gender<-as.factor(test$Gender)
test$Area<-  as.factor(test$Area)
test$Power<- as.factor(test$Power)
test$Leasing <- as.factor(test$Leasing)
test$Fract <- as.factor(test$Fract)
test$Contract <- as.factor(test$Contract)


test$CarAge <- cut(test$CarAge, breaks = seuilCar,include.lowest = TRUE)
test$DriverAge   <- cut(test$DriverAge, breaks = seuilDriver)
lambda<-exp(predict(modlightclaims,test))
#tous les lambda (pour chaque catégorie) sont contenus dans un seul vecteur lambda pour voir le nombre d'occurance de ce vecteur,
#on peut utiliser la fonction table(lambda) et on vérifie que ces différents lambda appraissent bien plusieurs fois. Cela a du sens
#car il existe plusisuers personnes dans, par exemple, la catégorie femme,(15,28),(0,6),Area=4,... à chaque lambda correspond une 
#catégorie particulière et il y en a 448 au total. 
#Pour connaitre le nombre de sinistres espérer, il suffit de sommer tous les lambda avec sum(lambda), nous obtenons ici le nombre de 2049,29
#On voit alors que cela fait 6,83% du portefeuille soit moins de 10% et que globalement la distribution de Poisson est justifiée dans notre cas. 


# on a pas de données pour prédire le cout moyen des sinsitres... 

# On doit rajouter une colonne "frequence des sinsitres" dans la table test qui sera donnée par lambda/Exposure ainisi qu'un colonne 
#number claims qui sera donné par lambda

NbClaims <- lambda
test <- data.frame(test,NbClaims)

NbExposure<-lambda/test$Exposure
test <- data.frame(test,NbExposure)

########## Calculer la cross validation peut etre avec cv.glm ??? 

library(boot)
cost <- function(r, pi = 0) mean(abs(r-pi))
tmp<-cv.glm(data=base,modnbclaims,cost,K=10)
tmp$delta 
tmp<-cv.glm(data=base,modnbclaims,K=10)
tmp$delta 

#Le vecteur delta a 2 composantes, la première est l'estimation de l'erreur de la cross validation et la 2e est l'estimation de la 
#cross validation mais ajustéé






