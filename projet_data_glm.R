
# On importe la base de données, attention au séparateur du fichier csv. 
base <- read.table("/Users/lacroixarthur/OneDrive - UCL/Master Actu 2/Data science for finance /projet/DBtrain.csv", sep=",", header=TRUE)

# on définit la durée minimale et maximale des contrats des assurés.
# la fonction floor permet d'arrondir vers le bas (peu importe le premier chiffre décimal) ceiling arrondi de la même manière mais vers le haut
minExposure <- floor(min(base$Exposure, na.rm = TRUE))
maxExposure <- ceiling(max(base$Exposure, na.rm = TRUE))
nExposure   <-  (maxExposure-minExposure)+1
Exposure    <- c(minExposure:maxExposure)
s1=rep(0,nExposure)
for (k in 1: nrow(base))
{
  j = floor(base$Exposure[k])
  s1[j] <- s1[j]+1
}

minAge <- floor(min(base$CarAge, na.rm = TRUE))
maxAge <- ceiling(max(base$CarAge, na.rm = TRUE))
nAge   <- (maxAge-minAge)+1
Age    <- c(minAge:maxAge)
s2=rep(0,nAge)
for (k in 1: nrow(base))
{
  j = floor(base$CarAge[k])
  s2[j] <- s2[j]+1
}

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

# on rajoute la fréquance des sinistres dans la base de données 
Nbexposure<-base$Nbclaims/base$Exposure
base <- data.frame(base,Nbexposure)

#On sépare les variables en plusieurs catégories
seuilDriver      = c(15,28,31,35,44,51,61,100)
seuilCar   = c(-1,5,100)
base$CarAge <- cut(base$CarAge, breaks = seuilCar)
base$DriverAge   <- cut(base$DriverAge, breaks = seuilDriver)

#On fit un modèle glm 
# prooblème avec le offset du prof... pas les mêmes longueurs... 
#en prenant l'offset par défaut ca fonctionne 
modnbclaims<-glm(Nbclaims~DriverAge+Gender+Area+Power+
                   CarAge+Fract+Leasing+Contract,data=base, offset = log(Exposure), family=poisson(link="log"))

#Summary
summary(modnbclaims)
# slow but OK
##confint(modnbclaims)

#Recalculation of the deviance
deviance(modnbclaims)
modlightclaims<-step(modnbclaims)

test <- read.table("/Users/lacroixarthur/OneDrive - UCL/Master Actu 2/Data science for finance /projet/DBtest.csv", sep=",", header=TRUE)

test$Gender<-as.factor(test$Gender)
test$Area<-  as.factor(test$Area)
test$Power<- as.factor(test$Power)
test$Leasing <- as.factor(test$Leasing)
test$Fract <- as.factor(test$Fract)
test$Contract <- as.factor(test$Contract)


test$CarAge <- cut(test$CarAge, breaks = seuilCar)
test$DriverAge   <- cut(test$DriverAge, breaks = seuilDriver)
lambda<-exp(predict(modlightclaims,test))
#tous les lambda (pour chaque catégorie) sont contenus dans un seul vecteur lambda pour voir le nombre d'occurance de ce vecteur,
#on peut utiliser la fonction table(lambda) et on vérifie que ces différents lambda appraissent bien plusieurs fois. Cela a du sens
#car il existe plusisuers personnes dans, par exemple, la catégorie femme,(15,28),(0,6),Area=4,... à chaque lambda correspond une 
#catégorie particulière et il y en a 448 au total. 

# on a pas de données pour prédire le cout moyen des sinsitres... 





