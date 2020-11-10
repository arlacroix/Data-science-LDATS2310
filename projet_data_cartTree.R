# Tree

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

# on rajoute la fréquance des sinistres dans la base de données 
Nbexposure<-base$Nbclaims/base$Exposure
base <- data.frame(base,Nbexposure)

#On sépare les variables en plusieurs catégories
#Pour l'âge des véhicules on n'oublie pas de mettre include.lowest =TRUE afin de ne pas exclure les véhicules d'age 0 
# on peut aussi commencer le cut -1 mais cela est 
seuilDriver      = c(15,28,31,35,44,51,61,100)
seuilCar   = c(0,5,100)
base$CarAge <- cut(base$CarAge, breaks = seuilCar, include.lowest = TRUE)
base$DriverAge   <- cut(base$DriverAge, breaks = seuilDriver)


test <- read.table("/Users/lacroixarthur/OneDrive - UCL/Master Actu 2/Data science for finance /projet/DBtest.csv", sep=",", header=TRUE)

test$Gender<-as.factor(test$Gender)
test$Area<-  as.factor(test$Area)
test$Power<- as.factor(test$Power)
test$Leasing <- as.factor(test$Leasing)
test$Fract <- as.factor(test$Fract)
test$Contract <- as.factor(test$Contract)


test$CarAge <- cut(test$CarAge, breaks = seuilCar,include.lowest = TRUE)
test$DriverAge   <- cut(test$DriverAge, breaks = seuilDriver )


library(rpart)
library(rpart.plot)
# On fit un modèle pour la fréquence des sinsitres (nbclaimss/Exposure) 
d.tree <- rpart( cbind(Exposure,Nbclaims)~DriverAge+Gender+Area+Power+
         CarAge+Leasing+Contract+Fract,
       data=base, method="poisson", parms=list(shrink=1),
       control=rpart.control(cp=0.000001, xval=10))

printcp(d.tree)
lambda<-predict(d.tree,test)
#On commence par choisir un arbre trop complexe, où nous avons un problème d'overfitting. On trouve ensuite le cp optimal, cad le cp pour lequel
# xerror dans la table printcp est le plus petit. Il s'agit ici d'un arbre ayant 5 noeuds. 

cp.opt <- d.tree$cptable[which.min(d.tree$cptable[,"xerror"]),"CP"]

d.tree_opt <- rpart( cbind(Exposure,Nbclaims)~DriverAge+Gender+Area+Power+
                   CarAge+Leasing+Contract+Fract,
                 data=base, method="poisson", parms=list(shrink=1),
                 control=rpart.control(cp=cp.opt, xval=10))

prp(d.tree_opt)

lambda_opt <- predict(d.tree_opt,test)
# la fonction prp du package rpart.plot nous donne une version plus intuitive de l'arbre construit. 

# On créé 2 nouvelles colonnes, une avec le fréquence des sinistres et une avec le nombre espérer de sinistres en fonction de l'individu.

NbClaims <- lambda_opt * (test$Exposure)
test <- data.frame(test,NbClaims)

nc <- sum(test$NbClaims)

NbExposure <- lambda_opt 
test <- data.frame(test,NbExposure)

# Comment comparer le modèle glm et le modèle Cart Tree ? avec la cross validtion, on la connait pour le glm mais avec le vecteur delta.
# Pour le modèle Cart Tree on la connait peut-être grace à la fonction printcp ce serait le xerror mais elle est bcp plus grande que pour 
# le modèle glm, une telle différence est-elle normale ? 


# On voit grace à la commande rsq.rpart(d.tree_opt) que le xerror ne décroit pas 
# significativement avec l'augmentation du cp. Cela nous laisse penser que cet arbre
# n'est pas fiable. 
















