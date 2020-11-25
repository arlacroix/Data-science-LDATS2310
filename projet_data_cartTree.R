# Tree
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tree)
library(rpart.plot)
library(dismo)
base <- read.table("/Users/lacroixarthur/OneDrive - UCL/Master Actu 2/Data science for finance/projet/DBtrain.csv", sep=",", header=TRUE)

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
seuilDriver      = c(15,28,31,35,44,51,61,100)
seuilCar   = c(0,5,100)
base$CarAge <- cut(base$CarAge, breaks = seuilCar, include.lowest = TRUE)
base$DriverAge   <- cut(base$DriverAge, breaks = seuilDriver)

set.seed(2)
inValidation =  sample(nrow(base), 0.9*nrow(base), replace = FALSE)
training.set = base[inValidation,]
validation.set = base[-inValidation,]

# On fit un modèle pour la fréquence des sinsitres (nbclaimss/Exposure) 
tree <- rpart( cbind(Exposure,Nbclaims)~DriverAge+Gender+Area+Power+
         CarAge+Leasing+Contract+Fract,
       data=training.set, method="poisson", parms=list(shrink=1),
       control=rpart.control(cp=0))

printcp(tree)
#On commence par choisir un arbre trop complexe, où nous avons un problème d'overfitting. On trouve ensuite le cp optimal, cad le cp pour lequel
# xerror dans la table printcp est le plus petit. Il s'agit ici d'un arbre ayant 5 noeuds. 

cp.opt <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

tree_opt <- rpart( cbind(Exposure,Nbclaims)~DriverAge+Gender+Area+Power+
                   CarAge+Leasing+Contract+Fract,
                 data=training.set, method="poisson", parms=list(shrink=1),
                 control=rpart.control(cp=cp.opt))

prp(tree_opt)

# la fonction prp du package rpart.plot nous donne une version plus intuitive de l'arbre construit. 

pred_rt <- predict(tree, newdata = validation.set )* validation.set$Exposure
dev_mean_rt <- calc.deviance(validation.set$Nbclaims, pred_rt, family = "poisson")
dev_rt2 <- calc.deviance(validation.set$Nbclaims, pred_rt, family = "poisson", calc.mean = FALSE)


pred_rt_opt <- predict(tree_opt, newdata = validation.set) * validation.set$Exposure
dev_mean_rt_opt <- calc.deviance(validation.set$Nbclaims, pred_rt_opt, family = "poisson")
dev_rt_opt2 <- calc.deviance(validation.set$Nbclaims, pred_rt_opt, family = "poisson", calc.mean = FALSE)

# On voit grace à la commande rsq.rpart(d.tree_opt) que le xerror ne décroit pas 
# significativement avec l'augmentation du cp. Cela nous laisse penser que cet arbre
# n'est pas fiable. 
















