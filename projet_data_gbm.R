base <- read.table("/Users/lacroixarthur/OneDrive - UCL/Master Actu 2/Data science for finance/projet/DBtrain.csv", sep=",", header=TRUE)

base$Gender <-as.factor(base$Gender)
base$Area <-  as.factor(base$Area)
base$Power <- as.factor(base$Power)
base$Leasing <- as.factor(base$Leasing)
base$Fract <- as.factor(base$Fract)
base$Contract <- as.factor(base$Contract)



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
validation.set = base[train,]
training.set = base[-train,]
# Ici, nous n'avons pas de Exposure nulles, si jamais nous en avons, nous devons les supprimer de cette façon :

# base<-base[is.na(base$Exposure)==0,]
# base<-base[base$Exposure>0,]
# range(base$Exposure)

library(gbm)

gbm1<-gbm(Nbclaims~offset(log(Exposure))+DriverAge+Gender+Area+Power+
               CarAge+Leasing+Fract+Contract, distribution="poisson", data=training.set,
             shrinkage=0.001,n.trees=50000, cv.folds = 10)

summary(gbm1)
print(gbm1)
gbm.perf(gbm1, method = "cv") # nombre d'iteration otpimal = 19754
# plot.gbm(gbm.fit,i.var="DriverAge")
# plot.gbm(gbm.fit,i.var="Gender")
# plot.gbm(gbm.fit,i.var="CarAge")
# plot.gbm(gbm.fit,i.var="Area")
# plot.gbm(gbm.fit,i.var="Power")
# plot.gbm(gbm.fit,i.var="Leasing")
# plot.gbm(gbm.fit,i.var="Fract")
# plot.gbm(gbm.fit,i.var="Contract")


pred_gbm1 <- exp(predict(gbm1, newdata = validation.set))
dev_mean_gbm1 <- calc.deviance(validation.set$Nbclaims, pred_gbm1, family = "poisson")
dev_gbm1 <- calc.deviance(validation.set$Nbclaims, pred_gbm1, family = "poisson", calc.mean = FALSE)





