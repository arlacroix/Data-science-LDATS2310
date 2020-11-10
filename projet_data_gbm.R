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


test <- read.table("/Users/lacroixarthur/OneDrive - UCL/Master Actu 2/Data science for finance /projet/DBtest.csv", sep=",", header=TRUE)

test$Gender<-as.factor(test$Gender)
test$Area<-  as.factor(test$Area)
test$Power<- as.factor(test$Power)
test$Leasing <- as.factor(test$Leasing)
test$Fract <- as.factor(test$Fract)
test$Contract <- as.factor(test$Contract)

library(gbm)

gbm.fit<-gbm(Nbclaims~offset(log(Exposure))+DriverAge+Gender+Area+Power+
               CarAge+Leasing+Fract+Contract, distribution="poisson", data=base,
             shrinkage=0.001,n.trees=5000, cv.folds = 10)

summary(gbm.fit)
#par(mfrow=c(2,2))
plot.gbm(gbm.fit,i.var="DriverAge")
plot.gbm(gbm.fit,i.var="Gender")
plot.gbm(gbm.fit,i.var="CarAge")
plot.gbm(gbm.fit,i.var="Area")
plot.gbm(gbm.fit,i.var="Power")
plot.gbm(gbm.fit,i.var="Leasing")
plot.gbm(gbm.fit,i.var="Fract")
plot.gbm(gbm.fit,i.var="Contract")



lambda<-exp(predict.gbm(gbm.fit,test,n.trees=1000)) 
plot.gbm(gbm.fit)

#plot(test$DriverAge, exp(lambda),type="l",col=c("red"),main="Expected Lambda")









