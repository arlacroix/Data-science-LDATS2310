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
# Nbexposure<-base$Nbclaims/base$Exposure
# base <- data.frame(base,Nbexposure)

#On sépare les variables en plusieurs catégories
#Pour l'âge des véhicules on n'oublie pas de mettre include.lowest =TRUE afin de ne pas exclure les véhicules d'age 0 
# on peut aussi commencer le cut -1 mais cela est 
# seuilDriver      = c(15,28,31,35,44,51,61,100)
# seuilCar   = c(0,5,100)
# base$CarAge <- cut(base$CarAge, breaks = seuilCar, include.lowest = TRUE)
# base$DriverAge   <- cut(base$DriverAge, breaks = seuilDriver)


test <- read.table("/Users/lacroixarthur/OneDrive - UCL/Master Actu 2/Data science for finance /projet/DBtest.csv", sep=",", header=TRUE)

test$Gender<-as.factor(test$Gender)
test$Area<-  as.factor(test$Area)
test$Power<- as.factor(test$Power)
test$Leasing <- as.factor(test$Leasing)
test$Fract <- as.factor(test$Fract)
test$Contract <- as.factor(test$Contract)

library(rpart)

M    = 100         #number of samples
nr   = nrow(base)  #size of the dataset
size = nr          #size of the sample
lambda =rep(0,nrow(test))
lambdaM <- matrix(0 , M,nrow(test))
ageM    <- matrix(0 , M,nrow(test))

d.treeInit <-rpart( cbind(Exposure,Nbclaims)~DriverAge+Gender+Area+Power+
                      CarAge+Fract+Leasing+Contract,
                    data=base, method="poisson", parms=list(shrink=1),
                    control=rpart.control(cp=0.001))

dstar = 2;
listcovariates=c("DriverAge","Gender","Area","Power",
                 "CarAge","Contract", "Leasing", "Fract" )

# random forest (non parametric)
for (ct in c(1:M))
{
  #non parametrique
  fit <-predict(d.treeInit,base)
  Nsim<-rpois(nr,fit*base$Exposure)
  basetmp             <-base	
  basetmp$NumberClaims<-Nsim
  
  rndFact <-sample(6, dstar, replace = FALSE, prob = NULL)
  
  equation=paste("cbind(Exposure,Nbclaims)~",listcovariates[rndFact[1]])
  t=equation
  for (j in c(2:dstar)){	
    t=paste(t,listcovariates[rndFact[j]],sep="+")
  }
  
  d.tree <-rpart( equation,data=basetmp, method="poisson",
                  parms=list(shrink=1),control=rpart.control(cp=0.0001))
  
  # if (!is.null(d.tree$csplit))
  # {
  #   plot(d.tree,main = "Regression tree, small d")
  #   text(d.tree) #equivalent to labels(d.tree)
  # }
  lambda<-lambda+predict(d.tree,test)
  lambdaM[ct,]<-lambda
  ageM[ct,]   <-test$DriverAge
  
  #readline("Press <return to continue") 
}

lambda<-lambda/M












