# Random forest code
library(caret)
library(rpart)
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


test <- read.table("/Users/lacroixarthur/OneDrive - UCL/Master Actu 2/Data science for finance/projet/DBtest.csv", sep=",", header=TRUE)

test$Gender<-as.factor(test$Gender)
test$Area<-  as.factor(test$Area)
test$Power<- as.factor(test$Power)
test$Leasing <- as.factor(test$Leasing)
test$Fract <- as.factor(test$Fract)
test$Contract <- as.factor(test$Contract)

set.seed(2)
inValidation =  sample(nrow(base), 0.9*nrow(base), replace = FALSE)
training.set = base[inValidation,]
validation.set = base[-inValidation,]


# test$CarAge <- cut(test$CarAge, breaks = seuilCar,include.lowest = TRUE)
# test$DriverAge   <- cut(test$DriverAge, breaks = seuilDriver )


library(rpart)

M    = 100        #number of samples
nr   = nrow(base)  #size of the dataset
size = nr          #size of the sample
lambda3 <- rep(0,nrow(validation.set))
lambda3M <- matrix(0 , M,nrow(validation.set))
ageM    <- matrix(0 , M,nrow(validation.set))

dstar = 2;
listcovariates=c("DriverAge","Gender","Area","Power",
                 "CarAge","Contract", "Leasing", "Fract" )

# random forest (non parametric)
for (ct in c(1:M))
{
  #non parametrique
  tmp     <-sample(nr, size, replace = TRUE, prob = NULL)
  basetmp <-training.set[tmp,]	
  
  rndFact <-sample(6, dstar, replace = FALSE, prob = NULL)
  
  equation=paste("cbind(Exposure,Nbclaims)~",listcovariates[rndFact[1]])
  for (j in c(2:dstar)){	
    equation=paste(equation,listcovariates[rndFact[j]],sep="+")
    
  }
  
  d.tree <-rpart( equation,data=basetmp, method="poisson",
                  parms=list(shrink=1),control=rpart.control(cp=0, xval = 10))
  inValidation = createDataPartition(base$Nbclaims, p=0.1, list=FALSE) 
  validation.set = base[inValidation,]
  training.set = base[-inValidation,]
  # if (!is.null(d.tree$csplit))
  # {
  # plot(d.tree,main = "Regression tree, small d")
  #   text(d.tree) #equivalent to labels(d.tree)
  # }
  
  lambda3<-lambda3+predict(d.tree,validation.set)
  lambda3M[ct,]<-lambda3
  ageM[ct,]   <- validation.set$DriverAge
  
  #readline("Press <return to continue")
}
lambda3 <- lambda3/M

# NbClaims <- lambda3 * (test$Exposure)
# test <- data.frame(test,NbClaims)
# 
# 
# NbExposure <- lambda3 
# test <- data.frame(test,NbExposure)

valid.predrf <- validation.set$Exposure*predict(d.tree, newdata=validation.set) 
pred.errorrf <- 1/nrow(validation.set)*2*(sum(valid.predrf)-sum(validation.set$Nbclaims) +sum(log((validation.set$Nbclaims/valid.predrf)^(validation.set$Nbclaims))))

#On peut alors prendre un cp optimal tel que le xerror est minimum
#On doit refaire un boucle et prendre le cp optimal pour chaque nouvel arbre créer. Nous voyons qu'il y a très peu de différence entre les deux 
#forêts. Il n'est donc pas utile d'essayer d'optimiser le cp. 
# for (ct in c(1:M))
# {
#   #non parametrique
#   cp.opt <- d.tree$cptable[which.min(d.tree$cptable[,"xerror"]),"CP"]
#   tmp     <-sample(nr, size, replace = TRUE, prob = NULL)
#   basetmp <-training.set[tmp,]	
#   
#   rndFact <-sample(6, dstar, replace = FALSE, prob = NULL)
#   
#   equation=paste("cbind(Exposure,Nbclaims)~",listcovariates[rndFact[1]])
#   for (j in c(2:dstar)){	
#     equation=paste(equation,listcovariates[rndFact[j]],sep="+")
#     
#   }
#   d.tree <-rpart(equation,data=basetmp, method="poisson",
#                  parms=list(shrink=1),control=rpart.control(cp=0, xval=10))
#   
#   cp.opt <- d.tree$cptable[which.min(d.tree$cptable[,"xerror"]),"CP"]
#   
#   d.tree_opt <-rpart( equation,data=basetmp, method="poisson",
#                   parms=list(shrink=1),control=rpart.control(cp=cp.opt))
#   inValidation = createDataPartition(base$Nbclaims, p=0.1, list=FALSE) 
#   validation.set = base[inValidation,]
#   training.set = base[-inValidation,]
#   # if (!is.null(d.tree$csplit))
#   # {
#   # plot(d.tree,main = "Regression tree, small d")
#   #   text(d.tree) #equivalent to labels(d.tree)
#   # }
#   
#   lambda3<-lambda3+predict(d.tree,validation.set)
#   lambda3M[ct,]<-lambda3
#   ageM[ct,]   <- validation.set$DriverAge
#   
#   #readline("Press <return to continue")
# }

pred_rf <- predict(d.tree, newdata = validation.set) * validation.set$Exposure
dev_mean_rf <- calc.deviance(validation.set$Nbclaims, pred_rf, family = "poisson")
dev_rf2 <- calc.deviance(validation.set$Nbclaims, pred_rf, family = "poisson", calc.mean = FALSE)

# Calcul de la déviance Poisson pour la forêt optimisé, très peu de différence et temps de calcul beacoup plus long. 
# pred_rf_opt <- predict(d.tree_opt, newdata = validation.set) * validation.set$Exposure
# dev_mean_rf_opt <- calc.deviance(validation.set$Nbclaims, pred_rf_opt, family = "poisson")
# dev_rf_opt2 <- calc.deviance(validation.set$Nbclaims, pred_rf_opt, family = "poisson", calc.mean = FALSE)

