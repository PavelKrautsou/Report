setwd("C:/titanic")
require(plyr)
require(gbm)
train <- read.csv("train.csv")
mod_data <- select(train,
                   Survived,
                   Sex,
                   Pclass,
                   Age,
                   SibSp,
                   Parch,
                   Fare
)

#tuning parameters
trees_to_use <- c(100,500,1000,2000,3000,4000)
lambdas <- c(0.01,0.03, 0.1)
depths <- c(1,2,3,4)
# Number of folds
k =10
n = floor(nrow(mod_data)/k)
results <- data.frame(acc = numeric(),trees = numeric(),
                       shrinkage = numeric(), depth = numeric())

for( tree in trees_to_use){
      for(lambda in lambdas){ 
         for(depth in depths){
             rec = rep(NA,k)
             for(i in 1:k){
                
                s1 = ((i - 1) * n+1) 
                s2 = (i * n)       
                subset = s1:s2
                cv.train = mod_data[-subset,] 
                cv.test = mod_data[subset,]

                fit = gbm(Survived~., data = cv.train, 
                                          interaction.depth = depth,
                                          shrinkage = lambda,
                                          n.trees = tree,
                                          verbose = FALSE,
                                          distribution = "bernoulli")
                prediction = predict(fit, newdata = cv.test[,-1],n.trees = tree,
                type= "response")
                tp <- sum(round(prediction) == cv.test[,1]& cv.test[,1] == 1)
                fn <- sum(round(prediction) == 0 & cv.test[,1] == 1)
                #prec[i] <- tp /sum(round(prediction) == 1)
                rec[i] <- tp /(tp + fn)
                #F[i] <- 2 * ((prec*rec)/(prec+rec))

                 
                               
}
acc <- mean(rec)

temp <- list(acc = acc,trees =tree, shrinkage =lambda,depth =depth)
results = rbind(results, temp)
}}}
results
                                      
