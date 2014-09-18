
train <- read.csv("train.csv", na.strings = c("NA", ""))
train$Survived = factor(train$Survived)
train$Pclass = factor(train$Pclass)
test$Pclass = factor(test$Pclass)
require(plyr)
mod_data = select(train,
               Survived,
               Sex,
               Pclass,
               Age,
               SibSp,
               Parch,
               Fare)

formulas <- list(Survived~.+ Pclass:Age +Parch:Age + SibSp:Sex,
                 Survived~.+ Pclass:Age +Parch:Age,
                 Survived~.+ Parch:Age + SibSp:Sex,
                 Survived~.+ Pclass:Age + SibSp:Sex,
                 Survived~.+ Pclass:Age,
                 Survived~.+ SibSp:Sex,
                 Survived~.+ Parch:Age,
                 Survived~.,
                 Survived~.-Parch,Survived~.-Parch - SibSp,
                 Survived~.-Parch,Survived~.-Parch - SibSp -Fare)
k =10
n = floor(nrow(mod_data)/k)
results <- data.frame(acc = numeric(),formula = character())

for(formula in 1:length(formulas)){
  acc.vect = rep(NA,k)
  for(i in 1:k){
    s1 = ((i - 1) * n+1) 
    s2 = (i * n)       
    subset = s1:s2
    cv.train = mod_data[-subset,] 
    cv.test = mod_data[subset,]
    fit = glm(formulas[[formula]], family = binomial, data =cv.train)
    prediction = predict(fit, newdata = cv.test[,-1],type= "response")
    acc.vect[i] <- sum(round(prediction) == cv.test[,1])/nrow(cv.test)
  }
  acc <- mean(acc.vect)
  temp <- list(acc = acc,formula = formula)
  results = rbind(results, temp)
}

results