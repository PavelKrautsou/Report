mod_data$Survived = factor(mod_data$Survived)
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

results <- data.frame(acc = numeric(),formula = character())
results = list()
for(formula in 1:length(formulas)){
      tune.out=tune(svm , formulas[[formula]], data=mod_data, kernel ="polynomial",scale = TRUE,
                  ranges=list(cost=c(0.5,1,2,2.5,3),
                              degree=c(2,3,4,5,6) ))
      print(tune.out$best.model)
      print(round(1 - tune.out$best.performance,4))
}