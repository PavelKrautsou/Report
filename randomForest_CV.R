ms <- c(1,2,3,4,5,6)
k =10
n = floor(nrow(mod_data)/k)
results <- data.frame(acc = numeric(),m = numeric())

for(m in ms){
  acc.vect = rep(NA,k)
    for(i in 1:k){
    s1 = ((i - 1) * n+1) 
    s2 = (i * n)       
    subset = s1:s2
    cv.train = mod_data[-subset,] 
    cv.test = mod_data[subset,]
    fit = randomForest(factor(Survived)~. , mtry = m, data =cv.train)
    prediction = predict(fit, newdata = cv.test[,-1])
    acc.vect[i] <- sum(prediction == cv.test[,1])/nrow(cv.test)
  }
  acc <- mean(acc.vect)
  temp <- list(acc = acc,m= m)
  results = rbind(results, temp)
}

mod <- randomForest(factor(Survived)~., mtry =2, data = mod_data,importance=TRUE)
varImpPlot(mod)
