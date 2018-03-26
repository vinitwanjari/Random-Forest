library('randomForest')

data = read.csv('breast-cancer.csv')  #breast cancer data
x = data[,2:11]
y = as.factor(as.vector(data[,1])) 
data = cbind(x, y)
fold = cut(seq(1,nrow(data)),breaks = 10, labels = FALSE)  #10 fold
accuracy = c()
for (i in 1:10)
{
  index = which(fold==i)
  train = data[-index,]
  test = data[index,]
  dt = randomForest(y~.,data = train, method = "class")   #decision tree model
  predicted = predict(dt, test, type = "class")   #predict test data
  actual = test[,11]   #oriinal class
  accuracy[i] = length(which(actual == predicted)) / length(actual) 
  #cat("accuracy:",accuracy,"\n")
  tab = table(predicted, actual)
  TN <- tab[1,1]
  FP <- tab[2,2]
  FN <- tab[2,1]
  TP <- tab[1,2]
 
  sensitivity = (TP/(TP + FN))
  specificity = (TN/(FP + TN))
  MCC = ((TP*TN) - (FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  #cat("accuracy:",accuracy,"\n")
  #cat("sensitivity:",sensitivity,"\n")
  #cat("specificity:",specificity,"\n")
  #cat("MCC:", MCC,"\n")
}
cat("accuracies in each fold:",accuracy,"\n")
cv_accuracy = sum(accuracy)/length(accuracy)  #10 fold cv accuracy
cat('CV_acccuracy:',cv_accuracy,"\n")
