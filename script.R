#load package
require(caret)
require(ggplot2)
#attach the iris dataset to the invironment
data("iris")
#rename the dataset
dataset <- iris
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]
#summarize the dataset
#dimensions of the dataset
dim(dataset)
#list types of each attribute
sapply(dataset, class)
#take a peek at the first 5 rows of the data
head(dataset)
#list the levels for the class
levels(dataset$Species)
#summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)
#summarize attribute distributions
summary(dataset)

#visualize dataset
#split input and output
x <- dataset[,1:4]
y <- dataset[,5]
#boxplot for each attribute on the same image
par(mfrow=c(1,4))
for(i in 1:4){
  boxplot(x[,i], main=names(iris)[i])
}
#bar plot for class breakdown
plot(y)
#scatter plot matrix
featurePlot(x=x, y=y, plot="ellipse")
#box and whisker plots for each attribute
featurePlot(x=x,y=y,plot="box")
#density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x,y=y,plot="density",scales=scales)

#evaluate some algorithms
#run algorithms using 10-fold cross validation
control <- trainControl(method="cv",number=10)
metric <- "Accuracy"
#build models
#1.linear algorithms
set.seed(7)
fit.lda <- train(Species~.,data=dataset,method="lda",metric=metric,trControl=control)
#2.non-linear algorithms
#CART
set.seed(7)
fit.cart <- train(Species~.,data=dataset,method="rpart",metric=metric,trControl=control)
#kNN
set.seed(7)
fit.knn <- train(Species~.,data=dataset,method="knn",metric=metric,trControl=control)
#3.advanced algorithms
#randomforest
set.seed(7)
fit.rf <- train(Species~.,data=dataset,method="rf",metric=metric,trControl=control)

#summarize accuracy of the models
results <- resamples(list(lda=fit.lda,cart=fit.cart,knn=fit.knn,rf=fit.rf))
summary(results)
#compare accuracy of the models
dotplot(results)
#summarize the best model
print(fit.lda)

#make predictions
#estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda,validation)
confusionMatrix(predictions,validation$Species)




