setwd("D:/NYU_Spring_2017/machine learning_class")
install.packages("mlr")
install.packages("ROCR")
install.packages("kernlab")
install.packages("ggplot2")

source("http://bioconductor.org/biocLite.R")
biocLite("golubEsets")
library(golubEsets)
library(mlr)
library(ROCR)
library(kernlab)
library(dplyr)
library(ggplot2)

data("Golub_Train")
data("Golub_Test")
data1 <- as.data.frame(Golub_Train,na.rm=TRUE)
training<-as.data.frame(data1)
training<-training[,-7140]
training<-training[,-7139]
training<-training[,-7138]
training<-training[,-7137]
training<-training[,-7136]
training<-training[,-7135]
training<-training[,-7134]
training<-training[,-7133]
training<-training[,-7132]
training<-training[,-7130]

predictors<-read.table("D:/NYU_Spring_2017/machine learning_class/predictors.txt")
predictors<-as.list(predictors)
ALL<-rownames(training[which(training$ALL.AML=="ALL"),])
AML<-rownames(training[which(training$ALL.AML=="AML"),])
group.status<- vector(mode="numeric") #length= nrow(training))
group.status[ALL]=1
group.status[AML]=0
#data<- as.data.frame(cbind(training, group.status))
#colnames(data)<-c(colnames(training),"group.status")
#data<-data[,-7130]

train <-subset(data1, select = c(unlist(predictors[1:200])))
train<-as.data.frame(cbind(train,group.status))
test<-as.data.frame(Golub_Test)

#indx <- sapply(data, is.factor)
#data[indx] <- lapply(data[indx], function(x) as.numeric(as.character(x)))
#data$group.status <- as.factor(data$group.status)
indx1<-sapply(train,is.factor)
train[indx1] <- lapply(train[indx1], function(x) as.numeric(as.character(x)))
train$group.status <- as.factor(train$group.status)

#predictors<-c(predictors,"group.status")



ctrl = makeTuneControlGrid()
inner = makeResampleDesc("Subsample", iters = 2)
ps = makeParamSet(
  makeNumericParam("C", lower = 1, upper = 12),
  makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
  makeNumericParam("sigma", lower = 1, upper = 12,
                   requires = quote(kernel == "rbfdot")),
  makeIntegerParam("degree", lower = 2L, upper = 5L,
                   requires = quote(kernel == "polydot"))
)
outer = makeResampleDesc("RepCV", reps = 100, folds = 10)


classif.task = makeClassifTask(data = train, target = "group.status")
lrn = makeTuneWrapper("classif.ksvm", resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)
r = tuneParams("classif.ksvm", task = classif.task, resampling = outer, par.set = ps, control = ctrl,  measures = list(acc, setAggregation(acc, test.sd)))
summary(r)
print(r$extract)

saveRDS(r, "r_svm_params.rds")

lrn = setHyperPars(lrn, par.vals = r$extract)

mod <- train(lrn, classif.task)
pred <- predict(mod, newdata = test)

pred$data$truth <- test.labels
saveRDS(pred, "svm_Results")

opt.grid <- as.data.frame(r$opt.path);
vanilla.opt.grid <- opt.grid[opt.grid$kernel=='vanilladot',]
g = ggplot(vanilla.opt.grid, aes(x = C, y = acc.test.mean, fill = acc.test.mean, label = round(acc.test.mean ,3)))
g + geom_tile() + geom_text(color = "white")
poly.opt.grid <- opt.grid[opt.grid$kernel=='polydot',];
g = ggplot(poly.opt.grid, aes(x = C, y = degree, fill = acc.test.mean, label = acc.test.mean))
g + geom_tile() + geom_text(color = "white")
rbf.opt.grid <- opt.grid[opt.grid$kernel=='rbfdot',];
g = ggplot(poly.opt.grid, aes(x = C, y = sigma, fill = acc.test.mean, label = acc.test.mean))
g = ggplot(rbf.opt.grid, aes(x = C, y = sigma, fill = acc.test.mean, label = acc.test.mean))
g + geom_tile() + geom_text(color = "white")

