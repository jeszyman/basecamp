library(party)
library(randomForest)
head(readingSkills)
output.forest=randomForest(nativeSpeaker ~ age + shoeSize + score, data = readingSkills)
output.forest
plot(output.forest)

getwd()
setwd('/Users/JeffBook/')
train=read.csv('train.csv', stringsAsFactors=F)
test=read.csv('test.csv', stringsAsFactors=F)
test$Survived=rep(0,418)
submit=data.frame(PassengerId=test$PassengerId, Survived= test$Survived)
write.csv(submit, file='theyallperish.csv',row.names=F)
prop.table(table(train$Sex, train$Survived),1)
test$Survived=0
test$Survived[test$Sex == 'female']=1
summary(train$Age)
train$Child=0
train$Child[train$Age <18]=1
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
test$Survived=0
test$Survived[test$Sex == 'female'] =1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20]=0

library(rpart)
fit=rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
          data=train,
          method='class')
plot(fit)
text(fit)


library(rpart.plot)

library(RColorBrewer)

rpart.plot(fit)
predict(fit,test,type='class') 
fit=rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
          data=train,
          method='class',
          control=rpart.control(minsplit=2, cp=0))

new.fit=prp(fit, snip=T)$obj

train$Name[1]
test$Survived=NA
names(train)
names(test)
train=train[,!(names(train))=='Child']
combi=rbind(train,test)
combi$Name=as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]][2]
combi$Title=sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title=sub(' ','',combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme','Mlle')]='Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title=factor(combi$Title)
combi$FamilySize=combi$SibSp + combi$Parch +1

combi$Surname=sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID=paste(as.character(combi$FamilySize),combi$Surname, sep='')
combi$FamilyID[combi$FamilySize == 2] = 'Small'
table(combi$FamilyID)
famIDs=data.frame(table(combi$FamilyID))
famIDs=famIDs[famIDs$Freq==2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1]='Small'
combi$Family=factor(combi$FamilyID)
train=combi[1:891,]
test=combi[892:1309,]
fit=rpart(Survived ~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
          data=train,
          method='class')
rpart.plot(fit)

summary(combi$Age)

Agefit=rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
             data=combi[!is.na(combi$Age),],
             method='anova')
combi$Age[is.na(combi$Age)]=predict(Agefit,combi[is.na(combi$Age),])
summary(combi)
summary(combi$Embarked)
combi$Embarked[which(combi$Embarked=='')]='S'
combi$Embarked=factor(combi$Embarked)
combi$Fare[which(is.na(combi$Fare))]=median(combi$Fare, na.rm=T)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

library(randomForest)
set.seed(415)
fit=randomForest(as.factor(Survived)~ Pclass + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                 data=train,
                 importance=T,
                 ntree=2000)
library(ggplot2)
varImpPlot(fit)
Prediction=predict(fit,test)
class(Prediction)
summary(Prediction)
268/(268+150)

library(party)
set.seed(415)
train$Sex=as.factor(train$Sex)
train$FamilyID=as.factor(train$FamilyID)
fit=cforest(as.factor(Survived)~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
            data=train,
            controls=cforest_unbiased(ntree=2000, mtry=3))

str(train)



1=1

data(coronary)
install.packages("bnlearn")
library(bnlearn)
coronary

# Load data:

training.data.raw=read.csv('/Users/JeffBook/Desktop/Python-Data-Science-and-Machine-Learning-Bootcamp/Machine Learning Sections/Logistic-Regression/titanic_train.csv', header=T,na.strings=c(""))

# Clean data
#  Identify and remove missing data by column: 

sapply(training.data.raw,function(x)sum(is.na(x)))
sapply(training.data.raw,function(x) length(unique(x)))
library(Amelia)
missmap(training.data.raw, main='Missing values vs observed')
remove=c('Cabin','PassengerId')
data = training.data.raw[,-which(names(training.data.raw) %in% remove)]

#  By Row
#   Fill in missing data:

data$Age[is.na(data$Age)] = mean(data$Age,na.rm=T)
is.factor(data$Sex)
contrasts(data$Sex)

#   Remove missing by row: 

head(data$Embarked)
data=data[!is.na(data$Embarked),]
rownames(data)=NULL

# Fit model, apply glm():

train=data[1:400,]
test=data[401:889,]
model=glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

# Assess model
#  ANOVA for chisq by variable:

anova(model, test='Chisq')

#  Assess predictive ability with predict: 
#  ROC curve:

library(pscl)
pR2(model)
library(ROCR)

library(shiny)

ui = fluidPage()

server = function(input, output) {}

shinyApp(ui = ui, server = server)

library(shiny)

ui = fluidPage(sliderInput(inputId = 'num',
                  label = 'Choose a number',
                  value = 25, min = 1, max = 100))

server = function(input, output) {}

shinyApp(ui = ui, server = server)

install.packages('rsconnect')
library(shiny)

ui = fluidPage(
    sliderInput(inputId = 'num',
                label = 'Choose a number',
                value = 25, min = 1, max = 100),
    plotOutput('hist')
)

server = function(input, output) {
    output$hist = renderplot({
        hist(rnorm(input$num))
    })
}

shinyApp(ui= ui, server = server)

?rsconnect

rsconnect::setAccountInfo(name='jeszyman', token='7D5593D7025B8BECC9FFBBD2D9AA4D56', secret='sbbKu1u92+rnimHDSzLhL500cOMn/wy7rVdTva2+')

source("~/Rscripts/setup.R")
set.seed(314)
pkl=1000 # There will be 1000 unique accession numbers (primary key length)
time_start =  as.POSIXct('2018-10-01T01:00:00z', format = "%Y-%m-%dT%H:%M:%S")
time_end = as.POSIXct('2018-11-01T01:00:00z', format = '%Y-%m-%dT%H:%M:%S')
diff_minutes_max = difftime(time_end,time_start, units = "mins")
diff_minutes_all = sample(1:diff_minutes_max, pkl, replace = T)
result_folate = rnorm(200, 25, 12)
result_MCV = rnorm(400, 88, 8) # A normal distribution around mean MCV
result_WBC = rnorm(400, 7500, 3500)

# vectors
accession = sample(1:pkl, pkl, replace = F) # 1000 unique accession #s
visit = sample(1:500, pkl, replace = T) # 500 patient visits
patient = sample(1:350, pkl, replace=T) # 350 patients
time_result = time_start + diff_minutes_all
result = c(result_folate,result_MCV,result_WBC)
test=c(rep.int("folate", 200), rep.int("MCV", 400), rep.int("WBC", 400))
df=data.frame(accession,visit,patient,time_result,test,result)
df

ip = as.data.frame(installed.packages()[,c(1,3:4)])
ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
ip

source("~/repos/bin/R/setup.R")
install.packages("flextable")
library(flextable)
myft = flextable(head(mtcars),col_keys=c("am", "carb"))
myft

set.seed(314)

source("~/Rscripts/setup.R")
pkl=1000 # There will be 1000 unique accession numbers (primary key length)
time_start =  as.POSIXct('2018-10-01T01:00:00z', format = "%Y-%m-%dT%H:%M:%S")
time_end = as.POSIXct('2018-11-01T01:00:00z', format = '%Y-%m-%dT%H:%M:%S')
diff_minutes_max = difftime(time_end,time_start, units = "mins")
diff_minutes_all = sample(1:diff_minutes_max, pkl, replace = T)
result_folate = rnorm(200, 25, 12)
result_MCV = rnorm(400, 88, 8) # A normal distribution around mean MCV
result_WBC = rnorm(400, 7500, 3500)

# vectors
accession = sample(1:pkl, pkl, replace = F) # 1000 unique accession #s
visit = sample(1:500, pkl, replace = T) # 500 patient visits
patient = sample(1:350, pkl, replace=T) # 350 patients
time_result = time_start + diff_minutes_all
result = c(result_folate,result_MCV,result_WBC)
test=c(rep.int("folate", 200), rep.int("MCV", 400), rep.int("WBC", 400))
df=data.frame(accession,visit,patient,time_result,test,result)
df

# toy data frame of medical data
patient=1:1000 # 1000 patients
# 10,000 visits with some repeats
result=#numeric lab result between 0 and 1

runif(3, min=0, max=100)
