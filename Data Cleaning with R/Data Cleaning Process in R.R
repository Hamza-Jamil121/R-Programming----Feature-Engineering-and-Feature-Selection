
train=read.csv('C:\\Users\\hamza jamil\\Downloads\\R for Data Science\\IntroToDataScience-master\\train.csv')
test=read.csv("C:\\Users\\hamza jamil\\Downloads\\R for Data Science\\IntroToDataScience-master\\test.csv")

test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

#test1=cbind(test,test.survived$survived)
data.combined <- rbind(train, test.survived)
length(train)
count(test)
nrow(test)
nrow(train)
ncol(test)
nrow(test.survived)
data.combined$pclass
table(data.combined$pclass)
table(data.combined$sex)
unique(data.combined$name)
length(unique(data.combined$name))      
duplicated(data.combined$age)

train[5:9,]
is.na(train$age)
sum(is.na(train$age))
train$age[is.na(train$age)]=0
mean(train$age)
is.na(train$age)
train$age ==0

filter(train,sex== 'male')
train$sex=='male'
#here we filter value == male
ki=is.factor(train$sex)
train$sex[train$sex == 'male']

#here we filter age value ==0
is.numeric(train$age)
train$age[train$age == 0 ]
filter(age == 0)
train$age[train$age == 30  ] #+  train$sex[train$sex == 'male']
# here we filter 2 or 3 column with our given datasets
djk=filter(train,(train$age == 30 & train$sex == 'male' & train$pclass == 2))
#here we fiter 2 or 3 column using over or conditoin
bk=filter(train,(train$survived ==1 & train$age == 30)| (train$survived ==0 & train$age == 40) )
bk

head(train)
is.na(train$age)
nrow(train$age)
sum(train$age)
length(train$age)
fc=cbind(train$pclass,train$name,train$sex,train$age)
head(train$name)
colnames = c('pclass,'name)
#here we select column in R
df <- data.frame(a = 1:10, b = 2:11, c = 3:12)
df <- subset(df, select = c(a, c))
#

sed = subset(train,select =c(name,age))
sed
length(sed)
summary(sed)
length(sed$age)
unique(sed$age)
unique(train$sex)
# here we work on Impute Alogrithm to clean over data
#library(mice)
#library(VIM)
#library(dplyr)
#library(plyr)

inco = read.csv('C:\\Users\\hamza jamil\\Desktop\\All dcouemnnt\\income.csv')
din=is.na(inco)
sum(is.na(inco))
# omit function remove all na value from dataset
omi=na.omit(inco)
dn=inco$Income
drop()
mean(inco$Income)
mean(inco$Income,na.rm = TRUE)
p=function(x) {sum(is.na(x))/ length(x)*100}
apply(inco,2,p)
md.pattern(inco)
md.pairs(inco)
marginplot(inco[,c('Age','Income')])
impute = mice(inco[,1:3],m=3)
print(impute)
impute$imp$Age
summary(inco)
complete(impute,1)
complete(impute,2)
impute$imp$Name
new_data = complete(impute,1)
new_data
stripplot(impute,pch=20,cex=1.2)
