data("iris")
str(iris)
summary(iris)

# partition of data
ind = sample(2,nrow(iris),replace = TRUE,prob = c(0.8,0.2))
training = iris[ind == 1,]
testing = iris[ind == 2,]

# Scatter plot & Corelation
library(psych)
pairs.panels(training[,-5],
             gap=0,
             bg=c('red','yellow','blue')[training$Species],
             pch = 21)
training[,-5]
#principle component analysis
pc = prcomp(training[,-5],
            center = TRUE,
            scale. = TRUE)
pc
attributes(pc)
pc$center
pc$scale
mean(training$Sepal.Length)
sd(training$Sepal.Length)
print(pc)
summary(pc)

# Orthogonality of Pc
pairs.panels(pc$x,
             gap=0,
             bg=c('red','yellow','blue'),
             pch=21)
library(devtools)
install_github('ggbiplot','vqv')
pc
#predictino with principle component
trg=predict(pc,training)
trg
# here we convert over traning data into  trg into dataframe
trg=data.frame(trg,training[5])
trg

tst=predict(pc,testing)

# here we convert over test data into  trg into dataframe
tst=data.frame(tst,testing[5])
tst

# here we create multinomial logistic regression
library(nnet)
trg$Species=relevel(trg$Species,ref = 'setosa')
trg
mymodel =multinom(Species~PC1+PC2,data = trg)
summary(mymodel)

#confussion metrix for mis classification error  trg
pp=predict(mymodel,trg)
tab=table(pp,trg$Species)
tab
1-sum(diag(tab))/sum(tab)
#confussion metrix for mis classification error  tst
p1=predict(mymodel,tst)
tab1=table(p1,tst$Species)
tab1
1-sum(diag(tab1))/sum(tab1)

install_github("vqv/ggbiplot")

