#libraries
#library(Boruta)
#library(mlbench)
#library(caret)
#library(randomForest)
data("Sonar")

str(Sonar)
summary(Sonar)
#feature Selection
set.seed(111)
bor=Boruta(Class ~ .,data = Sonar,doTrace=2,maxRuns=500)
print(bor)
plot(bor,las=2, cex.axis=0.7)
plotImpHistory(bor)
getNonRejectedFormula(bor)
getConfirmedFormula(bor)
#here we find tentative variable
teni=TentativeRoughFix(bor)
teni
getConfirmedFormula(teni)
getNonRejectedFormula(teni)
#more important about atrribute
attStats(bor)

set.seed(222)
ind = sample(2,nrow(Sonar),replace = T,prob = c(0.6,0.4))
train = Sonar[ind==1,]
test= Sonar[ind==2,]

# Random Forest Model without feature selection
rf60 = randomForest(Class ~ .,data = train)
rf60
summary(rf60)

# Predcit test Data & Confusion Metrix
p=predict(rf60,test)
p
confusionMatrix(p,test$Class)

#here we train model with boruta 42 column reject forumula

rf42= randomForest(Class ~ V1 + V2 + V4 + V5 + V8 + V9 + V10 + V11 + V12 + V13 + 
                     V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + 
                     V26 + V27 + V28 + V29 + V30 + V31 + V32 + V34 + V35 + V36 + 
                     V37 + V39 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V51 + 
                     V52 + V54+V59,data = train)
rf42
#now here we test boruta with non rejected fomula 42
p42=predict(rf42,test)
p42
confusionMatrix(p42,test$Class)
#here we train model with boruta 33 column confirmedforumula
rf33=randomForest(Class ~ V1 + V4 + V5 + V9 + V10 + V11 + V12 + V13 + V15 + V16 + 
                    V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + V27 + V28 + 
                    V31 + V35 + V36 + V37 + V43 + V44 + V45 + V46 + V47 + V48 + 
                    V49 + V51 + V52,data = train)
rf33
#now here we test boruta with confimed formula brota 33
p33=predict(rf33,test)
p33
confusionMatrix(p33,test$Class)

