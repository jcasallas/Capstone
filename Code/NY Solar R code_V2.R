
#####Loading Data########

infile <- "CapstoneFileSolar_Final_20160329_V2"
SolarFile <- read.csv(paste0('./', infile, '.csv'), header =TRUE)
na.omit(SolarFile)
Solar<- SolarFile

summary (Solar)
str(Solar)


Solar$SolarInstalled<- as.character(Solar$SolarInstalled)
Solar$Zip<- as.character(Solar$Zip)

Solar$SolarInstalled[Solar$SolarInstalled=="Yes"]<- 1
Solar$SolarInstalled[Solar$SolarInstalled=="No"]<- 0




Solar$SolarInstalled<- as.numeric(Solar$SolarInstalled)


str(Solar)

nums <- sapply(Solar, is.numeric)

M<-cor(Solar[,nums])

corrplot(cor(Solar[,nums]))
corrplot(M,order = "AOE", cl.ratio = 0.2, cl.align = "r")

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(Solar[,nums], 0.95)
res2 <- cor.mtest(Solar[,nums], 0.99)

corrplot(M, p.mat = res1[[2]], sig.level = 0.2)


InstallationsPerZip	Households	Pop	AverageIncome		Sola_Penetration		SolarInstalled	Average HH Size		Electric Utility	OwnerOccupancyPercentage	OwnerDetachedpercentage


cor.test(Solar$SolarInstalled, Solar$AverageIncome, method = c("pearson"))
cor.test(Solar$SolarInstalled, Solar$Sola_Penetration, method = c("pearson"))
cor.test(Solar$SolarInstalled, Solar$Average.HH.Size, method = c("pearson"))
cor.test(Solar$SolarInstalled, Solar$Households, method = c("pearson"))
cor.test(Solar$SolarInstalled, Solar$OwnerOccupancyPercentage, method = c("pearson"))
cor.test(Solar$SolarInstalled, Solar$OwnerDetachedpercentage, method = c("pearson"))
set.seed(2165)




#############################################################
#Model 1 - logistic regression Model
#############################################################

# Lower AIC indicated better model
rn_train_LRM <-sample(nrow(Solar),floor(nrow(Solar)*0.75))
train_LRM <- Solar[rn_train_LRM,]
test_LRM <- Solar[,][-rn_train_LRM,]

forwardtest_LRM <- step(glm(SolarInstalled~1, data= train_LRM), direction = "forward", scope = ~AverageIncome+Average.HH.Size+Households+OwnerOccupancyPercentage+OwnerDetachedpercentage)

str(Solar)
#Here is the logistic regressionmodel based off of the results

mylogit_LRM <- glm (SolarInstalled~AverageIncome+Average.HH.Size+Households+OwnerOccupancyPercentage+OwnerDetachedpercentage, data=train_LRM, family ="binomial")
summary (mylogit_LRM)

# This will evaluate the models fit and performance

influenceIndexPlot(mylogit_LRM, var=c("cook","hat"), id.n = 3)

## CIS Using Profiled log-likelihood
confint(mylogit_LRM)

## CIS using standard errors
confint.default(mylogit_LRM)

##put the coefficents and CI in a formar onto a useful scale
exp (mylogit_LRM$coefficients)
exp(confint(mylogit_LRM))

##odd ratios only
exp(coef(mylogit_LRM))

## odds rations and 95% CI
exp(cbind(OR = coef(mylogit_LRM),confint(mylogit_LRM)))

##############################################################
#Model 2 Multivariate
############################################################

Solar$SolarInstalled<- as.numeric(Solar$SolarInstalled)
model_mlr <-lm(SolarInstalled~AverageIncome+Average.HH.Size+Households+OwnerOccupancyPercentage+OwnerDetachedpercentage, data=Solar)
summary(model_mlr)


rn_train_MLR <-sample(nrow(Solar),floor(nrow(Solar)*0.75))
train_MLR <- Solar[rn_train_MLR,c(nums)]
test_MLR <- Solar[,nums][-rn_train_MLR,]
model_ulm_MLR <-lm(SolarInstalled~AverageIncome+Average.HH.Size+Households+OwnerOccupancyPercentage+OwnerDetachedpercentage, data=train_MLR)
prediction_MLR <-predict(model_ulm_MLR, test_MLR)

##############################################################
#Model 3 Random Forest Model
############################################################
##set.seed(2584)
Solar$SolarInstalled<- as.integer(Solar$SolarInstalled)
rn_train_RFM <-sample(nrow(Solar),floor(nrow(Solar)*0.75))
train_RFM <- Solar[rn_train_RFM,c(nums)]
test_RFM <- Solar[,nums][-rn_train_RFM,]

randomForestmodel <- randomForest(SolarInstalled~AverageIncome+Average.HH.Size+Households+OwnerOccupancyPercentage+OwnerDetachedpercentage, data = train_RFM, ntree=250, ntry=5,importance=TRUE)

print(randomForestmodel)
importance(randomForestmodel)


plot.new()

varImpPlot(randomForestmodel)

varImpPlot(randomForestmodel, type=1, pch=19, col=2, cex=1.0, main="")
abline(v=90, col="blue")

plot.new()

varImpPlot(randomForestmodel, type=2, pch=19, col=1, cex=1.0, main="")
abline(v=45, col="blue")



##############################################################
#Knowledge Discovery Build a decision tree using c5.0 for churn
############################################################

Solar$SolarInstalled<- as.factor(Solar$SolarInstalled)
Solar$Zip<- as.character(Solar$Zip)

str(Solar)

c50_tree_reslt<-C5.0(SolarInstalled~AverageIncome+Average.HH.Size+Households+OwnerOccupancyPercentage+OwnerDetachedpercentage,data=Solar, rules = TRUE)

summary (c50_tree_reslt)

C5imp(c50_tree_reslt, metric="usage")
C5imp(c50_tree_reslt, metric="splits")




############################
#Model 4 Bayes Calssifier
###########################
Solar$SolarInstalled<- as.factor(Solar$SolarInstalled)

rn_train_NB <- sample(nrow(Solar),
                   floor(nrow(Solar)*0.75))
train_NB <- Solar[rn_train_NB,]

test_NB <- Solar[,][-rn_train_NB,]

BayesModel <- naiveBayes(SolarInstalled~AverageIncome+Average.HH.Size+Households+OwnerOccupancyPercentage+OwnerDetachedpercentage, data=train_NB)



BayesModelPredict<- predict(BayesModel,test_NB)

table(pred=BayesModelPredict, true=test_NB$SolarInstalled)

mean(BayesModelPredict==test_NB$SolarInstalled)



#######################################################
#Measure preformance
######################################################

LogiticModel<- predict(mylogit_LRM, test_LRM, type="response")
RFResult <- predict(randomForestmodel, test_RFM, type="response")
BayesResults <-  predict(BayesModel, test_NB)
MLRResults <-predict(model_ulm_MLR, test_MLR)


test_LRM$YHat1 <-predict(mylogit_LRM,test_LRM, type="response")
test_MLR$YHat2 <-predict(model_ulm_MLR,test_MLR, type="response")
test_RFM$YHat3 <- predict(randomForestmodel,test_RFM, type="response")
test_NB$YHat4 <- predict(BayesModel,test_NB)


predict <- function(t) ifelse(LogiticModel >t, 1,0)
predict2 <- function(t) ifelse(MLRResults >t, 1,0)
predict3 <- function(t) ifelse(RFResult >t, 1,0)
predict4 <- function(t) ifelse(BayesResults >t, 1,0)


confusionMatrix(predict(0.5), test_LRM$SolarInstalled)
confusionMatrix(predict(0.5), test_MLR$SolarInstalled)
confusionMatrix(predict3(0.5), test_RFM$SolarInstalled)
confusionMatrix(test_NB$SolarInstalled,test_NB$YHat4)

test_NB$YHat4<- as.numeric(test_NB$YHat4)
test_NB$SolarInstalled<- as.numeric(test_NB$SolarInstalled)

pred1<- prediction(test_LRM$YHat1, test_LRM$SolarInstalled)
pred2<- prediction(test_MLR$YHat2, test_MLR$SolarInstalled)
pred3<- prediction(test_RFM$YHat3, test_RFM$SolarInstalled)
pred4<- prediction(test_NB$YHat4, test_NB$SolarInstalled)




perf<- performance(pred1, "tpr", "fpr")
perf2<- performance(pred2, "tpr", "fpr")
perf3<- performance(pred3, "tpr", "fpr")
perf4<- performance(pred4, "tpr", "fpr")


plot.new()

plot(perf,col="green", lwd =2.5, cex=0.5, cex.main=2, cex.label=1.5)
plot(perf2, add= TRUE, col="black", lwd =2.5)
plot(perf3, add= TRUE, col="blue", lwd =2.5)
plot(perf4, add= TRUE, col="Yellow", lwd =2.5)
abline(0,1, col="red",lwd=2.5, lty=2)

title("ROC Curve")
legend (0.7,0.4,c("Logistic","Multivariate","RF", "Naive Baynes"),
        lty=c(1,1,1,1),
        lwd=c(1.4, 1.4,1.4,1.4), col=c("green","black", "blue", "yellow"))

fit.auc1 <- performance(pred1,"auc")
fit.auc2 <- performance(pred2,"auc")
fit.auc3 <- performance(pred3,"auc")
fit.auc4 <- performance(pred4,"auc")

fit.auc1
fit.auc2
fit.auc3
fit.auc4

#######################################################
#Save model for later use
######################################################
## we idenify the Random Forest Model to be the best model to predict if a given customer would install Solar or not
save(randomForestmodel, file ="SolarNYPredictionModel.rda")
