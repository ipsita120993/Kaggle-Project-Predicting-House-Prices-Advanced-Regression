#All the data will be coming as numbers instead of exponential
options(scipen=999)

#Get the directory
getwd()

#Set the directory
setwd("D:/data science")

#Load the required libraries from memory
library(dplyr)
library(ggplot2)
library(caret)
library(psych)
library(glmnet)

#Import the dataset
house_sale_price_train<- read.csv("D:/data science/House Prices Advanced Regression Techniques/train.csv",header=T)
house_sale_price_test<- read.csv("D:/data science/House Prices Advanced Regression Techniques/test.csv",header=T)
house_sale_price_test$SalePrice<- NA

#Combine the train and test dataset for further interpretation and cleaning
house_sale_prediction<- rbind(house_sale_price_train,house_sale_price_test)
write.csv(house_sale_prediction,"D:/data science/House Prices Advanced Regression Techniques/house_sale_prediction.csv")

#check the class type of all the variables
myfun1<- function(x)
{
  class(x)
}
myfun1out<- t(lapply(house_sale_prediction,FUN=myfun1))
write.csv(myfun1out,file="D:/data science/House Prices Advanced Regression Techniques/data_types.csv")

#Check the dataframe
View(house_sale_prediction)
str(house_sale_prediction)
summary(house_sale_prediction)

#Drop the variables which is not required
house_sale_prediction$Id<- NULL

#Changing the data type of the variables of the dataframe as per requirement
house_sale_prediction$MSSubClass <- as.factor(house_sale_prediction$MSSubClass)
house_sale_prediction$LotFrontage <- as.numeric(house_sale_prediction$LotFrontage)
house_sale_prediction$LotArea<- as.numeric(house_sale_prediction$LotArea)
house_sale_prediction$OverallQual <- as.numeric(house_sale_prediction$OverallQual)
house_sale_prediction$OverallCond <- as.numeric(house_sale_prediction$OverallCond)
house_sale_prediction$YearBuilt <- as.numeric(house_sale_prediction$YearBuilt)
house_sale_prediction$YearRemodAdd<- as.numeric(house_sale_prediction$YearRemodAdd)
house_sale_prediction$MasVnrArea<- as.numeric(house_sale_prediction$MasVnrArea)
house_sale_prediction$BsmtFinSF1<- as.numeric(house_sale_prediction$BsmtFinSF1)
house_sale_prediction$BsmtFinSF2<- as.numeric(house_sale_prediction$BsmtFinSF2)
house_sale_prediction$TotalBsmtSF <- as.numeric(house_sale_prediction$TotalBsmtSF)
house_sale_prediction$BsmtUnfSF<- as.numeric(house_sale_prediction$BsmtUnfSF)
house_sale_prediction$X1stFlrSF <- as.numeric(house_sale_prediction$X1stFlrSF)
house_sale_prediction$X2ndFlrSF<- as.numeric(house_sale_prediction$X2ndFlrSF)
house_sale_prediction$LowQualFinSF <- as.numeric(house_sale_prediction$LowQualFinSF)
house_sale_prediction$GrLivArea<- as.numeric(house_sale_prediction$GrLivArea)
house_sale_prediction$BsmtFullBath <- as.numeric(house_sale_prediction$BsmtFullBath)
house_sale_prediction$BsmtHalfBath <- as.numeric(house_sale_prediction$BsmtHalfBath)
house_sale_prediction$FullBath <- as.numeric(house_sale_prediction$FullBath)
house_sale_prediction$HalfBath <- as.numeric(house_sale_prediction$HalfBath)
house_sale_prediction$BedroomAbvGr <- as.numeric(house_sale_prediction$BedroomAbvGr)
house_sale_prediction$KitchenAbvGr<- as.numeric(house_sale_prediction$KitchenAbvGr)
house_sale_prediction$TotRmsAbvGrd <- as.numeric(house_sale_prediction$TotRmsAbvGrd)
house_sale_prediction$Fireplaces <- as.numeric(house_sale_prediction$Fireplaces)
house_sale_prediction$GarageYrBlt <- as.numeric(house_sale_prediction$GarageYrBlt)
house_sale_prediction$GarageCars <- as.numeric(house_sale_prediction$GarageCars)
house_sale_prediction$GarageArea <- as.numeric(house_sale_prediction$GarageArea)
house_sale_prediction$WoodDeckSF <- as.numeric(house_sale_prediction$WoodDeckSF)
house_sale_prediction$OpenPorchSF <- as.numeric(house_sale_prediction$OpenPorchSF)
house_sale_prediction$EnclosedPorch <- as.numeric(house_sale_prediction$EnclosedPorch)
house_sale_prediction$X3SsnPorch <- as.numeric(house_sale_prediction$X3SsnPorch)
house_sale_prediction$ScreenPorch <- as.numeric(house_sale_prediction$ScreenPorch)
house_sale_prediction$PoolArea <- as.numeric(house_sale_prediction$PoolArea)
house_sale_prediction$MiscVal <- as.numeric(house_sale_prediction$MiscVal)
house_sale_prediction$MoSold <- as.factor(house_sale_prediction$MoSold)
house_sale_prediction$YrSold <- as.numeric(house_sale_prediction$YrSold)

#Separate the categorical and numerical data
numericVars <- which(sapply(house_sale_prediction, is.numeric))
num_data <- house_sale_prediction[,numericVars]
facVars <- which(sapply(house_sale_prediction, is.factor))
fac_data<- house_sale_prediction[,facVars]

#Write the num_data and fac_data in two separate files
write.csv(num_data,file="D:/data science/House Prices Advanced Regression Techniques/num.csv")
write.csv(fac_data,file="D:/data science/House Prices Advanced Regression Techniques/fac.csv")

#First, categorical variables data cleaning process
#We need to do missing value imputation with mode, level encoding.
#MSZoning
table(fac_data$MSZoning)
fac_data$MSZoning[is.na(fac_data$MSZoning)] <- names(sort(-table(fac_data$MSZoning)))[1]
fac_data$MSZoning <- as.factor(fac_data$MSZoning)

#Alley
table(fac_data$Alley)
levels(fac_data$Alley)
fac_data$Alley <- as.factor(fac_data$Alley)
fac_data$Alley = factor(fac_data$Alley, levels=c(levels(fac_data$Alley),"None"))
fac_data$Alley[is.na(fac_data$Alley)] = "None"


#Utilities
sum(is.na(fac_data$Utilities))
table(fac_data$Utilities)
fac_data$Utilities[is.na(fac_data$Utilities)] <- names(sort(-table(fac_data$Utilities)))[1]
fac_data$Utilities <- as.factor(fac_data$Utilities)

#Exterior1st and Exterior2nd
sum(is.na(fac_data$Exterior1st))
table(fac_data$Exterior1st)
fac_data$Exterior1st[is.na(fac_data$Exterior1st)] <- names(sort(-table(fac_data$Exterior1st)))[1]
fac_data$Exterior1st <- as.factor(fac_data$Exterior1st)

sum(is.na(fac_data$Exterior2nd))
table(fac_data$Exterior2nd)
fac_data$Exterior2nd[is.na(fac_data$Exterior2nd)] <- names(sort(-table(fac_data$Exterior2nd)))[1]
fac_data$Exterior2nd <- as.factor(fac_data$Exterior2nd)

#MasVnrType-> replace NA with mode
sum(is.na(fac_data$MasVnrType))
table(fac_data$MasVnrType)
fac_data$MasVnrType[is.na(fac_data$MasVnrType)] <- names(sort(-table(fac_data$MasVnrType)))[1]
fac_data$MasVnrType <- as.factor(fac_data$MasVnrType)

#BsmtQual-> replace Na with none for no basement
table(fac_data$BsmtQual)
levels(fac_data$BsmtQual)
fac_data$BsmtQual <- as.factor(fac_data$BsmtQual)
fac_data$BsmtQual = factor(fac_data$BsmtQual, levels=c(levels(fac_data$BsmtQual),"None"))
fac_data$BsmtQual[is.na(fac_data$BsmtQual)] = "None"

#BsmtCond
table(fac_data$BsmtCond)
levels(fac_data$BsmtCond)
fac_data$BsmtCond <- as.factor(fac_data$BsmtCond)
fac_data$BsmtCond = factor(fac_data$BsmtCond, levels=c(levels(fac_data$BsmtCond),"No Basement"))
fac_data$BsmtCond[is.na(fac_data$BsmtCond)] = "No Basement"

#BsmtExposure
table(fac_data$BsmtExposure)
levels(fac_data$BsmtExposure)
fac_data$BsmtExposure <- as.factor(fac_data$BsmtExposure)
fac_data$BsmtExposure = factor(fac_data$BsmtExposure, levels=c(levels(fac_data$BsmtExposure),"No Basement"))
fac_data$BsmtExposure[is.na(fac_data$BsmtExposure)] = "No Basement"

#BsmtFinType1
table(fac_data$BsmtFinType1)
levels(fac_data$BsmtFinType1)
fac_data$BsmtFinType1 <- as.factor(fac_data$BsmtFinType1)
fac_data$BsmtFinType1 = factor(fac_data$BsmtFinType1, levels=c(levels(fac_data$BsmtFinType1),"No Basement"))
fac_data$BsmtFinType1[is.na(fac_data$BsmtFinType1)] = "No Basement"

#BsmtFinType2
table(fac_data$BsmtFinType2)
levels(fac_data$BsmtFinType2)
fac_data$BsmtFinType2 <- as.factor(fac_data$BsmtFinType2)
fac_data$BsmtFinType2 = factor(fac_data$BsmtFinType2, levels=c(levels(fac_data$BsmtFinType2),"No Basement"))
fac_data$BsmtFinType2[is.na(fac_data$BsmtFinType2)] = "No Basement"

#Electrical
sum(is.na(fac_data$Electrical))
table(fac_data$Electrical)
fac_data$Electrical[is.na(fac_data$Electrical)] <- names(sort(-table(fac_data$Electrical)))[1]
fac_data$Electrical <- as.factor(fac_data$Electrical)

#KitchenQual
sum(is.na(fac_data$KitchenQual))
table(fac_data$KitchenQual)
fac_data$KitchenQual[is.na(fac_data$KitchenQual)] <- names(sort(-table(fac_data$KitchenQual)))[1]
fac_data$KitchenQual <- as.factor(fac_data$KitchenQual)

#Functional
sum(is.na(fac_data$Functional))
table(fac_data$Functional)
fac_data$Functional[is.na(fac_data$Functional)] <- names(sort(-table(fac_data$Functional)))[1]
fac_data$Functional <- as.factor(fac_data$Functional)

#FireplaceQu
table(fac_data$FireplaceQu)
levels(fac_data$FireplaceQu)
fac_data$FireplaceQu <- as.factor(fac_data$FireplaceQu)
fac_data$FireplaceQu = factor(fac_data$FireplaceQu, levels=c(levels(fac_data$FireplaceQu),"No Fireplace"))
fac_data$FireplaceQu[is.na(fac_data$FireplaceQu)] = "No Fireplace"

#GarageType
table(fac_data$GarageType)
levels(fac_data$GarageType)
fac_data$GarageType <- as.factor(fac_data$GarageType)
fac_data$GarageType = factor(fac_data$GarageType, levels=c(levels(fac_data$GarageType),"No Garage"))
fac_data$GarageType[is.na(fac_data$GarageType)] = "No Garage"

#GarageFinish
table(fac_data$GarageFinish)
levels(fac_data$GarageFinish)
fac_data$GarageFinish <- as.factor(fac_data$GarageFinish)
fac_data$GarageFinish = factor(fac_data$GarageFinish, levels=c(levels(fac_data$GarageFinish),"No Garage"))
fac_data$GarageFinish[is.na(fac_data$GarageFinish)] = "No Garage"

#GarageQual
table(fac_data$GarageQual)
levels(fac_data$GarageQual)
fac_data$GarageQual <- as.factor(fac_data$GarageQual)
fac_data$GarageQual = factor(fac_data$GarageQual, levels=c(levels(fac_data$GarageQual),"No Garage"))
fac_data$GarageQual[is.na(fac_data$GarageQual)] = "No Garage"

#GarageCond
table(fac_data$GarageCond)
levels(fac_data$GarageCond)
fac_data$GarageCond <- as.factor(fac_data$GarageCond)
fac_data$GarageCond = factor(fac_data$GarageCond, levels=c(levels(fac_data$GarageCond),"No Garage"))
fac_data$GarageCond[is.na(fac_data$GarageCond)] = "No Garage"

#PoolQC
table(fac_data$PoolQC)
levels(fac_data$GarageCond)
fac_data$PoolQC <- as.factor(fac_data$PoolQC)
fac_data$PoolQC = factor(fac_data$PoolQC, levels=c(levels(fac_data$PoolQC),"No Pool"))
fac_data$PoolQC[is.na(fac_data$PoolQC)] = "No Pool"

#Fence
table(fac_data$Fence)
levels(fac_data$Fence)
fac_data$Fence <- as.factor(fac_data$Fence)
fac_data$Fence = factor(fac_data$Fence, levels=c(levels(fac_data$Fence),"No Fence"))
fac_data$Fence[is.na(fac_data$Fence)] = "No Fence"

#MiscFeature
table(fac_data$MiscFeature)
levels(fac_data$MiscFeature)
fac_data$MiscFeature <- as.factor(fac_data$MiscFeature)
fac_data$MiscFeature = factor(fac_data$MiscFeature, levels=c(levels(fac_data$MiscFeature),"None"))
fac_data$MiscFeature[is.na(fac_data$MiscFeature)] = "None"

#SaleType
sum(is.na(fac_data$SaleType))
table(fac_data$SaleType)
fac_data$SaleType[is.na(fac_data$SaleType)] <- names(sort(-table(fac_data$SaleType)))[1]
fac_data$SaleType <- as.factor(fac_data$SaleType)

#So, here all missing values are treated properly
#Next, label encoding for the categorical variables and whichever required convert it to numeric
#PoolQC label encoding
table(fac_data$PoolQC)
fac_data<- transform(fac_data,PoolQC= ifelse(PoolQC == "No Pool", 0,
                                             ifelse(PoolQC == "Fa", 1, ifelse(PoolQC == "Gd",2,3))))

#GarageCond label encoding
table(fac_data$GarageCond)
fac_data<- transform(fac_data,GarageCond=ifelse(GarageCond == "No Garage", 0,ifelse(GarageCond == "Po", 1,ifelse(GarageCond == "Fa",2,ifelse(GarageCond=="TA",3,ifelse(GarageCond=="Gd",4,5))))))

#GarageQual label encoding
table(fac_data$GarageQual)
fac_data<- transform(fac_data,GarageQual=ifelse(GarageQual == "No Garage", 0,ifelse(GarageQual == "Po", 1,ifelse(GarageQual == "Fa",2,ifelse(GarageQual=="TA",3,ifelse(GarageQual=="Gd",4,5))))))

#GarageFinish label encoding
table(fac_data$GarageFinish)
fac_data<- transform(fac_data,GarageFinish=ifelse(GarageFinish == "No Garage", 0,ifelse(GarageFinish == "Unf", 1,ifelse(GarageFinish == "RFn",2,3))))

#FireplaceQu label encoding
table(fac_data$FireplaceQu)
fac_data<- transform(fac_data,FireplaceQu=ifelse(FireplaceQu == "No Fireplace", 0,ifelse(FireplaceQu == "Po", 1,ifelse(FireplaceQu == "Fa",2,ifelse(FireplaceQu=="TA",3,ifelse(FireplaceQu=="Gd",4,5))))))

#Functional label encoding
table(fac_data$Functional)
fac_data<- transform(fac_data,Functional=ifelse(Functional == "Sal", 0,ifelse(Functional == "Sev", 1,ifelse(Functional == "Maj2",2,ifelse(Functional=="Maj1",3,ifelse(Functional=="Mod",4,ifelse(Functional=="Min2",5,ifelse(Functional=="Min1",6,7))))))))

#KitchenQual label encoding
table(fac_data$KitchenQual)
fac_data<- transform(fac_data,KitchenQual= ifelse(KitchenQual == "Fa", 0,
                                             ifelse(KitchenQual == "TA", 1, ifelse(KitchenQual == "Gd",2,3))))

#HeatingQC label encoding
table(fac_data$HeatingQC)
fac_data<- transform(fac_data,HeatingQC=ifelse(HeatingQC == "Po", 0,ifelse(HeatingQC == "Fa", 1,ifelse(HeatingQC == "TA",2,ifelse(HeatingQC=="Gd",3,4)))))

#BsmtFinType1 label encoding
table(fac_data$BsmtFinType1)
fac_data<- transform(fac_data,BsmtFinType1=ifelse(BsmtFinType1 == "No Basement", 0,ifelse(BsmtFinType1 == "Unf", 1,ifelse(BsmtFinType1 == "LwQ",2,ifelse(BsmtFinType1=="Rec",3,ifelse(BsmtFinType1=="BLQ",4,ifelse(BsmtFinType1=="ALQ",5,6)))))))

#BsmtFinType2 label encoding
table(fac_data$BsmtFinType2)
fac_data<- transform(fac_data,BsmtFinType2=ifelse(BsmtFinType2 == "No Basement", 0,ifelse(BsmtFinType2 == "Unf", 1,ifelse(BsmtFinType2 == "LwQ",2,ifelse(BsmtFinType2=="Rec",3,ifelse(BsmtFinType2=="BLQ",4,ifelse(BsmtFinType2=="ALQ",5,6)))))))

#BsmtExposure label encoding
table(fac_data$BsmtExposure)
fac_data<- transform(fac_data,BsmtExposure=ifelse(BsmtExposure == "No Basement", 0,ifelse(BsmtExposure == "No", 1,ifelse(BsmtExposure == "Mn",2,ifelse(BsmtExposure=="Av",3,4)))))

#BsmtCond label encoding
table(fac_data$BsmtCond)
fac_data<- transform(fac_data,BsmtCond=ifelse(BsmtCond == "No Basement", 0,ifelse(BsmtCond == "Po", 1,ifelse(BsmtCond == "Fa",2,ifelse(BsmtCond=="TA",3,ifelse(BsmtCond=="Gd",4,5))))))

#BsmtQual label encoding
table(fac_data$BsmtQual)
fac_data<- transform(fac_data,BsmtQual=ifelse(BsmtQual == "None", 0,ifelse(BsmtQual == "Po", 1,ifelse(BsmtQual == "Fa",2,ifelse(BsmtQual=="TA",3,ifelse(BsmtQual=="Gd",4,5))))))

#ExterQual label encoding
table(fac_data$ExterQual)
fac_data<- transform(fac_data,ExterQual=ifelse(ExterQual == "Po", 0,ifelse(ExterQual == "Fa", 1,ifelse(ExterQual == "TA",2,ifelse(ExterQual=="Gd",3,4)))))

#ExterCond label encoding
table(fac_data$ExterCond)
fac_data<- transform(fac_data,ExterCond=ifelse(ExterCond == "Po", 0,ifelse(ExterCond == "Fa", 1,ifelse(ExterCond == "TA",2,ifelse(ExterCond=="Gd",3,4)))))

#LandSlope label encoding
table(fac_data$LandSlope)
fac_data<- transform(fac_data,LandSlope=ifelse(LandSlope == "Gtl", 0,ifelse(LandSlope == "Mod", 1,2)))

#Utilities label encoding
table(fac_data$Utilities)
#this variable has zero variance, because all houses have all utilities.So, we will remove this variable.
fac_data$Utilities<- NULL

#LotShape label encoding
table(fac_data$LotShape)
fac_data<- transform(fac_data,LotShape=ifelse(LotShape == "IR3", 0,ifelse(LotShape == "IR2", 1,ifelse(LotShape == "IR1",2,3))))

#Now,convert all these label encoded categorical variables which are ordinal in nature into numeric
fac_data$LotShape <- as.numeric(fac_data$LotShape)
fac_data$LandSlope <- as.numeric(fac_data$LandSlope)
fac_data$ExterCond <- as.numeric(fac_data$ExterCond)
fac_data$ExterQual <- as.numeric(fac_data$ExterQual)
fac_data$BsmtCond <- as.numeric(fac_data$BsmtCond)
fac_data$BsmtQual <- as.numeric(fac_data$BsmtQual)
fac_data$BsmtExposure <- as.numeric(fac_data$BsmtExposure)
fac_data$BsmtFinType2 <- as.numeric(fac_data$BsmtFinType2)
fac_data$BsmtFinType1 <- as.numeric(fac_data$BsmtFinType1)
fac_data$HeatingQC <- as.numeric(fac_data$HeatingQC)
fac_data$KitchenQual <- as.numeric(fac_data$KitchenQual)
fac_data$Functional <- as.numeric(fac_data$Functional)
fac_data$FireplaceQu <- as.numeric(fac_data$FireplaceQu)
fac_data$GarageQual <- as.numeric(fac_data$GarageQual)
fac_data$GarageFinish <- as.numeric(fac_data$GarageFinish)
fac_data$PoolQC <- as.numeric(fac_data$PoolQC)
fac_data$GarageCond <- as.numeric(fac_data$GarageCond)

#Keep all these converted categorical variables in another df
numericVars <- which(sapply(fac_data, is.numeric))
num_data_new <- fac_data[,numericVars]

#Drop all the converted cat variables from fac_data
fac_data$LotShape <- NULL
fac_data$LandSlope <- NULL
fac_data$ExterCond <- NULL
fac_data$ExterQual <- NULL
fac_data$BsmtCond <- NULL
fac_data$BsmtQual <- NULL
fac_data$BsmtExposure <- NULL
fac_data$BsmtFinType2 <- NULL
fac_data$BsmtFinType1 <- NULL
fac_data$HeatingQC <- NULL
fac_data$KitchenQual <- NULL
fac_data$Functional <- NULL
fac_data$FireplaceQu <- NULL
fac_data$GarageQual <- NULL
fac_data$GarageFinish <- NULL
fac_data$PoolQC <- NULL
fac_data$GarageCond <- NULL

#Combine num_data and num_data_new
num_data<- cbind(num_data,num_data_new)

#Write the num_data and fac_data in two separate files
write.csv(num_data,file="D:/data science/House Prices Advanced Regression Techniques/num.csv")
write.csv(fac_data,file="D:/data science/House Prices Advanced Regression Techniques/fac.csv")

#So, in total there are total 27 categorical variables and 52 numerical variables

#Now, outlier treatment and missing value treatment for numerical variables
#Understanding the numerical data
myfun3<- function(x)
{
  var_type=class(x)   
  nmiss<- sum(is.na(x))
  MEAN<- mean(x,na.rm=T)
  SD<- sd(x,na.rm=T)
  pctl<- quantile(x,na.rm=T,p=c(0.01,0.05,0.5,0.09,0.95,0.98,0.99))
  MAX<- max(x,na.rm=T)
  MIN<- min(x,na.rm=T)
  return(c(var_type=var_type,nmiss=nmiss,MEAN=MEAN,SD=SD,MAX=MAX,MIN=MIN,pctl=pctl))
}
num_housesale_data<- t(data.frame(lapply(num_data,FUN=myfun3)))
write.csv(num_housesale_data,file="D:/data science/House Prices Advanced Regression Techniques/descriptive.csv") 

#Missing Value Detection for NA
missing_treat1<- function(x)
{
  NMISSpct<- (sum(is.na(x))*100)/2919
  return(c(NMISSpct=NMISSpct))
}
out<-lapply(num_data,FUN=missing_treat1)
print(out)

#Check for outlier detection excel file in descriptive.csv
#Check for symmetric and non symmetric variables in excel file descriptive.csv by comparing
#mean and median. You can also draw histograms for checking symmetric and non symmetric.

#Outlier Treatment with 99 percentile as upper cap and 1 percentile as lower cap
outlier_treat2<- function(x)
{
  p99<-quantile(x,na.rm=T,p=c(0.99))
  p1<-quantile(x,na.rm=T,p=c(0.01))
  x[x>p99]<- p99
  x[x<p1]<-p1
  return (x)
}
vars<- c("LotFrontage","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF2","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars","WoodDeckSF","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","YrSold","LandSlope","ExterCond","GarageQual","GarageCond","PoolQC")
num_data[vars]<-lapply(num_data[vars],FUN=outlier_treat2)
num_housesale_data<- t(data.frame(lapply(num_data[vars],FUN=myfun3)))
write.csv(num_housesale_data,file="D:/data science/House Prices Advanced Regression Techniques/descriptive.csv")

##Outlier Treatment with 98 percentile as upper cap and 1 percentile as lower cap
outlier_treat3<- function(x)
{
  p98<-quantile(x,na.rm=T,p=c(0.98))
  p1<-quantile(x,na.rm=T,p=c(0.01))
  x[x>p98]<- p98
  x[x<p1]<-p1
  return (x)
}
vars1<- c("LotArea","BsmtFinSF1","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","LowQualFinSF","MiscVal","SalePrice","GrLivArea","BsmtUnfSF","GarageArea","OpenPorchSF")
num_data[vars1]<-lapply(num_data[vars1],FUN=outlier_treat3)
num_housesale_data<- t(data.frame(lapply(num_data[vars1],FUN=myfun3)))
write.csv(num_housesale_data,file="D:/data science/House Prices Advanced Regression Techniques/descriptive.csv")

num_housesale_data<- t(data.frame(lapply(num_data,FUN=myfun3)))
write.csv(num_housesale_data,file="D:/data science/House Prices Advanced Regression Techniques/descriptive.csv") 

#Missing value treatment for numerical variables
miss_impute1<- function(x)
{
  x[is.na(x)]<-quantile(x,na.rm=T,p=c(0.50))
  return (x)
}
vars3<- c("GarageArea","GarageCars","GarageYrBlt","BsmtHalfBath","BsmtFullBath","TotalBsmtSF","BsmtUnfSF","BsmtFinSF2","BsmtFinSF1","MasVnrArea","LotFrontage")
num_data[vars3]<-apply(num_data[vars3],2,FUN=miss_impute1)

#Combine num_data and fac_data
house_sale_prediction<- cbind(num_data,fac_data)

#Separate train and test again into individuals
house_sale_prediction_dev<- house_sale_prediction[!is.na(house_sale_prediction$SalePrice),]
house_sale_prediction_val<- house_sale_prediction[is.na(house_sale_prediction$SalePrice),]

#Check the significance of categorical variables with the response variable using anova test
anova_func<- function(x)
{
  anova_out<- aov(SalePrice~x,data=house_sale_prediction_dev)
  o<-summary(anova_out)
  return (o)
}
vars4<- c("MSSubClass","MSZoning","Street","Alley","LandContour","LotConfig","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","Foundation","Heating","CentralAir","Electrical","GarageType","PavedDrive","Fence","MiscFeature","MoSold","SaleType","SaleCondition")
lapply(house_sale_prediction_dev[vars4],FUN=anova_func)

#Remove the insignificant categorical variables from the dataset
house_sale_prediction_dev$Street <- NULL
house_sale_prediction_dev$MoSold <- NULL

#Assumptions Check-Normal distribution of dependent variable
hist(house_sale_prediction_dev$SalePrice)
#The response variables is positively skewed. So make it normal using log transformation.

#Make the response variable normal
house_sale_prediction_dev$ln_SalePrice<- log(house_sale_prediction_dev$SalePrice)
hist(house_sale_prediction_dev$ln_SalePrice)

#Assumptions Check- Linearity Check
house_sale_prediction_dev1<- house_sale_prediction_dev[,c("ln_SalePrice","LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","MiscVal","YrSold","LotShape","LandSlope","ExterQual","ExterCond","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","HeatingQC","KitchenQual","Functional","FireplaceQu","GarageFinish","GarageQual","GarageCond","PoolQC")]
correlation_matrix<- cor(house_sale_prediction_dev1,use = "complete.obs")
print(correlation_matrix)
library(xtable)
print(xtable(correlation_matrix), type="html")

#Drop the numerical variables which does not have any linear relationship with the response variable
house_sale_prediction_dev$OverallCond<- NULL
house_sale_prediction_dev$BsmtFinSF2<- NULL
house_sale_prediction_dev$BsmtHalfBath<- NULL
house_sale_prediction_dev$X3SsnPorch<- NULL
house_sale_prediction_dev$MiscVal<- NULL
house_sale_prediction_dev$YrSold<- NULL
house_sale_prediction_dev$LandSlope<- NULL
house_sale_prediction_dev$ExterCond<- NULL
house_sale_prediction_dev$BsmtFinType2<- NULL

#Data visualization to check zero variance of some numerical variables
ggplot(data  = house_sale_prediction_dev, aes( x = LowQualFinSF)) + geom_histogram(binwidth = 0.05)
ggplot(data  = house_sale_prediction_dev, aes( x = PoolArea)) + geom_histogram(binwidth = 0.05)
ggplot(data  = house_sale_prediction_dev, aes( x = PoolQC)) + geom_histogram(binwidth = 0.05)
#These 3 numerical variables give zero variance. So, drop it off.
house_sale_prediction_dev$PoolArea<- NULL
house_sale_prediction_dev$PoolQC<- NULL
house_sale_prediction_dev$LowQualFinSF<- NULL

#Multicollinearity check
numericVars <- which(sapply(house_sale_prediction_dev, is.numeric))
house_sale_prediction_dev2 <- house_sale_prediction_dev[,numericVars]
correlation_matrix<- cor(house_sale_prediction_dev2,use = "complete.obs")
print(correlation_matrix)
library(xtable)
print(xtable(correlation_matrix), type="html")

#Drop the highly correlated independent variables
#LotFrontage and LotArea are correlated to each other
#OverQuality is highly correlated with some other quality variables
house_sale_prediction_dev$LotFrontage<- NULL
house_sale_prediction_dev$YearBuilt<- NULL
house_sale_prediction_dev$YearRemodAdd<- NULL
house_sale_prediction_dev$GarageCars<- NULL
house_sale_prediction_dev$ExterQual<- NULL
house_sale_prediction_dev$BsmtQual<- NULL
house_sale_prediction_dev$KitchenQual<- NULL
house_sale_prediction_dev$GarageArea<- NULL
house_sale_prediction_dev$GarageFinish<- NULL
house_sale_prediction_dev$BsmtFinSF1<- NULL
house_sale_prediction_dev$X1stFlrSF<- NULL
house_sale_prediction_dev$X2ndFlrSF<- NULL
house_sale_prediction_dev$FullBath<- NULL
house_sale_prediction_dev$BedroomAbvGr<- NULL
house_sale_prediction_dev$Fireplaces<- NULL
house_sale_prediction_dev$TotalBsmtSF<- NULL
house_sale_prediction_dev$GrLivArea<- NULL


#Create dummies for categorical variables which are significant
library(fastDummies)
house_sale_prediction_dev<-dummy_cols(house_sale_prediction_dev,select_columns = c("MSSubClass","MSZoning","Alley","LandContour","LotConfig","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","Foundation","Heating","Electrical","GarageType","PavedDrive","Fence","MiscFeature","SaleType","SaleCondition"),remove_first_dummy = TRUE)
write.csv(house_sale_prediction_dev,file="D:/data science/House Prices Advanced Regression Techniques/final.csv")

#Rename some of the categorical dummy variables
colnames(house_sale_prediction_dev)[colnames(house_sale_prediction_dev)=="Exterior1st_Wd Sdng"]<-"Exterior1st_Wd_Sdng"
colnames(house_sale_prediction_dev)[colnames(house_sale_prediction_dev)=="Exterior2nd_Wd Shng"]<-"Exterior2nd_Wd_Shng"
colnames(house_sale_prediction_dev)[colnames(house_sale_prediction_dev)=="Exterior2nd_Wd Sdng"]<-"Exterior2nd_Wd_Sdng"
colnames(house_sale_prediction_dev)[colnames(house_sale_prediction_dev)=="Exterior2nd_Brk Cmn"]<-"Exterior2nd_Brk_Cmn"
colnames(house_sale_prediction_dev)[colnames(house_sale_prediction_dev)=="GarageType_No Garage"]<-"GarageType_No_Garage"

#Convert all the dummies from integer data type to factors
house_sale_prediction_dev$MSSubClass_20<-as.factor(house_sale_prediction_dev$MSSubClass_20)
house_sale_prediction_dev$MSSubClass_70<-as.factor(house_sale_prediction_dev$MSSubClass_70)
house_sale_prediction_dev$MSSubClass_50<-as.factor(house_sale_prediction_dev$MSSubClass_50)
house_sale_prediction_dev$MSSubClass_190<-as.factor(house_sale_prediction_dev$MSSubClass_190)
house_sale_prediction_dev$MSSubClass_45<-as.factor(house_sale_prediction_dev$MSSubClass_45)
house_sale_prediction_dev$MSSubClass_90<-as.factor(house_sale_prediction_dev$MSSubClass_90)
house_sale_prediction_dev$MSSubClass_120<-as.factor(house_sale_prediction_dev$MSSubClass_120)
house_sale_prediction_dev$MSSubClass_30<-as.factor(house_sale_prediction_dev$MSSubClass_30)
house_sale_prediction_dev$MSSubClass_85<-as.factor(house_sale_prediction_dev$MSSubClass_85)
house_sale_prediction_dev$MSSubClass_80<-as.factor(house_sale_prediction_dev$MSSubClass_80)
house_sale_prediction_dev$MSSubClass_160<-as.factor(house_sale_prediction_dev$MSSubClass_160)
house_sale_prediction_dev$MSSubClass_75<-as.factor(house_sale_prediction_dev$MSSubClass_75)
house_sale_prediction_dev$MSSubClass_180<-as.factor(house_sale_prediction_dev$MSSubClass_180)
house_sale_prediction_dev$MSSubClass_40<-as.factor(house_sale_prediction_dev$MSSubClass_40)
house_sale_prediction_dev$MSZoning_RM<-as.factor(house_sale_prediction_dev$MSZoning_RM)
house_sale_prediction_dev$MSZoning_C (all)<-as.factor(house_sale_prediction_dev$MSZoning_C (all))
house_sale_prediction_dev$MSZoning_FV<-as.factor(house_sale_prediction_dev$MSZoning_FV)
house_sale_prediction_dev$MSZoning_RH<-as.factor(house_sale_prediction_dev$MSZoning_RH)
house_sale_prediction_dev$Alley_Grvl<-as.factor(house_sale_prediction_dev$Alley_Grvl)
house_sale_prediction_dev$Alley_Pave<-as.factor(house_sale_prediction_dev$Alley_Pave)
house_sale_prediction_dev$LandContour_Bnk<-as.factor(house_sale_prediction_dev$LandContour_Bnk)
house_sale_prediction_dev$LandContour_Low<-as.factor(house_sale_prediction_dev$LandContour_Low)
house_sale_prediction_dev$LandContour_HLS<-as.factor(house_sale_prediction_dev$LandContour_HLS)
house_sale_prediction_dev$LotConfig_FR2<-as.factor(house_sale_prediction_dev$LotConfig_FR2)
house_sale_prediction_dev$LotConfig_Corner<-as.factor(house_sale_prediction_dev$LotConfig_Corner)
house_sale_prediction_dev$LotConfig_CulDSac<-as.factor(house_sale_prediction_dev$LotConfig_CulDSac)
house_sale_prediction_dev$LotConfig_FR3<-as.factor(house_sale_prediction_dev$LotConfig_FR3)
house_sale_prediction_dev$Neighborhood_Veenker<-as.factor(house_sale_prediction_dev$Neighborhood_Veenker)
house_sale_prediction_dev$Neighborhood_Crawfor<-as.factor(house_sale_prediction_dev$Neighborhood_Crawfor)
house_sale_prediction_dev$Neighborhood_NoRidge<-as.factor(house_sale_prediction_dev$Neighborhood_NoRidge)
house_sale_prediction_dev$Neighborhood_Mitchel<-as.factor(house_sale_prediction_dev$Neighborhood_Mitchel)
house_sale_prediction_dev$Neighborhood_Somerst<-as.factor(house_sale_prediction_dev$Neighborhood_Somerst)
house_sale_prediction_dev$Neighborhood_NWAmes<-as.factor(house_sale_prediction_dev$Neighborhood_NWAmes)
house_sale_prediction_dev$Neighborhood_OldTown<-as.factor(house_sale_prediction_dev$Neighborhood_OldTown)
house_sale_prediction_dev$Neighborhood_BrkSide<-as.factor(house_sale_prediction_dev$Neighborhood_BrkSide)
house_sale_prediction_dev$Neighborhood_Sawyer<-as.factor(house_sale_prediction_dev$Neighborhood_Sawyer)
house_sale_prediction_dev$Neighborhood_NridgHt<-as.factor(house_sale_prediction_dev$Neighborhood_NridgHt)
house_sale_prediction_dev$Neighborhood_NAmes<-as.factor(house_sale_prediction_dev$Neighborhood_NAmes)
house_sale_prediction_dev$Neighborhood_SawyerW<-as.factor(house_sale_prediction_dev$Neighborhood_SawyerW)
house_sale_prediction_dev$Neighborhood_IDOTRR<-as.factor(house_sale_prediction_dev$Neighborhood_IDOTRR)
house_sale_prediction_dev$Neighborhood_MeadowV<-as.factor(house_sale_prediction_dev$Neighborhood_MeadowV)
house_sale_prediction_dev$Neighborhood_Edwards<-as.factor(house_sale_prediction_dev$Neighborhood_Edwards)
house_sale_prediction_dev$Neighborhood_Timber<-as.factor(house_sale_prediction_dev$Neighborhood_Timber)
house_sale_prediction_dev$Neighborhood_Gilbert<-as.factor(house_sale_prediction_dev$Neighborhood_Gilbert)
house_sale_prediction_dev$Neighborhood_StoneBr<-as.factor(house_sale_prediction_dev$Neighborhood_StoneBr)
house_sale_prediction_dev$Neighborhood_ClearCr<-as.factor(house_sale_prediction_dev$Neighborhood_ClearCr)
house_sale_prediction_dev$Neighborhood_NPkVill<-as.factor(house_sale_prediction_dev$Neighborhood_NPkVill)
house_sale_prediction_dev$Neighborhood_Blmngtn<-as.factor(house_sale_prediction_dev$Neighborhood_Blmngtn)
house_sale_prediction_dev$Neighborhood_BrDale<-as.factor(house_sale_prediction_dev$Neighborhood_BrDale)
house_sale_prediction_dev$Neighborhood_SWISU<-as.factor(house_sale_prediction_dev$Neighborhood_SWISU)
house_sale_prediction_dev$Neighborhood_Blueste<-as.factor(house_sale_prediction_dev$Neighborhood_Blueste)
house_sale_prediction_dev$Condition1_Feedr<-as.factor(house_sale_prediction_dev$Condition1_Feedr)
house_sale_prediction_dev$Condition1_PosN<-as.factor(house_sale_prediction_dev$Condition1_PosN)
house_sale_prediction_dev$Condition1_Artery<-as.factor(house_sale_prediction_dev$Condition1_Artery)
house_sale_prediction_dev$Condition1_RRAe<-as.factor(house_sale_prediction_dev$Condition1_RRAe)
house_sale_prediction_dev$Condition1_RRNn<-as.factor(house_sale_prediction_dev$Condition1_RRNn)
house_sale_prediction_dev$Condition1_RRAn<-as.factor(house_sale_prediction_dev$Condition1_RRAn)
house_sale_prediction_dev$Condition1_PosA<-as.factor(house_sale_prediction_dev$Condition1_PosA)
house_sale_prediction_dev$Condition1_RRNe<-as.factor(house_sale_prediction_dev$Condition1_RRNe)
house_sale_prediction_dev$Condition2_Artery<-as.factor(house_sale_prediction_dev$Condition2_Artery)
house_sale_prediction_dev$Condition2_RRNn<-as.factor(house_sale_prediction_dev$Condition2_RRNn)
house_sale_prediction_dev$Condition2_Feedr<-as.factor(house_sale_prediction_dev$Condition2_Feedr)
house_sale_prediction_dev$Condition2_PosN<-as.factor(house_sale_prediction_dev$Condition2_PosN)
house_sale_prediction_dev$Condition2_PosA<-as.factor(house_sale_prediction_dev$Condition2_PosA)
house_sale_prediction_dev$Condition2_RRAn<-as.factor(house_sale_prediction_dev$Condition2_RRAn)
house_sale_prediction_dev$Condition2_RRAe<-as.factor(house_sale_prediction_dev$Condition2_RRAe)
house_sale_prediction_dev$BldgType_2fmCon<-as.factor(house_sale_prediction_dev$BldgType_2fmCon)
house_sale_prediction_dev$BldgType_Duplex<-as.factor(house_sale_prediction_dev$BldgType_Duplex)
house_sale_prediction_dev$BldgType_TwnhsE<-as.factor(house_sale_prediction_dev$BldgType_TwnhsE)
house_sale_prediction_dev$BldgType_Twnhs<-as.factor(house_sale_prediction_dev$BldgType_Twnhs)
house_sale_prediction_dev$HouseStyle_1Story<-as.factor(house_sale_prediction_dev$HouseStyle_1Story)
house_sale_prediction_dev$HouseStyle_1.5Fin<-as.factor(house_sale_prediction_dev$HouseStyle_1.5Fin)
house_sale_prediction_dev$HouseStyle_1.5Unf<-as.factor(house_sale_prediction_dev$HouseStyle_1.5Unf)
house_sale_prediction_dev$HouseStyle_SFoyer<-as.factor(house_sale_prediction_dev$HouseStyle_SFoyer)
house_sale_prediction_dev$HouseStyle_SLvl<-as.factor(house_sale_prediction_dev$HouseStyle_SLvl)
house_sale_prediction_dev$HouseStyle_2.5Unf<-as.factor(house_sale_prediction_dev$HouseStyle_2.5Unf)
house_sale_prediction_dev$HouseStyle_2.5Fin<-as.factor(house_sale_prediction_dev$HouseStyle_2.5Fin)
house_sale_prediction_dev$RoofStyle_Hip<-as.factor(house_sale_prediction_dev$RoofStyle_Hip)
house_sale_prediction_dev$RoofStyle_Gambrel<-as.factor(house_sale_prediction_dev$RoofStyle_Gambrel)
house_sale_prediction_dev$RoofStyle_Mansard<-as.factor(house_sale_prediction_dev$RoofStyle_Mansard)
house_sale_prediction_dev$RoofStyle_Flat<-as.factor(house_sale_prediction_dev$RoofStyle_Flat)
house_sale_prediction_dev$RoofStyle_Shed<-as.factor(house_sale_prediction_dev$RoofStyle_Shed)
house_sale_prediction_dev$RoofMatl_WdShngl<-as.factor(house_sale_prediction_dev$RoofMatl_WdShngl)
house_sale_prediction_dev$RoofMatl_Metal<-as.factor(house_sale_prediction_dev$RoofMatl_Metal)
house_sale_prediction_dev$RoofMatl_WdShake<-as.factor(house_sale_prediction_dev$RoofMatl_WdShake)
house_sale_prediction_dev$RoofMatl_Membran<-as.factor(house_sale_prediction_dev$RoofMatl_Membran)
house_sale_prediction_dev$RoofMatl_Tar&Grv<-as.factor(house_sale_prediction_dev$RoofMatl_Tar&Grv)
house_sale_prediction_dev$RoofMatl_Roll<-as.factor(house_sale_prediction_dev$RoofMatl_Roll)
house_sale_prediction_dev$RoofMatl_ClyTile<-as.factor(house_sale_prediction_dev$RoofMatl_ClyTile)
house_sale_prediction_dev$Exterior1st_MetalSd<-as.factor(house_sale_prediction_dev$Exterior1st_MetalSd)
house_sale_prediction_dev$Exterior1st_Wd_Sdng<-as.factor(house_sale_prediction_dev$Exterior1st_Wd_Sdng)
house_sale_prediction_dev$Exterior1st_HdBoard<-as.factor(house_sale_prediction_dev$Exterior1st_HdBoard)
house_sale_prediction_dev$Exterior1st_BrkFace<-as.factor(house_sale_prediction_dev$Exterior1st_BrkFace)
house_sale_prediction_dev$Exterior1st_WdShing<-as.factor(house_sale_prediction_dev$Exterior1st_WdShing)
house_sale_prediction_dev$Exterior1st_CemntBd<-as.factor(house_sale_prediction_dev$Exterior1st_CemntBd)
house_sale_prediction_dev$Exterior1st_Plywood<-as.factor(house_sale_prediction_dev$Exterior1st_Plywood)
house_sale_prediction_dev$Exterior1st_AsbShng<-as.factor(house_sale_prediction_dev$Exterior1st_AsbShng)
house_sale_prediction_dev$Exterior1st_Stucco<-as.factor(house_sale_prediction_dev$Exterior1st_Stucco)
house_sale_prediction_dev$Exterior1st_BrkComm<-as.factor(house_sale_prediction_dev$Exterior1st_BrkComm)
house_sale_prediction_dev$Exterior1st_AsphShn<-as.factor(house_sale_prediction_dev$Exterior1st_AsphShn)
house_sale_prediction_dev$Exterior1st_Stone<-as.factor(house_sale_prediction_dev$Exterior1st_Stone)
house_sale_prediction_dev$Exterior1st_ImStucc<-as.factor(house_sale_prediction_dev$Exterior1st_ImStucc)
house_sale_prediction_dev$Exterior1st_CBlock<-as.factor(house_sale_prediction_dev$Exterior1st_CBlock)
house_sale_prediction_dev$Exterior2nd_MetalSd<-as.factor(house_sale_prediction_dev$Exterior2nd_MetalSd)
house_sale_prediction_dev$Exterior2nd_Wd_Shng<-as.factor(house_sale_prediction_dev$Exterior2nd_Wd_Shng)
house_sale_prediction_dev$Exterior2nd_HdBoard<-as.factor(house_sale_prediction_dev$Exterior2nd_HdBoard)
house_sale_prediction_dev$Exterior2nd_Plywood<-as.factor(house_sale_prediction_dev$Exterior2nd_Plywood)
house_sale_prediction_dev$Exterior2nd_Wd_Sdng<-as.factor(house_sale_prediction_dev$Exterior2nd_Wd_Sdng)
house_sale_prediction_dev$Exterior2nd_CmentBd<-as.factor(house_sale_prediction_dev$Exterior2nd_CmentBd)
house_sale_prediction_dev$Exterior2nd_BrkFace<-as.factor(house_sale_prediction_dev$Exterior2nd_BrkFace)
house_sale_prediction_dev$Exterior2nd_Stucco<-as.factor(house_sale_prediction_dev$Exterior2nd_Stucco)
house_sale_prediction_dev$Exterior2nd_AsbShng<-as.factor(house_sale_prediction_dev$Exterior2nd_AsbShng)
house_sale_prediction_dev$Exterior2nd_Brk_Cmn<-as.factor(house_sale_prediction_dev$Exterior2nd_Brk_Cmn)
house_sale_prediction_dev$Exterior2nd_ImStucc<-as.factor(house_sale_prediction_dev$Exterior2nd_ImStucc)
house_sale_prediction_dev$Exterior2nd_AsphShn<-as.factor(house_sale_prediction_dev$Exterior2nd_AsphShn)
house_sale_prediction_dev$Exterior2nd_Stone<-as.factor(house_sale_prediction_dev$Exterior2nd_Stone)
house_sale_prediction_dev$Exterior2nd_Other<-as.factor(house_sale_prediction_dev$Exterior2nd_Other)
house_sale_prediction_dev$Exterior2nd_CBlock<-as.factor(house_sale_prediction_dev$Exterior2nd_CBlock)
house_sale_prediction_dev$MasVnrType_None<-as.factor(house_sale_prediction_dev$MasVnrType_None)
house_sale_prediction_dev$MasVnrType_Stone<-as.factor(house_sale_prediction_dev$MasVnrType_Stone)
house_sale_prediction_dev$MasVnrType_BrkCmn<-as.factor(house_sale_prediction_dev$MasVnrType_BrkCmn)
house_sale_prediction_dev$Foundation_CBlock<-as.factor(house_sale_prediction_dev$Foundation_CBlock)
house_sale_prediction_dev$Foundation_BrkTil<-as.factor(house_sale_prediction_dev$Foundation_BrkTil)
house_sale_prediction_dev$Foundation_Wood<-as.factor(house_sale_prediction_dev$Foundation_Wood)
house_sale_prediction_dev$Foundation_Slab<-as.factor(house_sale_prediction_dev$Foundation_Slab)
house_sale_prediction_dev$Foundation_Stone<-as.factor(house_sale_prediction_dev$Foundation_Stone)
house_sale_prediction_dev$Heating_GasW<-as.factor(house_sale_prediction_dev$Heating_GasW)
house_sale_prediction_dev$Heating_Grav<-as.factor(house_sale_prediction_dev$Heating_Grav)
house_sale_prediction_dev$Heating_Wall<-as.factor(house_sale_prediction_dev$Heating_Wall)
house_sale_prediction_dev$Heating_OthW<-as.factor(house_sale_prediction_dev$Heating_OthW)
house_sale_prediction_dev$Heating_Floor<-as.factor(house_sale_prediction_dev$Heating_Floor)
house_sale_prediction_dev$Electrical_FuseF<-as.factor(house_sale_prediction_dev$Electrical_FuseF)
house_sale_prediction_dev$Electrical_FuseA<-as.factor(house_sale_prediction_dev$Electrical_FuseA)
house_sale_prediction_dev$Electrical_FuseP<-as.factor(house_sale_prediction_dev$Electrical_FuseP)
house_sale_prediction_dev$Electrical_Mix<-as.factor(house_sale_prediction_dev$Electrical_Mix)
house_sale_prediction_dev$GarageType_Detchd<-as.factor(house_sale_prediction_dev$GarageType_Detchd)
house_sale_prediction_dev$GarageType_BuiltIn<-as.factor(house_sale_prediction_dev$GarageType_BuiltIn)
house_sale_prediction_dev$GarageType_CarPort<-as.factor(house_sale_prediction_dev$GarageType_CarPort)
house_sale_prediction_dev$GarageType_No_Garage<-as.factor(house_sale_prediction_dev$GarageType_No_Garage)
house_sale_prediction_dev$GarageType_Basment<-as.factor(house_sale_prediction_dev$GarageType_Basment)
house_sale_prediction_dev$GarageType_2Types<-as.factor(house_sale_prediction_dev$GarageType_2Types)
house_sale_prediction_dev$PavedDrive_N<-as.factor(house_sale_prediction_dev$PavedDrive_N)
house_sale_prediction_dev$PavedDrive_P<-as.factor(house_sale_prediction_dev$PavedDrive_P)
house_sale_prediction_dev$Fence_MnPrv<-as.factor(house_sale_prediction_dev$Fence_MnPrv)
house_sale_prediction_dev$Fence_GdWo<-as.factor(house_sale_prediction_dev$Fence_GdWo)
house_sale_prediction_dev$Fence_GdPrv<-as.factor(house_sale_prediction_dev$Fence_GdPrv)
house_sale_prediction_dev$Fence_MnWw<-as.factor(house_sale_prediction_dev$Fence_MnWw)
house_sale_prediction_dev$MiscFeature_Shed<-as.factor(house_sale_prediction_dev$MiscFeature_Shed)
house_sale_prediction_dev$MiscFeature_Gar2<-as.factor(house_sale_prediction_dev$MiscFeature_Gar2)
house_sale_prediction_dev$MiscFeature_Othr<-as.factor(house_sale_prediction_dev$MiscFeature_Othr)
house_sale_prediction_dev$MiscFeature_TenC<-as.factor(house_sale_prediction_dev$MiscFeature_TenC)
house_sale_prediction_dev$SaleType_New<-as.factor(house_sale_prediction_dev$SaleType_New)
house_sale_prediction_dev$SaleType_COD<-as.factor(house_sale_prediction_dev$SaleType_COD)
house_sale_prediction_dev$SaleType_ConLD<-as.factor(house_sale_prediction_dev$SaleType_ConLD)
house_sale_prediction_dev$SaleType_ConLI<-as.factor(house_sale_prediction_dev$SaleType_ConLI)
house_sale_prediction_dev$SaleType_CWD<-as.factor(house_sale_prediction_dev$SaleType_CWD)
house_sale_prediction_dev$SaleType_ConLw<-as.factor(house_sale_prediction_dev$SaleType_ConLw)
house_sale_prediction_dev$SaleType_Con<-as.factor(house_sale_prediction_dev$SaleType_Con)
house_sale_prediction_dev$SaleType_Oth<-as.factor(house_sale_prediction_dev$SaleType_Oth)
house_sale_prediction_dev$SaleCondition_Abnorml<-as.factor(house_sale_prediction_dev$SaleCondition_Abnorml)
house_sale_prediction_dev$SaleCondition_Partial<-as.factor(house_sale_prediction_dev$SaleCondition_Partial)
house_sale_prediction_dev$SaleCondition_AdjLand<-as.factor(house_sale_prediction_dev$SaleCondition_AdjLand)
house_sale_prediction_dev$SaleCondition_Alloca<-as.factor(house_sale_prediction_dev$SaleCondition_Alloca)
house_sale_prediction_dev$SaleCondition_Family<-as.factor(house_sale_prediction_dev$SaleCondition_Family)

#Anova test for dummy categorical variables
anova_func<- function(x)
{
  anova_out<- aov(SalePrice~x,data=house_sale_prediction_dev)
  o<-summary(anova_out)
  return (o)
}
vars4<- c("MSSubClass_20","MSSubClass_70","MSSubClass_50","MSSubClass_190","MSSubClass_45","MSSubClass_90","MSSubClass_120","MSSubClass_30","MSSubClass_85","MSSubClass_80","MSSubClass_160","MSSubClass_75","MSSubClass_180","MSSubClass_40","MSZoning_RM","MSZoning_C (all)","MSZoning_FV","MSZoning_RH","Alley_Grvl","Alley_Pave","LandContour_Bnk","LandContour_Low","LandContour_HLS","LotConfig_FR2","LotConfig_Corner","LotConfig_CulDSac","LotConfig_FR3","Neighborhood_Veenker","Neighborhood_Crawfor","Neighborhood_NoRidge","Neighborhood_Mitchel","Neighborhood_Somerst","Neighborhood_NWAmes","Neighborhood_OldTown","Neighborhood_BrkSide","Neighborhood_Sawyer","Neighborhood_NridgHt","Neighborhood_NAmes","Neighborhood_SawyerW","Neighborhood_IDOTRR","Neighborhood_MeadowV","Neighborhood_Edwards","Neighborhood_Timber","Neighborhood_Gilbert","Neighborhood_StoneBr","Neighborhood_ClearCr","Neighborhood_NPkVill","Neighborhood_Blmngtn","Neighborhood_BrDale","Neighborhood_SWISU","Neighborhood_Blueste","Condition1_Feedr","Condition1_PosN","Condition1_Artery","Condition1_RRAe","Condition1_RRNn","Condition1_RRAn","Condition1_PosA","Condition1_RRNe","Condition2_Artery","Condition2_RRNn","Condition2_Feedr","Condition2_PosN","Condition2_PosA","Condition2_RRAn","Condition2_RRAe","BldgType_2fmCon","BldgType_Duplex","BldgType_TwnhsE","BldgType_Twnhs","HouseStyle_1Story","HouseStyle_1.5Fin","HouseStyle_1.5Unf","HouseStyle_SFoyer","HouseStyle_SLvl","HouseStyle_2.5Unf","HouseStyle_2.5Fin","RoofStyle_Hip","RoofStyle_Gambrel","RoofStyle_Mansard","RoofStyle_Flat","RoofStyle_Shed","RoofMatl_WdShngl","RoofMatl_Metal","RoofMatl_WdShake","RoofMatl_Membran","RoofMatl_Tar&Grv","RoofMatl_Roll","RoofMatl_ClyTile","Exterior1st_MetalSd","Exterior1st_Wd_Sdng","Exterior1st_HdBoard","Exterior1st_BrkFace","Exterior1st_WdShing","Exterior1st_CemntBd","Exterior1st_Plywood","Exterior1st_AsbShng","Exterior1st_Stucco","Exterior1st_BrkComm","Exterior1st_AsphShn","Exterior1st_Stone","Exterior1st_ImStucc","Exterior1st_CBlock","Exterior2nd_MetalSd","Exterior2nd_Wd_Shng","Exterior2nd_HdBoard","Exterior2nd_Plywood","Exterior2nd_Wd_Sdng","Exterior2nd_CmentBd","Exterior2nd_BrkFace","Exterior2nd_Stucco","Exterior2nd_AsbShng","Exterior2nd_Brk_Cmn","Exterior2nd_ImStucc","Exterior2nd_AsphShn","Exterior2nd_Stone","Exterior2nd_Other","Exterior2nd_CBlock","MasVnrType_None","MasVnrType_Stone","MasVnrType_BrkCmn","Foundation_CBlock","Foundation_BrkTil","Foundation_Wood","Foundation_Slab","Foundation_Stone","Heating_GasW","Heating_Grav","Heating_Wall","Heating_OthW","Heating_Floor","Electrical_FuseF","Electrical_FuseA","Electrical_FuseP","Electrical_Mix","GarageType_Detchd","GarageType_BuiltIn","GarageType_CarPort","GarageType_No_Garage","GarageType_Basment","GarageType_2Types","PavedDrive_N","PavedDrive_P","Fence_MnPrv","Fence_GdWo","Fence_GdPrv","Fence_MnWw","MiscFeature_Shed","MiscFeature_Gar2","MiscFeature_Othr","MiscFeature_TenC","SaleType_New","SaleType_COD","SaleType_ConLD","SaleType_ConLI","SaleType_CWD","SaleType_ConLw","SaleType_Con","SaleType_Oth","SaleCondition_Abnorml","SaleCondition_Partial","SaleCondition_AdjLand","SaleCondition_Alloca","SaleCondition_Family")
lapply(house_sale_prediction_dev[vars4],FUN=anova_func)

#Remove the insignificant dummy variables
house_sale_prediction_dev$SaleCondition_Family<- NULL
house_sale_prediction_dev$SaleCondition_Alloca<- NULL
house_sale_prediction_dev$SaleCondition_AdjLand<- NULL
house_sale_prediction_dev$SaleType_Oth<- NULL
house_sale_prediction_dev$SaleType_Con<- NULL
house_sale_prediction_dev$SaleType_ConLw<- NULL
house_sale_prediction_dev$SaleType_CWD<- NULL
house_sale_prediction_dev$SaleType_ConLI<- NULL
house_sale_prediction_dev$SaleType_ConLD<- NULL
house_sale_prediction_dev$MiscFeature_TenC<- NULL
house_sale_prediction_dev$MiscFeature_Othr<- NULL
house_sale_prediction_dev$MiscFeature_Gar2<- NULL
house_sale_prediction_dev$Fence_MnWw<- NULL
house_sale_prediction_dev$Fence_GdPrv<- NULL
house_sale_prediction_dev$GarageType_2Types<- NULL
house_sale_prediction_dev$GarageType_Basment<- NULL
house_sale_prediction_dev$Electrical_Mix<- NULL
house_sale_prediction_dev$Electrical_FuseP<- NULL
house_sale_prediction_dev$Heating_Floor<- NULL
house_sale_prediction_dev$Heating_OthW<- NULL
house_sale_prediction_dev$Heating_GasW<- NULL
house_sale_prediction_dev$Foundation_Stone<- NULL
house_sale_prediction_dev$Foundation_Wood<- NULL
house_sale_prediction_dev$MasVnrType_BrkCmn<- NULL
house_sale_prediction_dev$Exterior2nd_CBlock<- NULL
house_sale_prediction_dev$Exterior2nd_Other<- NULL
house_sale_prediction_dev$Exterior2nd_Stone<- NULL
house_sale_prediction_dev$Exterior2nd_AsphShn<- NULL
house_sale_prediction_dev$Exterior2nd_ImStucc<- NULL
house_sale_prediction_dev$Exterior2nd_Brk_Cmn<- NULL
house_sale_prediction_dev$Exterior2nd_Stucco<- NULL
house_sale_prediction_dev$Exterior2nd_BrkFace<- NULL
house_sale_prediction_dev$Exterior2nd_Plywood<- NULL
house_sale_prediction_dev$Exterior2nd_Wd_Shng<- NULL
house_sale_prediction_dev$Exterior1st_CBlock<- NULL
house_sale_prediction_dev$Exterior1st_ImStucc<- NULL
house_sale_prediction_dev$Exterior1st_Stone<- NULL
house_sale_prediction_dev$Exterior1st_AsphShn<- NULL
house_sale_prediction_dev$Exterior1st_BrkComm<- NULL
house_sale_prediction_dev$Exterior1st_Stucco<- NULL
house_sale_prediction_dev$Exterior1st_Plywood<- NULL
house_sale_prediction_dev$Exterior1st_WdShing<- NULL
house_sale_prediction_dev$Exterior1st_BrkFace<- NULL
house_sale_prediction_dev$RoofMatl_ClyTile<- NULL
house_sale_prediction_dev$RoofMatl_Roll<- NULL
house_sale_prediction_dev$RoofMatl_Tar_Grv <- NULL
house_sale_prediction_dev$RoofMatl_Membran<- NULL
house_sale_prediction_dev$RoofMatl_WdShake<- NULL
house_sale_prediction_dev$RoofMatl_Metal<- NULL
house_sale_prediction_dev$RoofStyle_Shed<- NULL
house_sale_prediction_dev$RoofStyle_Flat<- NULL
house_sale_prediction_dev$RoofStyle_Mansard<- NULL
house_sale_prediction_dev$RoofStyle_Gambrel<- NULL
house_sale_prediction_dev$HouseStyle_2.5Unf<- NULL
house_sale_prediction_dev$HouseStyle_SLvl<- NULL
house_sale_prediction_dev$BldgType_TwnhsE<- NULL
house_sale_prediction_dev$Condition2_RRAe<- NULL
house_sale_prediction_dev$Condition2_RRAn<- NULL
house_sale_prediction_dev$Condition2_PosA<- NULL
house_sale_prediction_dev$Condition2_PosN<- NULL
house_sale_prediction_dev$Condition2_Feedr<- NULL
house_sale_prediction_dev$Condition2_RRNn<- NULL
house_sale_prediction_dev$Condition2_Artery<- NULL
house_sale_prediction_dev$Condition1_RRNe<- NULL
house_sale_prediction_dev$Condition1_PosA<- NULL
house_sale_prediction_dev$Condition1_RRAn<- NULL
house_sale_prediction_dev$Condition1_PosN<- NULL
house_sale_prediction_dev$Neighborhood_Blueste<- NULL
house_sale_prediction_dev$Neighborhood_Blmngtn<- NULL
house_sale_prediction_dev$Neighborhood_NPkVill<- NULL
house_sale_prediction_dev$Neighborhood_Gilbert<- NULL
house_sale_prediction_dev$Neighborhood_SawyerW<- NULL
house_sale_prediction_dev$Neighborhood_NWAmes<- NULL
house_sale_prediction_dev$Neighborhood_Mitchel<- NULL
house_sale_prediction_dev$LotConfig_FR3<- NULL
house_sale_prediction_dev$LotConfig_Corner<- NULL
house_sale_prediction_dev$LotConfig_FR2<- NULL
house_sale_prediction_dev$LandContour_Low<- NULL
house_sale_prediction_dev$Alley_Pave<- NULL
house_sale_prediction_dev$MSSubClass_40<- NULL
house_sale_prediction_dev$MSSubClass_75<- NULL
house_sale_prediction_dev$MSSubClass_80<- NULL
house_sale_prediction_dev$MSSubClass_85<- NULL
house_sale_prediction_dev$MSSubClass_70<- NULL
house_sale_prediction_dev$MSSubClass_20<- NULL

#ANOVA Test for other set of categorical variables
anova_func<- function(x)
{
  anova_out<- aov(SalePrice~x,data=house_sale_prediction_dev)
  o<-summary(anova_out)
  return (o)
}
vars4<- c("MSSubClass_20","MSSubClass_50","MSSubClass_190")
lapply(house_sale_prediction_dev[vars4],FUN=anova_func)
write.csv(house_sale_prediction_dev,file="D:/data science/House Prices Advanced Regression Techniques/final.csv")

#Remove the parent categorical variables
house_sale_prediction_dev$MSSubClass<- NULL
house_sale_prediction_dev$MSZoning<- NULL
house_sale_prediction_dev$Alley<- NULL
house_sale_prediction_dev$LandContour<- NULL
house_sale_prediction_dev$LotConfig<- NULL
house_sale_prediction_dev$Neighborhood<- NULL
house_sale_prediction_dev$Condition1<- NULL
house_sale_prediction_dev$Condition2<- NULL
house_sale_prediction_dev$BldgType<- NULL
house_sale_prediction_dev$HouseStyle<- NULL
house_sale_prediction_dev$RoofStyle<- NULL
house_sale_prediction_dev$RoofMatl<- NULL
house_sale_prediction_dev$Foundation<- NULL
house_sale_prediction_dev$MasVnrType<- NULL
house_sale_prediction_dev$Exterior2nd<- NULL
house_sale_prediction_dev$Exterior1st<- NULL
house_sale_prediction_dev$PavedDrive<- NULL
house_sale_prediction_dev$GarageType<- NULL
house_sale_prediction_dev$Electrical<- NULL
house_sale_prediction_dev$Heating<- NULL
house_sale_prediction_dev$SaleCondition<- NULL
house_sale_prediction_dev$SaleType<- NULL
house_sale_prediction_dev$MiscFeature<- NULL
house_sale_prediction_dev$Fence<- NULL

#Check the significance of CentralAir variable using anova test
aa<-aov(SalePrice~CentralAir,data=house_sale_prediction_dev_new)
summary(aa)

#Label Encoding for CentralAir variable
house_sale_prediction_dev<- transform(house_sale_prediction_dev,CentralAir=ifelse(CentralAir=='Y',1,0))
house_sale_prediction_dev$CentralAir <- as.factor(house_sale_prediction_dev$CentralAir)

#Rename of some variables as per the requirement
colnames(house_sale_prediction_dev)[colnames(house_sale_prediction_dev)=="MSZoning_C..all."]<-"MSZoning_C_all"
colnames(house_sale_prediction_dev)[colnames(house_sale_prediction_dev)=="RoofMatl_Tar.Grv"]<-"RoofMatl_Tar_Grv"

#Split the data into training and testing dataset
smp_size<-floor(0.70*nrow(house_sale_prediction_dev))
set.seed(123)
train_ind<- sample(seq_len(nrow(house_sale_prediction_dev)),size=smp_size)
train<- house_sale_prediction_dev[train_ind,]
test<-house_sale_prediction_dev[-train_ind,]

#Building the model
fit<- lm(ln_SalePrice~LotArea+OverallQual+MasVnrArea+BsmtUnfSF+MSZoning_C_all+RoofMatl_Tar_Grv+BsmtFullBath+HalfBath+KitchenAbvGr+TotRmsAbvGrd+GarageYrBlt+WoodDeckSF+OpenPorchSF+EnclosedPorch+ScreenPorch+LotShape+BsmtCond+BsmtExposure+BsmtFinType1+HeatingQC+Functional+FireplaceQu+GarageQual+GarageCond+CentralAir+MSSubClass_50+MSSubClass_190+MSSubClass_45+MSSubClass_90+MSSubClass_120+MSSubClass_30+MSSubClass_160+MSSubClass_180+MSZoning_RM+MSZoning_FV+MSZoning_RH+Alley_Grvl+LandContour_Bnk+LandContour_HLS+LotConfig_CulDSac+Neighborhood_Veenker+Neighborhood_Crawfor+Neighborhood_NoRidge+Neighborhood_Somerst+Neighborhood_OldTown+Neighborhood_BrkSide+Neighborhood_Sawyer+Neighborhood_NridgHt+Neighborhood_NAmes+Neighborhood_IDOTRR+Neighborhood_MeadowV+Neighborhood_Edwards+Neighborhood_Timber+Neighborhood_StoneBr+Neighborhood_ClearCr+Neighborhood_BrDale+Neighborhood_SWISU+Condition1_Feedr+Condition1_Artery+Condition1_RRAe+Condition1_RRNn+BldgType_2fmCon+BldgType_Duplex+BldgType_Twnhs+HouseStyle_1Story+HouseStyle_1.5Fin+HouseStyle_1.5Unf+HouseStyle_SFoyer+HouseStyle_2.5Fin+RoofStyle_Hip+RoofMatl_WdShngl+Exterior1st_MetalSd+Exterior1st_Wd_Sdng+Exterior1st_HdBoard+Exterior1st_CemntBd+Exterior1st_AsbShng+Exterior2nd_MetalSd+Exterior2nd_HdBoard+Exterior2nd_Wd_Sdng+Exterior2nd_CmentBd+Exterior2nd_AsbShng+MasVnrType_None+MasVnrType_Stone+Foundation_CBlock+Foundation_BrkTil+Foundation_Slab+Heating_Grav+Heating_Wall+Electrical_FuseF+Electrical_FuseA+GarageType_Detchd+GarageType_BuiltIn+GarageType_CarPort+GarageType_No_Garage+PavedDrive_N+PavedDrive_P+Fence_MnPrv+Fence_GdWo+MiscFeature_Shed+SaleType_New+SaleType_COD+SaleCondition_Abnorml+SaleCondition_Partial,data=train)
summary(fit)
fit1<- lm(ln_SalePrice~LotArea+OverallQual+MasVnrArea+BsmtUnfSF+MSZoning_C_all+RoofMatl_Tar_Grv+BsmtFullBath+HalfBath+KitchenAbvGr+TotRmsAbvGrd+GarageYrBlt+WoodDeckSF+OpenPorchSF+EnclosedPorch+ScreenPorch+LotShape+BsmtCond+BsmtExposure+BsmtFinType1+HeatingQC+Functional+FireplaceQu+GarageQual+GarageCond+CentralAir+MSSubClass_50+MSSubClass_190+MSSubClass_45+MSSubClass_90+MSSubClass_120+MSSubClass_30+MSSubClass_160+MSSubClass_180+MSZoning_RM+MSZoning_FV+MSZoning_RH+Alley_Grvl+LandContour_Bnk+LandContour_HLS+LotConfig_CulDSac+Neighborhood_Veenker+Neighborhood_Crawfor+Neighborhood_NoRidge+Neighborhood_Somerst+Neighborhood_OldTown+Neighborhood_BrkSide+Neighborhood_Sawyer+Neighborhood_NridgHt+Neighborhood_NAmes+Neighborhood_IDOTRR+Neighborhood_MeadowV+Neighborhood_Edwards+Neighborhood_Timber+Neighborhood_StoneBr+Neighborhood_ClearCr+Neighborhood_BrDale+Neighborhood_SWISU+Condition1_Feedr+Condition1_Artery+Condition1_RRAe+Condition1_RRNn+BldgType_2fmCon+BldgType_Duplex+BldgType_Twnhs+HouseStyle_1Story+HouseStyle_1.5Fin+HouseStyle_1.5Unf+HouseStyle_SFoyer+HouseStyle_2.5Fin+RoofStyle_Hip+RoofMatl_WdShngl+Exterior1st_MetalSd+Exterior1st_Wd_Sdng+Exterior1st_HdBoard+Exterior1st_CemntBd+Exterior1st_AsbShng+Exterior2nd_MetalSd+Exterior2nd_HdBoard+Exterior2nd_Wd_Sdng+Exterior2nd_CmentBd+Exterior2nd_AsbShng+MasVnrType_None+MasVnrType_Stone+Foundation_CBlock+Foundation_BrkTil+Foundation_Slab+Heating_Grav+Heating_Wall+Electrical_FuseF+Electrical_FuseA+GarageType_Detchd+GarageType_BuiltIn+GarageType_CarPort+GarageType_No_Garage+PavedDrive_N+PavedDrive_P+Fence_MnPrv+Fence_GdWo+MiscFeature_Shed+SaleType_New+SaleType_COD+SaleCondition_Abnorml+SaleCondition_Partial,data=train)
library(MASS)
step<-stepAIC(fit1,direction="both")

#After step wise regression, following final predictors I got
fit2<- lm(ln_SalePrice ~ LotArea + OverallQual + MasVnrArea + MSZoning_C_all + 
            BsmtFullBath + HalfBath + TotRmsAbvGrd + GarageYrBlt + WoodDeckSF + 
            OpenPorchSF + ScreenPorch + LotShape + BsmtCond + BsmtExposure + 
            BsmtFinType1 + HeatingQC + FireplaceQu + GarageQual + CentralAir + 
            MSSubClass_190 + MSSubClass_120 + MSSubClass_30 + MSSubClass_160 + 
            MSZoning_RM + LotConfig_CulDSac + Neighborhood_Veenker + 
            Neighborhood_Crawfor + Neighborhood_NoRidge + Neighborhood_Somerst + 
            Neighborhood_BrkSide + Neighborhood_Sawyer + Neighborhood_NridgHt + 
            Neighborhood_NAmes + Neighborhood_MeadowV + Neighborhood_Edwards + 
            Neighborhood_Timber + Neighborhood_StoneBr + Neighborhood_ClearCr + 
            Neighborhood_BrDale + Condition1_Feedr + Condition1_Artery + 
            Condition1_RRAe + HouseStyle_1Story + HouseStyle_1.5Fin + 
            HouseStyle_1.5Unf + HouseStyle_2.5Fin + Exterior1st_Wd_Sdng + 
            Exterior1st_HdBoard + Exterior1st_CemntBd + Exterior2nd_Wd_Sdng + 
            Exterior2nd_CmentBd + Foundation_CBlock + Foundation_BrkTil + 
            GarageType_Detchd + GarageType_CarPort + PavedDrive_N + PavedDrive_P + 
            SaleType_New + SaleCondition_Abnorml + SaleCondition_Partial, data=train)
summary(fit2)

#Custom Control Parameters
custom<- trainControl(method = "repeatedcv", number=10,repeats = 5,verboseIter = T)

#Lasso Regression
set.seed(1234)
lasso<- train(ln_SalePrice ~ LotArea + OverallQual + MasVnrArea + MSZoning_C_all + 
                BsmtFullBath + HalfBath + TotRmsAbvGrd + GarageYrBlt + WoodDeckSF + 
                OpenPorchSF + ScreenPorch + LotShape + BsmtCond + BsmtExposure + 
                BsmtFinType1 + HeatingQC + FireplaceQu + GarageQual + CentralAir + 
                MSSubClass_190 + MSSubClass_120 + MSSubClass_30 + MSSubClass_160 + 
                MSZoning_RM + LotConfig_CulDSac + Neighborhood_Veenker + 
                Neighborhood_Crawfor + Neighborhood_NoRidge + Neighborhood_Somerst + 
                Neighborhood_BrkSide + Neighborhood_Sawyer + Neighborhood_NridgHt + 
                Neighborhood_NAmes + Neighborhood_MeadowV + Neighborhood_Edwards + 
                Neighborhood_Timber + Neighborhood_StoneBr + Neighborhood_ClearCr + 
                Neighborhood_BrDale + Condition1_Feedr + Condition1_Artery + 
                Condition1_RRAe + HouseStyle_1Story + HouseStyle_1.5Fin + 
                HouseStyle_1.5Unf + HouseStyle_2.5Fin + Exterior1st_Wd_Sdng + 
                Exterior1st_HdBoard + Exterior1st_CemntBd + Exterior2nd_Wd_Sdng + 
                Exterior2nd_CmentBd + Foundation_CBlock + Foundation_BrkTil + 
                GarageType_Detchd + GarageType_CarPort + PavedDrive_N + PavedDrive_P + 
                SaleType_New + SaleCondition_Abnorml + SaleCondition_Partial,train,method="glmnet",
              tuneGrid=expand.grid(alpha=1,lambda=seq(0.001,0.1,by = 0.0005)), trControl=custom)
lasso
plot(varImp(lasso, scale = F))
lassoVarImp <- varImp(lasso,scale=F)
print(lassoVarImp)
lassoImportance <- lassoVarImp$importance
varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))
print(varsNotSelected)
print(varsSelected)
#From variable importance plot, drop the insignificant variables and build a model with others
fit3<- lm(ln_SalePrice ~  OverallQual +  MSZoning_C_all + 
            BsmtFullBath + HalfBath + 
            LotShape + BsmtCond + BsmtExposure + 
            FireplaceQu + GarageQual + CentralAir + 
            MSSubClass_30 + MSSubClass_160 + 
            MSZoning_RM + LotConfig_CulDSac + Neighborhood_Veenker + 
            Neighborhood_Crawfor + Neighborhood_NoRidge + Neighborhood_Somerst + 
            Neighborhood_BrkSide + Neighborhood_Sawyer + Neighborhood_NridgHt + 
            Neighborhood_MeadowV + 
            Neighborhood_Timber + Neighborhood_StoneBr + Neighborhood_ClearCr + 
            Condition1_Feedr + Condition1_Artery + 
            Condition1_RRAe + HouseStyle_1Story + 
            HouseStyle_1.5Unf + HouseStyle_2.5Fin + Exterior1st_Wd_Sdng + 
            Exterior1st_HdBoard +Exterior2nd_Wd_Sdng + 
            Exterior2nd_CmentBd + Foundation_CBlock + Foundation_BrkTil + 
            GarageType_CarPort + PavedDrive_P + 
            SaleType_New + SaleCondition_Abnorml, data=train)
summary(fit3)
#Next, from summary of linear rehression, we can drop some more variables which 
#are not significant. If in summary, *** is present, then that variable is important,
#if * is not present then that variable is of no significance.
#SaleCondition_Partial, SaleType_New, PavedDrive_P, GarageType_CarPort, Exterior2nd_CmentBd, 
#Exterior1st_CemntBd, HouseStyle_1Story, Condition1_RRAe, Condition1_Feedr, Neighborhood_Sawyer,
#Neighborhood_BrkSide, Neighborhood_Veenker, LotConfig_CulDSac, LotShape
fit4<- lm(ln_SalePrice ~  OverallQual + MSZoning_C_all +
            BsmtFullBath + HalfBath + 
            BsmtCond + BsmtExposure + 
            FireplaceQu + GarageQual + CentralAir + 
            MSSubClass_30 + MSSubClass_160 + 
            MSZoning_RM + 
            Neighborhood_Crawfor + Neighborhood_NoRidge + Neighborhood_Somerst + 
            Neighborhood_NridgHt +
            Neighborhood_MeadowV + 
            Neighborhood_Timber + Neighborhood_StoneBr + Neighborhood_ClearCr + 
            Condition1_Artery + 
            HouseStyle_1.5Unf + HouseStyle_2.5Fin + Exterior1st_Wd_Sdng + 
            Exterior1st_HdBoard +Exterior2nd_Wd_Sdng + 
            Foundation_CBlock + Foundation_BrkTil + 
            SaleCondition_Abnorml, data=train)
summary(fit4)

#predict the values for training dataset
train$ln_predicted_SalePrice<- predict(fit4,newdata = train)
train$predicted_SalePrice<- exp(predict(fit4,newdata = train))

#Decile Analysis of training data
decLocations<- quantile(train$predicted_SalePrice,probs=seq(0.1,0.9,by=0.1))
train$decile<- findInterval(train$predicted_SalePrice,c(-Inf,decLocations,Inf))
summary(train$decile)
library(sqldf)
train_decile_analysis<- sqldf("select decile, count(decile) as count, avg(predicted_SalePrice) as avg_predicted_SalePrice, avg(SalePrice) as avg_SalePrice from train group by decile order by decile desc")
write.csv(train_decile_analysis,file="D:/data science/House Prices Advanced Regression Techniques/decile_analysis_training_data.csv")

#After building the model on training data, now predict the data for test dataset
test$ln_predicted_SalePrice<- predict(fit4,newdata=test)
test$predicted_SalePrice<- exp(predict(fit4,newdata=test))

#Decile Analysis of Testing dataset
decLocations<- quantile(test$predicted_SalePrice,probs=seq(0.1,0.9,by=0.1))
test$decile<- findInterval(test$predicted_SalePrice,c(-Inf,decLocations,Inf))
summary(test$decile)
test_decile_analysis<- sqldf("select decile, count(decile) as count, avg(predicted_SalePrice) as avg_predicted_SalePrice, avg(SalePrice) as avg_SalePrice from test group by decile order by decile desc")
write.csv(test_decile_analysis,file="D:/data science/House Prices Advanced Regression Techniques/decile_analysis_linear_regression_test_data.csv")

#Checking for RMSE, MAPE, MSE for training dataset
actuals_pred<- data.frame(cbind(actuals=train$SalePrice,predicted=train$predicted_SalePrice))
correlation_accuracy<- cor(actuals_pred)
print(correlation_accuracy)
head(actuals_pred)
write.csv(actuals_pred,file="D:/data science/House Prices Advanced Regression Techniques/actuals_pred.csv")

mape<- mean(abs((actuals_pred$predicted-actuals_pred$actuals))/actuals_pred$actuals)
print(mape)
mean_absolute_error<- mean(abs(actuals_pred$predicted-actuals_pred$actuals)) 
print(mean_absolute_error)
#mean_square_error<- mean((actuals_pred$predicted-actuals_pred$actuals)^2)
#print(mean_square_error)
#RMSE<- sqrt(mean_square_error)
#print(RMSE)

#To find RMSE
RSS <- c(crossprod(fit4$residuals))
MSE <- RSS / length(fit4$residuals)
RMSE <- sqrt(MSE)

#Checking for RMSE, MAPE, MSE, MAE for testing dataset
actuals_pred<- data.frame(cbind(actuals=test$SalePrice,predicted=test$predicted_SalePrice))
correlation_accuracy<- cor(actuals_pred)
print(correlation_accuracy)
head(actuals_pred)
write.csv(actuals_pred,file="D:/data science/House Prices Advanced Regression Techniques/actuals_pred_test.csv")

mape<- mean(abs((actuals_pred$predicted-actuals_pred$actuals))/actuals_pred$actuals)
print(mape)
mean_absolute_error<- mean(abs(actuals_pred$predicted-actuals_pred$actuals)) 
print(mean_absolute_error)
#mean_square_error<- mean((actuals_pred$predicted-actuals_pred$actuals)^2)
#print(mean_square_error)
#RMSE<- sqrt(mean_square_error)
#print(RMSE)

#Data Visualization of residual plots or diagnostic plots
par(mfrow=c(2,2))
plot(fit4)

# heteroscadacity using NCV test
library(car)
ncvTest(fit4)
#-----------------#-------------------#---------------------------#
#XGBOOST MODEL
install.packages("xgboost")
library(xgboost)
library(Matrix)
trainm<- sparse.model.matrix(ln_SalePrice~OverallQual + MSZoning_C_all +
                               BsmtFullBath + HalfBath + 
                               BsmtCond + BsmtExposure + 
                               FireplaceQu + GarageQual + CentralAir + 
                               MSSubClass_30 + MSSubClass_160 + 
                               MSZoning_RM + 
                               Neighborhood_Crawfor + Neighborhood_NoRidge + Neighborhood_Somerst + 
                               Neighborhood_NridgHt +
                               Neighborhood_MeadowV + 
                               Neighborhood_Timber + Neighborhood_StoneBr + Neighborhood_ClearCr + 
                               Condition1_Artery + 
                               HouseStyle_1.5Unf + HouseStyle_2.5Fin + Exterior1st_Wd_Sdng + 
                               Exterior1st_HdBoard +Exterior2nd_Wd_Sdng + 
                               Foundation_CBlock + Foundation_BrkTil + 
                               SaleCondition_Abnorml, data=train)
train_label<- train[,"ln_SalePrice"]
train_matrix<- xgb.DMatrix(data=as.matrix(trainm),label= train_label)

testm<- sparse.model.matrix(ln_SalePrice~OverallQual + MSZoning_C_all +
                               BsmtFullBath + HalfBath + 
                               BsmtCond + BsmtExposure + 
                               FireplaceQu + GarageQual + CentralAir + 
                               MSSubClass_30 + MSSubClass_160 + 
                               MSZoning_RM + 
                               Neighborhood_Crawfor + Neighborhood_NoRidge + Neighborhood_Somerst + 
                               Neighborhood_NridgHt +
                               Neighborhood_MeadowV + 
                               Neighborhood_Timber + Neighborhood_StoneBr + Neighborhood_ClearCr + 
                               Condition1_Artery + 
                               HouseStyle_1.5Unf + HouseStyle_2.5Fin + Exterior1st_Wd_Sdng + 
                               Exterior1st_HdBoard +Exterior2nd_Wd_Sdng + 
                               Foundation_CBlock + Foundation_BrkTil + 
                               SaleCondition_Abnorml, data=test)
test_label<- test[,"ln_SalePrice"]
test_matrix<- xgb.DMatrix(data=as.matrix(testm),label= test_label)
xgb_params<- list(objective = "reg:linear",booster = "gbtree",eta=0.3,gamma=0,max_depth=6,
                  min_child_weight=1,subsample=1,colsample_bytree=1, eval_metric="rmse")
xgbcv <- xgb.cv( params = xgb_params, data = train_matrix, nrounds = 500, nfold = 5, showsd = T, stratified = T, print.every.n = 40, early.stop.round = 10, maximize = F)
xgb_mod <- xgb.train(data = train_matrix, params=xgb_params, nrounds = 27)
XGBpred <- predict(xgb_mod, test_matrix)
predictions_XGB <- exp(XGBpred)
head(predictions_XGB)
head(test)
