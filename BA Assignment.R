getwd()
setwd("/Users/Faderemi/Documents")
library(readxl)

install.packages("psych")
library(psych)
install.packages("dplyr")
library(dplyr)

data<-read_excel("ames.xlsx")

#descriptive data
summary(data)

install.packages("psych")
library(psych)
describe(data)
pacman(tidyverse, caret, ggplot2)
tab1<-describe(data)
write.csv(tab1, "describe.csv")
install.packages("pacman")
pacman::p_load(tidyverse,caret,dplyr,ggplot2,psych,rms)

head(data)
tail(data)
dim(data)
describeBy(data$Sale.Price, data$Overall.Qual)
describeBy(data$Sale.Price, data$MS.Zoning)
aggregate(Sale.Price~Overall.Qual, data=data, FUN = "mean")
aggregate(Sale.Price~MS.Zoning, data=data, FUN = "mean")
aggregate(Sale.Price~Bldg.Type, data=data, FUN = "mean")
aggregate(Sale.Price~Foundation, data=data, FUN = "mean")
aggregate(Sale.Price~Bsmt.Qual, data=data, FUN = "mean")
#histogram of Sale.Price
hist(data$Sale.Price, main="histogram of sale price", breaks = 50)
hist(data$Sale.Price[data$Sale.Price<800000], main="histogram of sale price", xlab = "sale price")
boxplot(data$Sale.Price, main="Boxplot of sale price", xlab = "sale price")
qplot(data$Sale.Price[data$Sale.Price<800000],main="qplot of sale price", xlab = "sale price", ylab = "frequency")

#format and identify data quality issues

data$Exter.Cond[data$Exter.Cond == "Good"] <- "Gd"
data$Exter.Qual[data$Exter.Qual == "Good"] <- "Gd"

#Outliers 
data$Overall.Qual[data$Overall.Qual > 10] <- NA
data$Overall.Cond[data$Overall.Cond > 9] <- NA
data$Year.Built[data$Year.Built > 2010] <- NA
data$Garage.Yr.Blt[data$Garage.Yr.Blt> 2010] <- NA
data$Lot.Area[data$Lot.Area> 200000] <- NA
#removing columns with NA
data<-select(data, -Garage)
data<-select(data, -Alley)
#formatting 
data$Fireplace.Qu <- factor(replace_na(data$Fireplace.Qu,'0'), 
                             levels = c('0','Po','Fa','TA','Gd','Ex'))

data$Exter.Qual <- factor(replace_na(data$Exter.Qual,'0'), 
                            levels = c('0','Po','Fa','TA','Gd','Ex'))

data$Exter.Cond <- factor(replace_na(data$Exter.Cond,'0'), 
                            levels = c('0','Po','Fa','TA','Gd','Ex'))

data$Bsmt.Qual <- factor(replace_na(data$Bsmt.Qual,'0'), 
                            levels = c('0','Po','Fa','TA','Gd','Ex'))

data$Bsmt.Cond <- factor(replace_na(data$Bsmt.Cond,'0'), 
                            levels = c('0','Po','Fa','TA','Gd','Ex'))

data$Heating.QC <- factor(replace_na(data$Heating.QC,'0'), 
                            levels = c('0','Po','Fa','TA','Gd','Ex'))

data$Garage.Qual <- factor(replace_na(data$Garage.Qual,'0'), 
                            levels = c('0','Po','Fa','TA','Gd','Ex'))

data$Garage.Cond <- factor(replace_na(data$Garage.Cond,'0'), 
                            levels = c('0','Po','Fa','TA','Gd','Ex'))

data$Kitchen.Qual <- factor(replace_na(data$Kitchen.Qual,'0'), 
                            levels = c('0','Po','Fa','TA','Gd','Ex'))

data$Lot.Shape <- factor(replace_na(data$Lot.Shape,'0'), 
                            levels = c('0','IR3','IR2','IR1','Reg'))

data$Utilities <- factor(replace_na(data$Utilities,'0'), 
                            levels = c('0','ELO','NoSeWa','NoSewr','AllPub'))

data$Land.Slope <- factor(replace_na(data$Land.Slope,'0'), 
                            levels = c('0','Sev','Mod','Gtl'))

data$Bsmt.Exposure <- factor(replace_na(data$Bsmt.Exposure,'0'), 
                            levels = c('0','No','Mn','Av','Gd'))

data$BsmtFin.Type.1 <- factor(replace_na(data$BsmtFin.Type.1,'0'), 
                            levels = c('0','Unf','LwQ','Rec','BLQ','ALQ','GLQ'))

data$BsmtFin.Type.2 <- factor(replace_na(data$BsmtFin.Type.2,'0'), 
                              levels = c('0','Unf','LwQ','Rec','BLQ','ALQ','GLQ'))

data$Electrical <- factor(replace_na(data$Electrical,'0'), 
                              levels = c('0','Mix','FuseP','FuseF','FuseA','SBrkr'))

data$Garage.Finish <- factor(replace_na(data$Garage.Finish,'0'), 
                              levels = c('0','Unf','RFn','Fin'))

data$`Functio l` <- factor(replace_na(data$`Functio l`,'0'), 
                              levels = c('0','Sal','Sev','Maj2','Maj1','Mod','Min2','Min1','Typ'))

data$Fence <- factor(replace_na(data$Fence,'0'), 
                            levels = c('0','MnWw','GdWo','MnPrv','GdPrv'))



data[sapply(data,is.character)]<-lapply(data[sapply(data,is.character)],as.factor)
str(data)
sapply(data, class)
#visualisation after addressing data quality

ggplot(data=data)+
  geom_bar(mapping = aes(x=as.factor(MS.Zoning), y=Sale.Price), stat="summary", fun.y="mean")+
  labs(title = "average house price by MS.Zoning", x="MS.Zoning", y="mean of Sale.price($)")

qplot(Overall.Qual,Sale.Price, data=data, geom = c ("boxplot"), fill=Overall.Qual)

ggplot(data,aes(as.factor(Overall.Qual), Sale.Price))+
  geom_boxplot()

data<-data %>%
  filter(Overall.Qual<=10)



