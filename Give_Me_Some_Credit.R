library(dplyr)
library(ggplot2)
data <- read.csv('cs-training.csv',header=T)

attach(data)
str(data)
head(data)
tail(data)
summary(data)
names(data)[1] <- "Id"
typesummary <- as.data.frame(sapply(data,function(x) class(x)))
numvar <- names(data)[which(sapply(data,is.numeric))]
numfactor <- names(data)[which(sapply(data,is.factor))]

vars <- names(data)


#Detection of errors 

#identify id variable 
(ids <- which(sapply(data, function(x) length(unique(x))) == nrow(data)))


#Duplicated rows 
errors <- data[Id %in% Id[duplicated(Id)],]
errors <- arrange(errors,desc(ListingNumber))

data <- data[!duplicated(Id),]


#identify all missing variables 
ignore <- vector()
mvc <- sapply(data[vars], function(x) sum(is.na(x)))
(mvn <- names(which(mvc == nrow(data))))
ignore <- union(ignore, mvn)

#identify all missing rows 
mvr <- which(sapply(data,function(x) all(is.na(x))))

#identify many missing
mvn70 <- names(which(mvc > 0.7*nrow(data)))


# identify constant variables
(constants <- names(which(sapply(data[vars], function(x) all(x == x[1L])))))
ignore <- union(ignore, constants)

#datac for data cleaned 
length(vars)
varsc <- setdiff(vars, ignore)
length(vars)
datac <- data[vars]

##Dealing with aberations 

#young and old person no revenue DebtRatio > 1
(quantile(data$DebtRatio,seq(0,1,0.01)))
datayold <- datac[datac$DebtRatio>1,]

datac <- setdiff(datac,datayold)

#dealing with na value replacing by median 

##identifying na value 
mvcc <- sapply(datac, function(x) sum(is.na(x)))
replace_na_median <- function(column){
  column<-as.numeric(as.character(column)) #converting into numeric 
  column[is.na(column)] =median(column, na.rm=TRUE) 
  return(column) 
}
datac2 <- as.data.frame(sapply(datac,replace_na_median))

#verif
(naverif <- sapply(datac2, function(x) sum(is.na(x))))


#Recoding names of varaibles for better user experience 

names(datac2) <- c("Id", "SeriousDlqin2yrs", "RU", 
                   "Age", "NbT30_59Worse", "DebtRatio", "MonthlyIncome", 
                   "Nbopenaccount", "NbT90DaysLate", 
                   "NbRELL", "Nbt60.89Worse", 
                   "NbDependents")

##manual cleaning of absurd outliers 
#RU strange 
quantile(datac2$RU,seq(0.98,1,0.0002))
removeRU_absurd <- datac2[datac2$RU>10,]
removelate_absurd <- datac2[datac2$NbT30_59Worse>10|datac2$NbT90DaysLate>10|datac2$Nbt60.89Worse>10,]
#Strangely removelate_absurd are not outliers !!!

#correction(RU)
datac2$RU[datac2$RU>10] <- median(datac2$RU)

to_remove_RU_risky <- (datac2[datac2$RU>1 & datac2$RU<=10,])

datac4 <- datac2

#check for removeRU_absurd and removelate_risky 
summary(removelate_absurd) #very risky 
summary(removeRU_absurd) # not too risky 
summary(to_remove_RU_risky) #very risky 
summary(datac4)
str(datac4)




# simple outliers 1D detection
outliers1Df <- function(data,strong=3){
  attach(data)
  result <- sapply(data,function(x) data$Id[which(x %in% boxplot.stats
                                          (x, coef = strong)$out )])
  result2 <- sapply(result,function(x) length(x))
  result3 <-unique(unlist(result))
  result4 <- length(result3)
  names(result) <- names(data)
  names(result2) <- names(data)
  l <- list(result,result2,result3,result4)
  names(l) <- c("variable_outliers","Nb_outliers_for_each_variable"
                ,'all_1D_outliers',"Nball_1D_outliers")
  return(l)
}
outliers1d <- outliers1Df(datac2,3)
outliers1d[[2]]
outlierstoremoveid <- unique(c(outliers1d[[1]]$MonthlyIncome,
                              outliers1d[[1]]$Nbopenaccount,
                              outliers1d[[1]]$NbDependents,
                              outliers1d[[1]]$NbRELL
                              ))
View(datac2[datac2$Id %in% outliers1d[[1]]$MonthlyIncome,])
dataoutliers <- datac2[datac2$Id %in% outlierstoremoveid,]


#data visualisation 
library(corrplot)

#corrplot 
mcor <- cor(datac4)
# Print mcor and round to 2 digits
round(mcor, digits=2)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black", order="FPC")


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

res1 <- cor.mtest(datac4, 0.95)
res2 <- cor.mtest(datac4, 0.99)
## specialized the insignificant value according to the significant level
corrplot(mcor, p.mat = res2[[1]], sig.level = 0.2)

#add all pv-alue 

corrplot(mcor, p.mat = res1[[1]], insig = "p-value", sig.level = -1)


#data modif fusion of "NbT30_59Worse","NbT90DaysLate","Nbt60.89Worse"
dataNB <- datac4
dataNB$Deliquencies <- datac4$NbT30_59Worse +datac4$NbT90DaysLate
+datac4$Nbt60.89Worse

dataNB <- dataNB[!names(dataNB) %in% 
                   c("NbT30_59Worse","NbT90DaysLate","Nbt60.89Worse")]
str(dataNB)

mcor2 <- cor(dataNB)
# Print mcor and round to 2 digits
mcor2 <- round(mcor2, digits=2)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor2, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black",  order="FPC")

#Transform numeric variable into categorical variables forggpairs
dataNBc <- dataNB

#manual modif 
 dataNBc$SeriousDlqin2yrs <- as.factor(
   as.character(dataNBc$SeriousDlqin2yrs))
 dataNBc$Deliquencies <- cut(dataNBc$Deliquencies,
                             breaks=c(0,1,2,3,4,max(dataNBc$Deliquencies)),
                             include.lowest = T, right = F,ordered_result=T)
dataNBc$NbDependents <- cut(dataNBc$NbDependents,
                            breaks=c(0,1,2,3,4,max(dataNBc$NbDependents)),
                            include.lowest = T, right = F,ordered_result=T)
dataNBc$NbDependents <- cut(dataNBc$NbDependents,
                            breaks=c(0,1,2,3,4,max(dataNBc$NbDependents)),
                            include.lowest = T, right = F,ordered_result=T)
dataNBc$NbRELL <- cut(dataNBc$NbRELL,
                            breaks=c(0,1,2,3,4,max(dataNBc$NbRELL)),
                            include.lowest = T, right = F,ordered_result=T)
dataNBc$Nbopenaccount <- cut(dataNBc$Nbopenaccount,
                            breaks=c(0,1,5,10,15,max(dataNBc$Nbopenaccount)),
                            include.lowest = T, right = F,ordered_result=T)
summary(dataNBc)


# numericcut1 <- which(sapply(dataNB,function(x) is.numeric(x)))
#   for (i in numericcut[-1]){
#     dataNBc[,i]<-cut(dataNB[,i],unique(quantile(dataNB[,i],probs=seq(0,1,0.05))),
#                      include.lowest = T, right = F,ordered_result=T)
#   } 
str(dataNBc)
summary(dataNBc,maxsum = 20)
#ggpairs
#beautiful data visualisation 
library(GGally)

str(dataNBc)
#we balance the dataset with equal default to statistic and
#better data vizualisation 

#remove outliers for better data viz 
dataNBc <- dataNBc[!dataNBc$Id %in% outlierstoremoveid, ]
dataNBcD <- dataNBc[dataNBc$SeriousDlqin2yrs==1,]
dataNBcG <- dataNBc[dataNBc$SeriousDlqin2yrs==0,]

sampleDefault <- dataNBcD[sample(1:nrow(dataNBcD),5000),-1]
sampleGood <- dataNBcG[sample(1:nrow(dataNBcG),5000),-1]

dataggpairsP <- union(sampleDefault,sampleGood)
summary(dataggpairsP)
ggpair1 <- ggpairs(data=dataggpairsP,
        lower = list(continuous = "density",
                     params = c(method = "loess", fill = "blue"),
                     combo = "box"),   	
        diag = list(continuous = "density", discrete = "bar"),
        color = "SeriousDlqin2yrs",
        filled = TRUE, 
        axisLabels="show")
ggpair1

ggpair2<- ggpairs(data=dataggpairsP[,5:10],
                  lower = list(continuous = "density", combo = "box"),     
                  diag = list(continuous = "density", discrete = "bar"),
                  color = "SeriousDlqin2yrs",
                  filled = TRUE, 
                  axisLabels="show")
#Summary dplyr RU and Deliquencies very important 

summarysdplyr <- function(data=NULL, measurevar, groupvars=vector(), na.rm=TRUE,
                          conf.interval=.95) {
  library(dplyr)
  attach(data)
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- data %.% regroup(groupvars) %.% summarise(
    N=n(),
    Min=min(measurevar,na.rm=na.rm),
    Mean=mean(measurevar,na.rm=na.rm),
    Max=max(measurevar,na.rm=na.rm),
    Sd=sd(measurevar,na.rm=na.rm),
    Se=Sd/sqrt(N),
    Ci=qt(conf.interval/2 + .5, (N-1)) * Se)
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  return(datac)
}


dataNBc$SeriousDlqin2yrs <- as.numeric(as.character(dataNBc$SeriousDlqin2yrs))

str(dataNBc)
summarysdplyr(dataNBc,SeriousDlqin2yrs,groupvars=list("Deliquencies"))

#test on datac2 on RU 
datatest <- datac2
datatest$RU <- cut(datatest$RU,quantile(datatest$RU,probs=seq(0,1,0.05)),
                   include.lowest = T, right = F,ordered_result=T)
summary(datatest$RU,maxsum = 20)
summarysdplyr(datatest,SeriousDlqin2yrs,groupvars=list("RU"))

summary(datac2[datac2$RU>1,])

#Cut for the NB algorithm 


#Intuition 
summary(datac4[datac4$NbT30_59Worse>1,])
summary(datac4[datac4$RU>1 & datac4$RU<2,])
summary(datac4[datac4$RU>2,])

