#dataNB is the first base dataset 

cutdata <- function(data,alpha=0.1){
  data$Deliquencies <- cut(data$Deliquencies,
                           breaks=c(0,1,2,3,4,max(data$Deliquencies)),
                           include.lowest = T, right = F,ordered_result=T)
  data$NbDependents <- cut(data$NbDependents,
                           breaks=c(0,1,2,3,4,max(data$NbDependents)),
                           include.lowest = T, right = F,ordered_result=T)
  data$NbRELL <- cut(data$NbRELL,
                     breaks=c(0,1,2,3,4,max(data$NbRELL)),
                     include.lowest = T, right = F,ordered_result=T)
  data$Nbopenaccount <- cut(data$Nbopenaccount,
                            breaks=c(0,1,5,10,15,max(data$Nbopenaccount)),
                            include.lowest = T, right = F,ordered_result=T)
  data$RU <- cut(data$RU,breaks=c(0,0.0032877022, 0.0113449733, 0.0211195538, 0.032852284, 
                                  0.0480705532, 0.0673875167, 0.0936115512, 0.1281959273, 0.171413096, 
                                  0.2248684921, 0.2914270474, 0.369598673, 0.4629485376, 0.5738497795, 
                                  0.7081339066, 0.8556174928, 0.9800623168, 1, 8.851851852),
                 include.lowest = T, right = F,ordered_result=T)
  

  numericcut <- names(data)[which(sapply(data,function(x) is.numeric(x)))]
  
  numericcut <- setdiff(numericcut,c("Id","SeriousDlqin2yrs"))
  for (e in numericcut){
    data[[e]]<-cut(data[[e]],unique(quantile(data[[e]],probs=seq(0,1,alpha))),
                   include.lowest = T, right = F,ordered_result=T)
  }
  return(data) 
}
dataNBFC <- cutdata(dataNB)

str(dataNBFC)
summary(dataNBFC,maxsum = 20)

#test summarydplyr 
summarysdplyr(dataNBFC,SeriousDlqin2yrs,groupvars=list("RU"))
summarysdplyr(dataNBFC,SeriousDlqin2yrs,groupvars=list("Deliquencies"))
summarysdplyr(dataNBFC,SeriousDlqin2yrs,groupvars=list("DebtRatio"))
summarysdplyr(dataNBFC,SeriousDlqin2yrs,groupvars=list("Age"))


#Own Naive Bayes bad code for speed ()

#Few tests 
train <- dataNBFC
jointProb<-sapply(train,function(x) prop.table(table(train$SeriousDlqin2yrs,x),1))
                  
NaiveBayesTCV<-function(data,predictor,toremoveV,CV){
  d<-mean(data[[predictor]])
  toremove <- c(predictor,toremoveV)
  sample1 <- sample(1:nrow(data),as.integer(nrow(data)/CV))
  Vtopredict <- data[-sample1,predictor]
  train<-data[-sample1,!names(data) %in% toremove]
  test <- data[sample1,!names(data) %in% toremove]
  long<-nrow(test)
  larg<-ncol(train)
  PD_NB <- 1:long
  PG_NB <- 1:long
  Prediction <- 1:long
  Score <- 1:long
  jointProba<-sapply(train,
                     function(x) prop.table(table(Vtopredict,x),1))
  for (i in 1:long){
    
    defaultprob<-d
    goodprob<-1-defaultprob
    x<-test[i,]
    
    for (j in 1:larg){
      
      defaultprob <- defaultprob*jointProba[[j]][2,which(colnames(jointProba[[j]])==x[[j]])]
      goodprob <- goodprob*jointProba[[j]][1,which(colnames(jointProba[[j]])==x[[j]])]
    }
    PD_NB[i] <- as.numeric(defaultprob)
    PG_NB[i] <- as.numeric(goodprob)
    Prediction[i] <- ifelse(defaultprob>=goodprob,1,0)
    Score[i]<-as.numeric(ifelse((PG_NB[i]/PD_NB[i])!=Inf,(PG_NB[i]/PD_NB[i]),NA))
  }
  Scorecut <- cut(Score,breaks=quantile(Score,seq(0,1,0.0125)),
                  include.lowest = T, right = F,ordered_result=T)
  result<-data.frame(data[sample1,],PD_NB,PG_NB,Prediction,Scorecut)
  result<-arrange(result,desc(Score))
  return(result)
}
result1 <- NaiveBayesTCV(dataNBFC,"SeriousDlqin2yrs",toremove="ID",10)
CVM <- function(data,selgrade,toremove,NMC,percentage){
  result1 <- NaiveBayesTCV(data,selgrade,toremove,percentage)
  i<-0
  repeat {
    temp<-NaiveBayesTCV(dataNB,selgrade,toremove,percentage)
    result1 <- rbind(temp,result1)
    i <- i+1
    if (i>NMC)
      break
  }
  result1<-arrange(result1,desc(Score))
  return(result1)
}




