data <- dataNBFC
predictor <- "SeriousDlqin2yrs"
toremoveV <- "Id"
CV <- 10

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


(resultat <- summarysdplyr(result,SeriousDlqin2yrs,groupvars=list("Scorecut")))
