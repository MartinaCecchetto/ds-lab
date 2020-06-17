clienti <- read.csv("DatasetClientClustering.csv", encoding="UTF-8", comment.char="#", na.strings="")
clienti <- clienti[,-(1:8)]
clienti[clienti$PanicMood==-1,"PanicMood"]<-1
clienti$controllo <- rowSums(clienti[,(ncol(clienti)-4):ncol(clienti)])
clienti[clienti$PanicMood==-1,"PanicMood"]<-1

#Sistemo cose strane 
nrow(clienti[clienti$controllo<0.95,]) #Sono 90, ho scelto 0,95 a caso ragaaaa a casoooooooooooooooooo


#Creo RiskPropensionClass
hist(clienti$RiskPropension)
clienti$RiskPropensionClass <- ifelse((clienti$RiskPropension<=0.2), 1, 
                                      ifelse((clienti$RiskPropension>0.2 & clienti$RiskPropension<=0.4), 2, 
                                             ifelse((clienti$RiskPropension>0.4 & clienti$RiskPropension<=0.6),3,
                                                    ifelse((clienti$RiskPropension>0.6 & clienti$RiskPropension<=0.8),4,5))))
hist(clienti[clienti$controllo<0.95,"RiskPropension"])


conditional_mean <- data.frame(matrix(ncol=6,nrow=0, dimnames=list(NULL,c("RiskClass", "BondInvestments", "EquityInvestments", "MoneyMarketInvestments", "OtherInvestments", "Cash"))))
for (risk in 1:5) {
  clienti_risk <- clienti[(clienti$RiskPropensionClass==risk & clienti$controllo>=0.95),]
  conditional_mean[risk,1]<-risk
  conditional_mean[risk,2]<-mean(clienti_risk[,"BondInvestments"])
  conditional_mean[risk,3]<-mean(clienti_risk[,"EquityInvestments"])
  conditional_mean[risk,4]<-mean(clienti_risk[,"MoneyMarketInvestments"])
  conditional_mean[risk,5]<-mean(clienti_risk[,"OtherInvestments"])
  conditional_mean[risk,6]<-mean(clienti_risk[,"Cash"])
}
conditional_mean$controllo <- rowSums(conditional_mean[2:6])

for (risk in 1:5) {
  clienti[(clienti$RiskPropensionClass==risk & clienti$controllo<0.95),"BondInvestments"]<-conditional_mean[risk,"BondInvestments"]
  clienti[(clienti$RiskPropensionClass==risk & clienti$controllo<0.95),"EquityInvestments"]<-conditional_mean[risk,"EquityInvestments"]
  clienti[(clienti$RiskPropensionClass==risk & clienti$controllo<0.95),"MoneyMarketInvestments"]<-conditional_mean[risk,"MoneyMarketInvestments"]
  clienti[(clienti$RiskPropensionClass==risk & clienti$controllo<0.95),"OtherInvestments"]<-conditional_mean[risk,"OtherInvestments"]
  clienti[(clienti$RiskPropensionClass==risk & clienti$controllo<0.95),"Cash"]<-conditional_mean[risk,"Cash"]
} 

clienti$controllo_finale <- rowSums(clienti[,21:25])
summary(clienti$controllo_finale)

#
