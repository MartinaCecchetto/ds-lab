VAR_NUMERIC= c("RiskPropension","PortfolioRisk","ClientInvestmentHorizon", "ClientKnowledgeExperience", "IncomeNeed", "LongTermCareNeed", "ProtectionNeed", "PensionNeed", "BondInvestments","EquityInvestments", "MoneyMarketInvestments", "OtherInvestments", "Cash")
cor(clienti[ ,VAR_NUMERIC])
#creo test e training set per classificazone
test=clienti[clienti$Controllo == 0, ]
training= clienti[clienti$Controllo != 0, ]
#modelli most probable
model_bond <- glm(BondInvestments ~ IncomeNeed +LongTermCareNeed - 1 + ProtectionNeed + PensionNeed,family=binomial(link='probit'),data=training)
model_equity<- glm(EquityInvestments ~ IncomeNeed +LongTermCareNeed - 1 + ProtectionNeed + PensionNeed,family=binomial(link='probit'),data=training)
model_money<- glm(MoneyMarketInvestments ~ IncomeNeed +LongTermCareNeed - 1 + ProtectionNeed + PensionNeed,family=binomial(link='probit'),data=training)
model_others <- glm(OtherInvestments ~ IncomeNeed +LongTermCareNeed - 1 + ProtectionNeed + PensionNeed,family=binomial(link='probit'),data=training)
model_cash <- glm(Cash ~ IncomeNeed +LongTermCareNeed - 1 + ProtectionNeed + PensionNeed,family=binomial(link='probit'),data=clienti)

summary(model_bound)
summary(model_equity)
summary(model_money)
summary(model_others)
summary(model_cash)
#predizione
test$BondInvestments=predict.glm(model_bond,test, type = "response")
test$EquityInvestments=predict.glm(model_equity,test, type = "response")
test$MoneyMarketInvestments=predict.glm(model_money,test, type = "response")
test$OtherInvestments=predict.glm(model_others,test, type = "response")
test$Cash=predict.glm(model_cash,test, type = "response")
#percentuale di investimento
test$sum=apply(test[ , 21:25],1, sum)
test[ , 21:25]=test[ , 21:25]/test$sum


