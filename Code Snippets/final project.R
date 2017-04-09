library(leaps)
regfit.bwd=regsubsets(y~.,data = trainy_processed, method = "backward", really.big = TRUE, nbest = 1, nvmax =25)
summary(regfit.bwd)
coef(regfit.bwd,25)

regfit.fwd=regsubsets(y~.,data = trainy_processed, method = "forward", really.big = TRUE, nvmax=30)
summary(regfit.fwd)
coef(regfit.fwd,30)

library(pls)
pcr.fit5=pcr(y~.,data=trainy_processed, validation="CV", ncomp=5)
summary(pcr.fit5)
pcr.fit10=pcr(y~.,data=trainy_processed, validation="CV", ncomp=10)
summary(pcr.fit10)
pcr.fit15=pcr(y~.,data=trainy_processed, validation="CV", ncomp=15)
summary(pcr.fit15)
pcr.fit50=pcr(y~.,data=trainy_processed, validation="CV", ncomp=50)
validationplot(pcr.fit50,val.type = "MSEP")


#still working out the problems in this#
pls.fit=plsr(y~.,data=trainy_processed, validation="CV")
validationplot(pls.fit,val.type = "MSEP")
summary(pls.fit)



