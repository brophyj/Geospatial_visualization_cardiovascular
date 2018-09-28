# poisson regression model.

#create data set for poisson regression:
ds<-age_df3%>%filter(V5 %in% c('[65,70)','[70,75)','[75,80)','[80,85)','[85,Inf)'))%>%
  left_join(final_tb[,c(1,2,4)])%>%
  left_join(interv_count,b=c('V5'='age_index.cat','fsa_clean'='pc'))%>%
  left_join(sociale,by=c('fsa_clean'='cp3'))%>%
  left_join(materielle,by=c('fsa_clean'='cp3'))%>%
  left_join(mi_count,by=c('fsa_clean'='pc'))

ds<-left_join(ds,comorb_summary_cat,by=c('fsa_clean'='pc'))
ds[,10:17]<-lapply(ds[,10:17],as.factor)


ds[,c('case','interv','mi')]<-lapply(ds[,c('case','interv','mi')],function(x){x<-ifelse(is.na(x),0,x)})
ds%<>%filter(case!=0)
ds%<>%filter(materielle_median!=0,sociale_median!=0)

ds[,c('materielle_median','sociale_median','mi_cat')]<-lapply(ds[,c('materielle_median','sociale_median','mi_cat')],as.factor)

fit <- glm(interv ~ V5 + materielle_median + sociale_median +mi_cat+CKD+COPD+diabete+dyslipidemia+PVD+hypertension+
             offset(log(case)), data = ds, family = poisson)
result<-summary(fit)

exp(coef(result))

exp(confint(fit))

deviance(fit)/fit$df.residual