#run Mixed effect Cox model, model selection:

library(survival)
library(coxme)

# --- fit a mixed effect Cox model with FSA code as the cluster
m1 <- coxme(Surv(futime, outcome) ~ age_entry + sexe + materielle_median + 
              sociale_median+CKD+mi+diabete_combined+hypertension_combined+pvd_combined+cad_combined+
              copd_combined+hyperlipidemia_combined+(1 | pc), data = cohort)

summary(m1)

# ----calculate confidence interval of coefficients:
#first get SE:
se<-sqrt(diag(vcov(m1)))
coeff<-m1$coefficients

# 95% CI is calculated as (exp(coeff-1.96*SE),exp(coeff+1.96*SE))
CI<-data.frame(low.95=exp(coeff-1.96*se),high.95=exp(coeff+1.96*se))
CI

# --- fit a regular Cox model
m2<-coxph(Surv(futime, outcome) ~ age_entry + sexe + materielle_median + 
            sociale_median+CKD+mi+diabete_combined+hypertension_combined+pvd_combined+cad_combined+
            copd_combined+hyperlipidemia_combined, data = cohort)

# --- compare two models:
anova(m1,m2)

#How to test the model hypothesis?

#Using age as categorical variable?


 