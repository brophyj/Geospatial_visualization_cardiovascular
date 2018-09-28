#confounder acute_mi: ascertained from in-hospital admission diagnosis, sufficient enough because patients with mi always go to hospital:

acute_mi<-c('4109',unique(ICD$CIM.10.CA[ICD$CIM9.Quebec=='4109']))
acute_mi<-me_diag%>%filter(diag_diag%in% acute_mi)

#filter for first time diagnosis above 60:
acute_mi<-acute_mi%>%left_join(sejour[,c(1:2,6)],by=c('nam','no_seq'))%>%
  group_by(nam)%>%
  filter(dtsort==min(dtsort))%>%
  ungroup()%>%
  left_join(demo)%>%
  distinct(nam,dtsort,age,sexe,dt_index)
#filter to individuals who were first diagnosed acute MI when over 65
acute_mi<-acute_mi%>%mutate(age_index=as.numeric(age+(dtsort-dt_index)/365))%>%
  filter(age_index>=65)%>%
  mutate(age_index.cat=cut(age_index,breaks=c(seq(from = 60, to =85, by =5),Inf),right=F))%>%
  mutate(cyear=year(dtsort))%>%
  left_join(clsc[,c(1,2,4)],by=c('nam'='nam','cyear'='year'))%>%
  filter(!is.na(pc))

#calculate confounders in the poisson_model
#count mi cases by pc and divide count into quantiles:
mi_count<-acute_mi%>%filter(pc %in% fsacode)%>%
  group_by(pc)%>%
  summarise(mi=n_distinct(nam))

mi_count <- setDT(mi_count)[, mi_cat:= cut(mi, quantile(mi, probs=0:5/5), include.lowest=TRUE, labels=FALSE)]


# confounder 2: 
# ascertain from both in-hospital diagnosis and prescription data:
#again only take patients entering the study >=66
#assign the first date of diagnosis or prescription as the date to link to geographic location:


#from in-hospital diagnosis:
  
  #'From inpatient-diagnostic and intervention table find the ICD10 code for the following:
  dyslipidemia<-c('^272\\d{,2}')
  diabete<-c('^250\\d{,2}')
  chronic_kidney<-c('\\b585\\b')
  hypertension<-c('^401\\d{,2}')
  
  cad<-c('^410\\d{,2}','^411\\d{,2}','^412\\d{,2}','^413\\d{,2}','^414\\d{,2}')
  
  #code for peripheral:
  peripheral_10<-c('^I70\\d{,2}','^I71\\d{,2}')
  peripheral_10<-c(ICD$CIM.10.CA[grepl(paste(peripheral_10,collapse ='|'),ICD$CIM.10.CA)],'I731',
                   'I738','I739','I771','I790','K551','K558','K559','Z958','Z959')
  peripheral_9<-c(ICD$CIM9.Quebec[grepl('^441.*',ICD$CIM9.Stand)],'4439','V434','7854')
  peripheral<-c(peripheral_9,peripheral_10)
  
  #code for asthma and copd:
  COPD<-c('^491\\d{,2}','^490\\d{,2}','^492\\d{,2}','^493\\d{,2}','^494\\d{,2}','^495\\d{,2}','^496\\d{,2}',
          '^500\\d{,2}','^501\\d{,2}','^502\\d{,2}','^503\\d{,2}','^504\\d{,2}','^505\\d{,2}')
  #heart failure:
  hf<-c('^428\\d{,2}')
  #find icd10 code from ICD table:
  icd9_codes<-c(dyslipidemia,diabete,chronic_kidney,hypertension,cad,COPD,hf)
  icd10<-ICD$CIM.10.CA[grepl(paste(icd9_codes,collapse ='|'),ICD$CIM9.Stand)]
  icd9_qc<-ICD$CIM9.Quebec[grepl(paste(icd9_codes,collapse ='|'),ICD$CIM9.Stand)]
  icd_final<-unique(c(icd9_qc,icd10,peripheral))
  
  #find subjects with any of the diagnostic code from me_diag:, filter with nam in ramq_cc
  diag_subset<-me_diag%>%filter(diag_diag %in% icd_final)%>%
    filter(nam %in% demo$nam)%>%
    select(1,2,3,6)%>%
    left_join(sejour[,c(1,2,5,6,13)])
  
  
  #assign binary variables to each disease state:
  icd9_list<-list(dyslipidemia,diabete,chronic_kidney,hypertension,cad,COPD,hf)
  code<-list()
  for (i in seq_along(icd9_list)){
    
    code[[i]]<-unique(c(ICD$CIM.10.CA[grepl(paste(icd9_list[[i]],collapse='|'),ICD$CIM9.Stand)],
                        ICD$CIM9.Quebec[grepl(paste(icd9_list[[i]],collapse="|"),ICD$CIM9.Stand)]))
  }
  names(code)<-c('dyslipidemia','diabete','chronic_kidney','hypertension','cad','COPD','hf')

  #code for comorbidity:  1.dyslipidemia 2.diabete 3.chronic_kidney 4.hypertension 5.cad 6.COPD 7.hf 8.peripheral
  diag_subset<-diag_subset%>%mutate(comorbidity=case_when(diag_diag %in% code[[1]]~1,
                                                          diag_diag %in% code[[2]]~2,
                                                          diag_diag %in% code[[3]]~3,
                                                          diag_diag %in% code[[4]]~4,
                                                          diag_diag %in% code[[6]]~6,
                                                          diag_diag %in% code[[5]]~5,
                                                          diag_diag %in% peripheral~8,
                                                          diag_diag %in% code[[7]]~7))
# subset to first time diagnosis:
diag_subset<-diag_subset%>%select(nam,diag_diag,dtadm,dtsort,comorbidity)%>%
                           group_by(nam,comorbidity)%>%
                           arrange(dtsort)%>%
                           slice(1)%>%
                           ungroup()

# #from prescription data:
# diabete_pres<-readRDS('C:/Users/Nancy Zhu/OneDrive - McGill University/Code for thesis/R/RData files/diabete.RData')
# diabete_pres%<>%filter(nam%in%demo$nam)

############################################################################################################
#extract first time prescription:
objects <- vector(mode='list',length=6)
files<-c('asthma','beta_user','diabete','hypertension','statin_user','pvd')
filename<-paste0('C:/Users/Nancy Zhu/OneDrive - McGill University/Code for thesis/R/RData files/',files, ".RData")
for (i in seq_along(files)){
  objects[[i]]<-readRDS(filename[i])
}

names(objects)<-c('asthma','beta_blocker','diabete','hypertension','statin','pvd')

#remove prescription record with 0 quantite input
objects<-lapply(objects,function(x){x<-x%>%filter(nam %in% demo$nam,quantite!=0)
                                            return(x)})

#subset to first prescription:
objects<-lapply(objects,function(x){x<-x%>%select(nam,dt_serv)%>%
                                           group_by(nam)%>%
                                           arrange(dt_serv)%>%
                                           slice(1)%>%
                                           ungroup()
                                    return(x)})

#####################################################################################################
#For Cox model dataset creation, see 'data_clean_Cox_model.R

#For Poisson model dataset creation:
#Combine data from diagnosis and prescription
#Pick which ever event happend first as the date of confounder ascertainment, bind to terr table by year to define geographic location
#Calculate total count of confounders in each pc3 over 10 years

#add column to indicate drug category:                 code from drug:
# #code for comorbidity:  1.dyslipidemia              statin  (5)
#                         2.diabete                   diabete (3)
#                         3.chronic_kidney 
#                         4.hypertension              hypertension (4)
#                         5.cad                       beta-blocker (2)
#                         6.COPD                      asthma (1)
#                         7.hf 
#                         8.peripheral                pvd (6)

objects[[1]]['comorbidity']<-6
objects[[2]]['comorbidity']<-5
objects[[3]]['comorbidity']<-2
objects[[4]]['comorbidity']<-4
objects[[5]]['comorbidity']<-1
objects[[6]]['comorbidity']<-8

prescription<-do.call(rbind,objects)

rm(objects)

#bind diagnosis and prescription data:
combtb<-full_join(diag_subset[,c(1,2,4,5)],prescription)
demo%<>%filter(age>=66)
combtb%<>%filter(nam %in% demo$nam)

combtb$year_ascertain<-year(pmin(combtb$dtsort,combtb$dt_serv,na.rm=T))

combtb%<>%left_join(clsc[,c(1,2,4)],by=c('nam','year_ascertain'='year'))

combtb<-left_join(combtb,demo[,c(1,2,5)])

#calculate age at diagnosis or prescription, whichever happened first:
#combtb$age_ascertain<-combtb$age+combtb$year_ascertain-year(combtb$dt_index)
#combtb$age_cat<-cut(combtb$age_ascertain,breaks=c(seq(from = 60, to =85, by =5),Inf),right=F)

#calculate total number of cases for each comorbidity over the 10 years in each pc
comorb_summary<-combtb%>%group_by(pc,comorbidity)%>%summarise(n=n_distinct(nam))%>%
                         filter(pc!=999 & pc %in% fsacode)%>%
                         mutate(comorbidity=case_when(comorbidity==1~'dyslipidemia',
                                                      comorbidity==2~'diabete',
                                                      comorbidity==3~'CKD',
                                                      comorbidity==4~'hypertension',
                                                      comorbidity==5~'CAD',
                                                      comorbidity==6~'COPD',
                                                      comorbidity==7~'HF',
                                                      comorbidity==8~'PVD'))

#reorganize to wide format and bind to final ds for modelling:
comorb_summary<-spread(comorb_summary,comorbidity,n)
comorb_summary[is.na(comorb_summary)]<-0

#transform continuous count into categories by quantile:
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(x, probs = seq(0, 1, by = 0.20))), 
      labels=c("1","2","3","4","5"),include.lowest=TRUE)
}

comorb_summary_cat<-list()
for(i in seq_along(colnames(comorb_summary)[-1])){
  comorb_summary_cat[[i]]<-ApplyQuintiles((comorb_summary)[[i+1]])
}

names(comorb_summary_cat)<-colnames(comorb_summary)[2:9]

comorb_summary_cat<-as.data.frame(do.call(cbind,comorb_summary_cat))
comorb_summary_cat$pc<-comorb_summary$pc