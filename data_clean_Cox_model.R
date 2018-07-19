#define study cohort:

#Inclusion criteria: Individuals with AS diagnosis from in-hospital admission, billing diagnosis code
#                      Individuals with at least one year follow-up history
# Follow-up end point:
#   1. Having an SAVR procedure
#   2. Death
#   3. Study end 2011-03-01
  
#filter in-patient data with diagnostic codes:(both icd 9 and icd 10 code)
AS<-c('4241','I350','I351','I352','I358','I359','I391')

#link individuals to hospital admission date, select for first time diagnosis
case1<-me_diag%>%filter(diag_diag %in% AS)%>%select(nam,diag_diag,no_seq,type_diag)%>%distinct()

#keep only first time diagnosis:
case1<-left_join(case1,distinct(sejour[,c('no_seq','nam','dtsort')]))%>%
  select(nam,dtsort)%>%
  left_join(demo[,c(1,2,5)])%>%
  group_by(nam)%>%
  filter(dtsort==min(dtsort))%>%
  ungroup()%>%
  distinct() #25797 screened in from in-patient record)

#exclude individuals with less than 1 year history
case1%<>%filter(dt_index<=dtsort) #20262 individuals

#add extra from billing data: (only those without a hospital admission data)
bill_subset<-bill_as%>%filter(nam %in% as_id)
bill_subset$dt_serv<-as.Date(bill_subset$dt_serv,origin="1960-01-01")

#link to cohort entry date: (remove individuals not in demo)
bill_subset<-left_join(bill_subset,demo[,c(1,2,5)])%>%filter(!is.na(dt_index))

case2<-bill_subset%>%group_by(nam)%>%
                     arrange(dt_serv)%>%
                     slice(1)%>%
                     ungroup()%>%
                     filter(dt_index<=dt_serv)   #individuals with diagnosis from billing code
case2%<>%rename(dtbill=dt_serv)

#define diagnosis date for individuals with a hospital admission and bill code,the earliest date of diagnosis possible
#add extra cases from billing only:
case1<-left_join(case1,case2[,c('nam','dtbill','dt_index','age')])
case1$study_entry<-pmin(case1$dtsort,case1$dtbill,na.rm=T)

`%ni%`<-Negate('%in%')
case2<-case2%>%filter(nam %ni% case1$nam )%>%rename(study_entry=dtbill)
cohort<-bind_rows(case1[,c('nam','age','dt_index','study_entry')],
                  case2[,c('nam','age','dt_index','study_entry')])               

#remove individuals who died on the day of admission:

#calculate age at study entry (time of AS diagnosis):
cohort$age_entry<-round(cohort$age+as.numeric(cohort$study_entry-cohort$dt_index)/365,1)

rm(case1,case2,bill_as,bill_as_1,bill_as_2,bill_subset)

# ---ascertain study end date: (from AS surgery billing and death registery)
savr<-bill_savr%>%filter(nam %in% cohort$nam)%>%
                  left_join(cohort[,c('nam','study_entry')])%>%
                  mutate(dt_serv=as.Date(dt_serv,origin='1960-01-01'))%>%
                  filter(dt_serv>study_entry)%>%
                  group_by(nam)%>%
                  arrange(dt_serv)%>%
                  slice(1)%>%
                  ungroup()

cohort$outcome<-ifelse(cohort$nam %in% savr$nam,1,0)
cohort<-left_join(cohort,savr%>%rename(savrdt=dt_serv)%>%select(nam,savrdt))
rm(savr,bill_savr)

# death:
deces<-deces%>%filter(nam %in% cohort$nam)%>%select(nam,Dt_dec,causeini)
deces$Dt_dec<-ymd(deces$Dt_dec)
cohort<-left_join(cohort,deces[,1:2])
cohort$study_end<-ymd('2011-03-31')
cohort$study_exit<-pmin(cohort$savrdt,cohort$Dt_dec,cohort$study_end,na.rm=T)


#remove individuals who exit the study on the same day as entry:
cohort%<>%filter(study_exit>study_entry)

# ---- ascertain covariates

#1. ascertain geographic location during FU:
#for simplicity use postal code at study_entry, if available
cohort_clsc<-cohort%>%select(nam,study_entry,study_exit)%>%
                      mutate(yr_entry=year(study_entry),
                             yr_exit=year(study_exit))%>%
                      left_join(clsc[,c(1,2,4)])%>%
                      filter(year==yr_entry)
#    835 individual have missing pc at year of entry, impute pc from previous years:
#reason for missing: 
#individual died before April 1 on the year
clsc_imp<-clsc%>%filter(nam %in% setdiff(cohort$nam,cohort_clsc$nam))%>%
                 left_join(cohort[,c('nam','study_entry','study_exit')])%>%
                 group_by(nam)%>%
                 arrange(desc(year))%>%
                 slice(1)%>%
                 ungroup()
cohort_clsc<-rbind(cohort_clsc[,c('nam','pc')],clsc_imp[,c('nam','pc')])
cohort<-left_join(cohort,cohort_clsc)
rm(clsc_imp,clsc,cohort_clsc)

#subset to residents in Montreal region:
fsacode<-read.csv('montreal_fsa.csv',stringsAsFactors = F,header=F)
fsacode<-unlist(str_extract_all(fsacode$V1,'H\\d{1}[[:upper:]]{1}'))

cohort%<>%filter(pc %in% fsacode)
#6576 total cohort size  1025 outcome


#2. ascertain materielle and sociale deprivation index:
materielle<-readRDS('materielle.RData')
sociale<-readRDS('sociale.RData')

cohort<-left_join(cohort,materielle,by=c('pc'='cp3'))%>%
        left_join(sociale,by=c('pc'='cp3'))
#1 individual with pc H0A in RAMQ, H0A is not assigned in montreal, impute number from H1A
cohort$materielle_median[cohort$pc=='H0A']<-materielle$materielle_median[materielle$cp3=='H1A']
cohort$sociale_median[cohort$pc=='H0A']<-sociale$sociale_median[sociale$cp3=='H1A']

# 3. ascertain comorbidities: CAD, COPD, CKD and etc. Use functions from thesis project:
cohort$diabete_combined<-ifelse(cohort$diabete==1|cohort$nam%in% unique(objects_subset[[3]]$nam),1,0)
cohort$hypertension_combined<-ifelse(cohort$hypertension==1 | cohort$nam%in% unique(objects_subset[[4]]$nam),1,0)
cohort$pvd_combined<-ifelse(cohort$peripheral==1|cohort$nam%in% unique(objects_subset[[7]]$nam),1,0)
cohort$cad_combined<-ifelse(cohort$CAD==1 | cohort$nam%in% unique(objects_subset[[2]]$nam),1,0)
cohort$copd_combined<-ifelse(cohort$COPD==1 | cohort$nam%in% unique(objects_subset[[1]]$nam),1,0)
cohort$hyperlipidemia_combined<-ifelse(cohort$dyslipidemia==1 | cohort$statin==1,1,0)

#add acute MI : (from in-hospital admission)
acute_mi<-c('4109',unique(ICD$CIM.10.CA[ICD$CIM9.Quebec=='4109']))
acute_mi<-me_diag%>%filter(diag_diag%in% acute_mi,nam %in% cohort$nam)
acute_mi<-acute_mi%>%left_join(sejour[,c(1:2,6)],by=c('nam','no_seq'))%>%
                     group_by(nam)%>%
                     filter(dtsort==min(dtsort))%>%
                     ungroup()%>%
                     left_join(cohort[,c('nam','study_entry')])%>%
                     filter(dtsort<study_entry)
cohort$mi<-ifelse(cohort$nam %in% acute_mi$nam,1,0)                    
cohort<-left_join(cohort,demo[,c('nam','sexe')])

cohort$futime<-cohort$study_end-cohort$study_entry