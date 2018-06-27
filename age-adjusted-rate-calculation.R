source('parse_census_xml.R')

age_df3$V5<-as.factor(age_df3$V5)
standard_pop%<>%rename(population.std=population)
clsc<-readRDS('cleaned_clsc_terr.RData')%>%rename(clsc_rez=code_clsc)

##############################################################################################################
#calculate age-adjusted AS incidence rate in each fsa area:

#calculate number of AS cases (from in hospital diagnosis) in each fsa area:
case<-readRDS('case1.RData')
case<-case%>%distinct(nam,dtsort,cyear)%>%
      left_join(case,clsc[,c(1,2,4)],by=c('nam'='nam','cyear'='year'))%>%
      filter(!is.na(pc))%>%
      left_join(case,demo[,c(1:2,5)])

#calculate age at the time AS was first diagnosed in an individual
#use year of diagnosis to calculate age (equivalent to rounding, birth date not available)
case$age_index<-as.numeric(case$age+(case$dtsort-case$dt_index)/365)
case$age_index.cat<-cut(case$age_index,breaks=c(seq(from = 60, to =85, by =5),Inf),right=F)

case_count<-case%>%filter(pc %in% fsacode)%>%
                   group_by(pc,age_index.cat)%>%
                   summarise(case=n_distinct(nam))

#prepare combined table for calculation:

final_tb<-age_df3%>%filter(V5 %in% c('[65,70)','[70,75)','[75,80)','[80,85)','[85,Inf)'))%>%
          left_join(case_count,by=c('fsa_clean'='pc','V5'='age_index.cat'))%>%
          left_join(final_tb,standard_pop[,3:4])

#mistake in coding: H0A not assigned FSA but show up in RAMQ data (1 observation deleted)

#first calculate age specific rate per 1,000 population:
final_tb$age.specific.rate<-round(final_tb$case/final_tb$population*1000,1)

#then calculate proportion of standard population:
final_tb<-final_tb%>%group_by(fsa_clean)%>%
                     mutate(proportion=population.std/sum(population.std))%>%
                     ungroup()

final_tb$age.specific.rate[is.na(final_tb$age.specific.rate)]<-0
final_tb$age.specific.rate[final_tb$age.specific.rate==Inf]<-0 #deal with mistakes from census data
#where cases were shown but age-specific population were zero
  
#finally calculate age-standardized rate for each fsa code:
age_standaridized<-final_tb%>%group_by(fsa_clean)%>%
                              summarise(age.standarized.rate=round(sum(age.specific.rate*proportion),1))


########################################################################################3
#now calculate age-standarized rate of AS surgical interventions:
interv<-readRDS('interv.RData')

interv<-interv%>%distinct(nam,dt_interv,cyear)
interv<-left_join(interv,clsc[,c(1,2,4)],by=c('nam'='nam','cyear'='year'))%>%
  filter(!is.na(pc))
interv<-left_join(interv,demo[,c(1:2,5)])

#calculate age at the time AS was first diagnosed in an individual
#use year of diagnosis to calculate age (equivalent to rounding, birth date not available)
interv$age_index<-as.numeric(interv$age+(interv$dt_interv-interv$dt_index)/365)
interv$age_index.cat<-cut(interv$age_index,breaks=c(seq(from = 60, to =85, by =5),Inf),right=F)

interv_count<-interv%>%filter(pc %in% fsacode)%>%
  group_by(pc,age_index.cat)%>%
  summarise(interv=n_distinct(nam))

interv_final_tb<-age_df3%>%filter(V5 %in% c('[65,70)','[70,75)','[75,80)','[80,85)','[85,Inf)'))%>%
  left_join(interv_count,by=c('fsa_clean'='pc','V5'='age_index.cat'))%>%
  left_join(standard_pop[,3:4])%>%
  mutate(age.specific.rate=round(interv/population*1000,1))%>%
  group_by(fsa_clean)%>%
  mutate(proportion=population.std/sum(population.std))%>%
  ungroup()%>%
  mutate(age.specific.rate=ifelse(is.na(age.specific.rate),0,age.specific.rate),
         age.specific.rate=ifelse(age.specific.rate==Inf,0,age.specific.rate))%>%
  group_by(fsa_clean)%>%
  summarise(age.standarized.rate=round(sum(age.specific.rate*proportion),1))



########################################################################################3
#calculate age-standarized rate of acute myocardial infarction:
#icd code for acute MI: 
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

mi_count<-acute_mi%>%filter(pc %in% fsacode)%>%
  group_by(pc,age_index.cat)%>%
  summarise(mi=n_distinct(nam))

mi_final_tb<-age_df3%>%filter(V5 %in% c('[65,70)','[70,75)','[75,80)','[80,85)','[85,Inf)'))%>%
  left_join(mi_count,by=c('fsa_clean'='pc','V5'='age_index.cat'))%>%
  left_join(standard_pop[,3:4])%>%
  mutate(age.specific.rate=round(mi/population*1000,1))%>%
  group_by(fsa_clean)%>%
  mutate(proportion=population.std/sum(population.std))%>%
  ungroup()%>%
  mutate(age.specific.rate=ifelse(is.na(age.specific.rate),0,age.specific.rate),
         age.specific.rate=ifelse(age.specific.rate==Inf,0,age.specific.rate))%>%
  group_by(fsa_clean)%>%
  summarise(age.standarized.rate=round(sum(age.specific.rate*proportion),1))

saveRDS(mi_final_tb,'MI_age_incidence.RData')





######################################################################################################
#function to calculate age-standarized incidence rate using in-patient data from RAMQ:

