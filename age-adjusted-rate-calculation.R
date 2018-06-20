#calculate age-adjusted AS incidence rate in each fsa area:

#calculate number of AS cases (from in hospital diagnosis) in each fsa area:
case<-readRDS('case1.RData')

case<-case%>%distinct(nam,dtsort,cyear)
clsc<-readRDS('cleaned_clsc_terr.RData')%>%rename(clsc_rez=code_clsc)

case<-left_join(case,clsc[,c(1,2,4)],by=c('nam'='nam','cyear'='year'))%>%
      filter(!is.na(pc))
demo<-fread('E:/thesis_data/demo.csv',stringsAsFactors = F)
case<-left_join(case,demo[,1:2])

#calculate age at the time AS was first diagnosed in an individual
#use year of diagnosis to calculate age (equivalent to rounding, birth date not available)
case$age_index<-case$cyear-case$year_born
case$age_index.cat<-cut(case$age_index,breaks=c(seq(from = 60, to =85, by =5),Inf),right=F)

case_count<-case%>%filter(pc %in% fsacode)%>%
                   group_by(pc,age_index.cat)%>%
                   summarise(case=n_distinct(nam))

#prepare combined table for calculation:
age_df3$V5<-as.factor(age_df3$V5)
final_tb<-age_df3%>%filter(V5 %in% c('[60,65)','[65,70)','[70,75)','[75,80)','[80,85)','[85,Inf)'))%>%
          left_join(case_count,by=c('fsa_clean'='pc','V5'='age_index.cat'))

standard_pop%<>%rename(population.std=population)
final_tb<-left_join(final_tb,standard_pop[,3:4])

#mistake in coding: H0A not assigned FSA but show up in RAMQ data (1 observation deleted)

#first calculate age specific rate per 1,000 population:
final_tb$age.specific.rate<-round(final_tb$case/final_tb$population*1000,1)

#then calculate proportion of standard population:
final_tb<-final_tb%>%group_by(fsa_clean)%>%
                     mutate(proportion=population.std/sum(population.std))%>%
                     ungroup()
final_tb$age.specific.rate[is.na(final_tb$age.specific.rate)]<-0
  
#finally calculate age-standardized rate for each fsa code:
age_standaridized<-final_tb%>%group_by(fsa_clean)%>%
                              summarise(age.standarized.rate=round(sum(age.specific.rate*proportion),1))


########################################################################################3
#now calculate age-standarized rate of AS surgical interventions:
interv<-readRDS('interv.RData')

interv<-interv%>%distinct(nam,dtsort,cyear)
interv<-left_join(interv,clsc[,c(1,2,4)],by=c('nam'='nam','cyear'='year'))%>%
  filter(!is.na(pc))
interv<-left_join(interv,demo[,1:2])

#calculate age at the time AS was first diagnosed in an individual
#use year of diagnosis to calculate age (equivalent to rounding, birth date not available)
interv$age_index<-interv$cyear-interv$year_born
interv$age_index.cat<-cut(interv$age_index,breaks=c(seq(from = 60, to =85, by =5),Inf),right=F)

interv_count<-interv%>%filter(pc %in% fsacode)%>%
  group_by(pc,age_index.cat)%>%
  summarise(interv=n_distinct(nam))

interv_final_tb<-age_df3%>%filter(V5 %in% c('[60,65)','[65,70)','[70,75)','[75,80)','[80,85)','[85,Inf)'))%>%
  left_join(interv_count,by=c('fsa_clean'='pc','V5'='age_index.cat'))%>%
  left_join(standard_pop[,3:4])%>%
  mutate(age.specific.rate=round(interv/population*1000,1))%>%
  group_by(fsa_clean)%>%
  mutate(proportion=population.std/sum(population.std))%>%
  ungroup()%>%
  mutate(age.specific.rate=ifelse(is.na(age.specific.rate),0,age.specific.rate))%>%
  group_by(fsa_clean)%>%
  summarise(age.standarized.rate=round(sum(age.specific.rate*proportion),1))
