library(dplyr)
library(magrittr)
library(lubridate)
library(stringr)


###################################################################################################################################################
#selecting AS case from diagnostic and intervention:
#filter in-patient data with diagnostic codes:(both icd 9 and icd 10 code)

#load inpatient diagnosis and intervention data
source('read_files.RData')
source('load_billing_data.R')

#define aortic stenosis
AS<-c('4241','I350','I351','I352','I358','I359','I391')

#procedure code for surgical surgery and TAVA
interv_cci<-CCI[grepl('1HV',CCI$CCI),]
#convert CCI code to 7 digit format:
interv_cci$CCI<-substr(interv_cci$CCI,1,7)
AS_p<-unique(c(interv_cci$CCI,interv_cci$CCA.stand))


#link individuals to hospital admission date, select for first time diagnosis
case1<-me_diag%>%filter(diag_diag %in% AS)%>%select(nam,diag_diag,no_seq,type_diag)%>%distinct()

case1<-left_join(case1,sejour[,c('nam','no_seq','dtsort')])%>%
  left_join(demo[,c(1,5)])%>%
  group_by(nam)%>%
  filter(dtsort==min(dtsort))%>%
  ungroup()%>%
  distinct(nam,dtsort,dt_index)

#add cases from outpatient diagnosis:
bill_as<-lapply(bill,function(x)x[x$diag %in% AS,])
bill_as<-do.call(rbind,bill_as)

#subjects with at least two diagnosis,extract the first-time diagnosis:
bill_as_1<-bill_as%>%group_by(nam)%>%filter(n()>1)%>%
  arrange(dt_serv)%>%
  slice(1)%>%
  ungroup()

#subject with a diagnosis by cardiologist:
bill_as_2<-bill_as%>%filter(sp_prof==6)

#subjects with AS defined by bill info:
as_id<-unique(bill_as_1$nam,bill_as_2$nam)

as_bill<-bill_as%>%filter(nam %in% as_id)%>%
  select(1:3)%>%
  left_join(demo[,c(1,5)])%>%
  filter(!is.na(dt_index))%>%
  mutate(dt_serv=as.Date(dt_serv,origin="1960-01-01"))%>%
  group_by(nam)%>%
  filter(dt_serv==min(dt_serv))%>%
  ungroup()%>%
  distinct(nam,dt_serv,age,dt_index)
rm(bill_as,bill_as_1,bill_as_2,as_id)

sum(as_bill$nam %in% case1$nam)
#11465 individuals had both diagnosis from in-hospital diagnosis and outpatient diagnosis
#combine to in hospital admission data, de-duplicate rows
`%ni%`<-Negate(`%in%`)
as_bill<-as_bill%>%filter(nam %ni% case1$nam)
as_bill%<>%rename(dtsort=dt_serv)

case1<-rbind(case1,as_bill)
 

#first time intervention
interv<-me_interv%>%filter(code_interv %in% AS_p)%>%
  select(nam,dt_interv,no_seq)%>%
  left_join(demo[,c(1,5)])%>%
  group_by(nam)%>%
  filter(dt_interv==min(dt_interv))%>%
  ungroup()%>%
  distinct(nam,dt_interv,dt_index)

 #load physician billing history, remove those who did not show in billing
sum(interv$nam %in% bill_savr$nam) #12320 out of 13316 has a billing code

interv%<>%filter(nam %in% bill_savr$nam)

#11464 individuals had both diagnosis and intervention of AS



case1$cyear<-year(case1$dtsort)  #saved as case1.Rdata
interv$cyear<-year(interv$dt_interv) #saved as interv.Rdata


fsacode<-read.csv('montreal_fsa.csv',stringsAsFactors = F,header=F)
fsacode<-unlist(str_extract_all(fsacode$V1,'H\\d{1}[[:upper:]]{1}'))



