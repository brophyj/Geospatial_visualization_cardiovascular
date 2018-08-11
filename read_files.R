library(pacman)
p_load(dplyr,ggplot2,tidyr,stringr,xlsx,magrittr,data.table,lubridate)


directory<-'E:/thesis_data/'
demo_filepath<-paste0(directory,'demo.csv')
interv_filepath<-paste0(directory,'me_interv.csv')
diag_filepath<-paste0(directory,'me_diag.csv')
sejour_filepath<-paste0(directory,'me_sejour.csv')


demo<-fread(demo_filepath,stringsAsFactors = F) 
#no single individual was classified in more than one cohort, independent cohort
#limit age from 67 to 100
demo<-demo%>%select(nam,age,sexe,cohort,dt_index)%>%distinct()
#convert dt_index:
demo$dt_index<-as.Date(demo$dt_index,origin = "1960-01-01")
summary(demo$dt_index)
#219246 individuals aged between 67 and 100 during the follow up period from 2001-04-01 to 2011-03-31

#study cutoff date: 2011-03-31, there are follow-up information up to 2012 for me_diag and me_interv:

me_interv<-fread(interv_filepath,stringsAsFactors = F)%>%
  mutate(dt_interv=as.Date(dt_interv,origin='1960-01-01'))%>%
  filter(nam %in% demo$nam,dt_interv<as.Date('2011-03-31'))

me_diag<-fread(diag_filepath,stringsAsFactors = F)%>%filter(nam %in% demo$nam)
sejour<-fread(sejour_filepath,stringsAsFactors = F)%>%filter(nam%in% demo$nam)
sejour[,c('dtadm','dtsort')]<-lapply(sejour[,c('dtadm','dtsort')],function(x)as.Date(x,origin='1960-01-01'))
sejour%<>%filter(dtsort<as.Date('2011-03-31'))

ICD<-read.xlsx2('C:/Users/Nancy Zhu/OneDrive - McGill University/Thesis project/CIM10CA_v_CIM9_conv_Quebec_2006-2009 to play lyne.xls',1,stringsAsFactors=F)
CCI<-read.xlsx2('C:/Users/Nancy Zhu/OneDrive - McGill University/Thesis project/CCI_vCCADTC_conv_Quebec_2006-2009 order by CCA-CCP.xls',1,stringsAsFactors=F)

#check unique number of nam in each dataset:
# length(unique(me_diag$nam))
# length(unique(me_interv$nam))
# length(unique(sejour$nam))

#setdiff(demo$nam,me_diag$nam)

#missing information of diagnostic and sejour for about 2409 people compared to demo
#what about in prescription data? billing data?

#all 2409 people had a billing code during follow-up (use billing code to ascertain AS intervention?) #reason for people with intervention code but no diagnostic code?
#need to ascertain cases by diagnostic,intervention and billing code.


#ggplot(data=interv%>%group_by(year(dt_interv))%>%summarise(count=n_distinct(nam)),aes(x=`year(dt_interv)`,y=count))+
#  geom_bar(stat='identity')
