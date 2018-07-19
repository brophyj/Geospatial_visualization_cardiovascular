#this file calculates the cumulative incidence rate of AS in population above 65
#the result was used in the mapping in the explore_geographic_AS.R file


pop<-read.csv('CLSC_population.csv',stringsAsFactors = F)

#subset to CLSC population in Montreal:
pop<-pop[pop$Niveau.géographique=='CLSC' & pop$Code.du.territoire %in% as.numeric(id_clsc$CLSC_code),]

#select total population above 65
pop<-pop[pop$Sexe=='Total',c('Territoire','Code.du.territoire','X65.ans.ou.plus','Année')]

#subset population between year 2000 and 2010:
pop<-pop[pop$Année<=2010&pop$Année>=2000,]

#calculate 10 year incident rate of AS/1000 population above 65 using data from 2000 to 2010
#, then take the mean for each neighborhood:

#first calculate mean population in each CLSC region over 10 year
pop_10y<-pop%>%group_by(Code.du.territoire)%>%
               summarise(mean_pop=round(mean(X65.ans.ou.plus),0))

#calculate cumulative 10 yr incidence rate of AS in each CLSC region:
incidence_case<-case_count%>%group_by(CLSC_code)%>%
                             summarise(incidence.case=sum(n))%>%
                             mutate(CLSC_code=as.numeric(CLSC_code))%>%
                             right_join(pop_10y,by=c('CLSC_code'='Code.du.territoire'))%>%
                             mutate(rate=round(incidence.case*1000/(mean_pop*10),2))%>%
                             left_join(id_clsc[,1:3]%>%mutate(CLSC_code=as.numeric(CLSC_code)))

incidence_interv<-interv_count%>%group_by(CLSC_code)%>%
  summarise(incidence.case=sum(n))%>%
  mutate(CLSC_code=as.numeric(CLSC_code))%>%
  right_join(pop_10y,by=c('CLSC_code'='Code.du.territoire'))%>%
  mutate(rate=round(incidence.case*1000/(mean_pop*10),2))%>%
  left_join(id_clsc[,1:3]%>%mutate(CLSC_code=as.numeric(CLSC_code)))