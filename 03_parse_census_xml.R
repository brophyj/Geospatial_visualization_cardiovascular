#extract census data:
library(xml2)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)

#age specific population data:
age<-read_xml('./2006_census/Age_sex/Generic_94-575-XCB2006003.xml')


value<-xml_find_all(age,'//generic:ObsValue')%>%xml_attr('value')
geo<-xml_find_all(age,'//generic:Value[@concept="GEO"]')%>%xml_attr('value')
age_cat<-xml_find_all(age,'//generic:Value[@concept="A06_SexAge49_D1"]')%>%xml_attr('value')

age_df<-data.frame(age_cat=age_cat,
                   fsa=geo,
                   population=value,stringsAsFactors = F)

fsacode<-read.csv('montreal_fsa.csv',stringsAsFactors = F,header=F)
fsacode<-unlist(str_extract_all(fsacode$V1,'H\\d{1}[[:upper:]]{1}'))
#clean up table:
age_df$fsa_clean<-str_extract(age_df$fsa,'[[:upper:]]\\d[[:upper:]]')
age_df2<-age_df%>%filter(fsa_clean %in% fsacode)

#[1] "H4T" "H0A" "H0M" "H4Z" "H5A" "H5B" fsa doesn't exit in 2006 census
#subset data to montreal region with fsa:

age_row<-read.csv('census_age_sample.csv',stringsAsFactors = F,header=F)
age_row$V1<-as.character(age_row$V1)
age_row$V5<-NA
age_row$V5[c(4:21,23:40)]<-as.character(unique(cut(1:85,breaks=c(seq(from = 0, to = 85, by =5),Inf),right=F)))
age_df2<-left_join(age_df2,age_row[,c(1,3,5)],by=c('age_cat'='V1'))

#transform to wide format:
age_df3<-age_df2%>%filter(!is.na(V5))%>%
                   select(-age_cat,-fsa)%>%
                   spread(V3,population)

age_df3$population<-as.numeric(age_df3$Female)+as.numeric(age_df3$Male)
age_df3%<>%select(-Female,-Male)

#using 2006 Canadian age specific population as standard population:
standard_pop<-age_df%>%filter(fsa=='01')%>%
  left_join(age_row[,c(1,3,5)],by=c('age_cat'='V1'))

#calculate total age-specific population:
standard_pop<-standard_pop%>%filter(!is.na(V5))%>%
  select(-age_cat)%>%
  spread(V3,population)%>%
  mutate(population=as.numeric(Female)+as.numeric(Male))%>%
  select(-Female,-Male,-fsa_clean)

rm(age_df2,age_df,age)



# income data
income<-read_xml('./2006_census/income/Generic_94-581-XCB2006003.xml')

