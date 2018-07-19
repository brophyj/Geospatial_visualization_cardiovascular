#map social deprivation index in Montreal:

#For quick analysis, take median auintosoc and quintmat for all available years: use pc code from 2006

deprivation_index<-fread('E:/thesis_data/inde_def.csv',stringsAsFactors = F)

#impute missing values in the table
#rule: 0 in quint indicates missing values, impute the value based on the reference year and fsa
#assign median quint in each fsa in a given year to missing values

deprivation_index%<>%arrange(annee_financ,cp3)


#remove pc code: H0A, H0M and H0H, not assigned postal code geographically, causing deletion of 113 obs in clsc over 10 years
#small impact (Akwesasn not counted)
`%ni%`<-Negate(`%in%`)
sociale_med<-deprivation_index%>%filter(cp3 %ni% c('H0A','H0M','H0H'))%>%
                             group_by(annee_financ,cp3)%>%
                             summarise(sociale_median=round(median(quintsoc),1))%>%
                             arrange(annee_financ,cp3)%>%
                             mutate(sociale_median_imp=ifelse(sociale_median==0,lag(sociale_median),sociale_median))

#if 0 as median, impute value based on the same year with a previous cp3 code:
materielle<-deprivation_index%>%
  filter(cp3 %ni% c('H0A','H0M','H0H'))%>%
  group_by(annee_financ,cp3)%>%
  summarise(materielle_median=round(median(quintmat),1))%>%
  arrange(annee_financ,cp3)%>%
  mutate(materielle_median_imp=ifelse(materielle_median==0,lead(materielle_median),materielle_median))

deprivation_index<-deprivation_index%>%
                   left_join(sociale_med[,c(1,2,4)])%>%
                   left_join(materielle[,c(1,2,4)])%>%
                   mutate(quintmat=ifelse(quintmat==0,materielle_median_imp,quintmat),
                          quintsoc=ifelse(quintsoc==0,sociale_median_imp,quintsoc))%>%
                   filter(cp3 %ni% c('H0A','H0M','H0H'))

#re-calculate median for imputed quintiles:
sociale<-deprivation_index%>%group_by(cp3)%>%
                       summarise(sociale_median=median(quintsoc))

materielle<-deprivation_index%>%group_by(cp3)%>%
                                summarise(materielle_median=median(quintmat))


saveRDS(sociale,'sociale.RData')
saveRDS(materielle,'materielle.RData')
