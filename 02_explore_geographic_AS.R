# explore geographic distribution for patients diagonosed with AS and had surgical intervention across Quebec:
library(dplyr)
library(magrittr)
library(lubridate)

#find shp file for Quebec CLSC region and make maps with it:
#download shape file for CLSC regions in Quebec from 'http://www.msss.gouv.qc.ca/professionnels/informations-geographiques-et-de-population/information-geographique/'
library(rgdal)
library(maptools)

clsc_map<-readOGR(dsn='clsc_map',layer='Territoires_CLSC_2017',encoding='UTF-8',stringsAsFactors = F)
clsc_data<-clsc_map@data

# subset data to Montreal:
clsc_mtl<-clsc_map[clsc_data$RLS_code %in% c('0622','0611','0612','0621','0631','0632','0641',
                                             '0642','0651','0652','0653','0643'),]
clsc_data<-clsc_mtl@data
rm(clsc_map)


#######################################################################################################
#link to clsc residence table and subset to Montreal region:
clsc<-readRDS('cleaned_clsc_terr.RData')%>%rename(clsc_rez=code_clsc)
case1<-left_join(case1,clsc,by=c('nam'='nam','cyear'='year'))
interv<-left_join(interv,clsc,by=c('nam'='nam','cyear'='year'))

#load clsc code table:
clsc_code<-read.xlsx('LGEO_Terri_CLSC.xls',2,stringsAsFactors=F)%>%
           mutate(code_clsc=as.integer(code_clsc),
                  descript_clsc=tolower(descript_clsc))

mtl_clsc_code<-clsc_code$code_clsc[47:81]

#case_hosp<-case1%>%filter(code_clsc%in% mtl_clsc_code) #from hospitalization record
case1%<>%filter(clsc_rez %in% mtl_clsc_code) #for residence record

#interv_hosp<-interv%>%filter(code_clsc%in% mtl_clsc_code)
interv%<>%filter(clsc_rez %in% mtl_clsc_code)


#count the number of patients diagnosed with AS or having surgery Each year in Each clsc/3 digit postal code:
#plot line chart,grouped by clsc, using plotly for interaction, with bar chart on the left showing overall counts?


#look at overall trend v.s yearly trend
# there is a lagging effect from the time someone got diagonosis to having surgery! How to interpret that?
case_count<-case1%>%group_by(cyear,clsc_rez)%>%
                    summarise(n=n_distinct(nam))%>%
                    left_join(clsc_code2[,c(1,3)],by=c('clsc_rez'='code_clsc'))
                    

interv_count<-interv%>%group_by(cyear,clsc_rez)%>%
                       summarise(n=n_distinct(nam))%>%
                       left_join(clsc_code2[,c(1,3)],by=c('clsc_rez'='code_clsc'))



#plot AS cases onto Montreal map
#harmonize clsc code between RAMQ clsc data and map clsc code
clsc_mtl2<-fortify(clsc_mtl)

ggplot(clsc_mtl2) + 
  aes(long,lat,group=group) + 
  geom_polygon(fill='white',color='black')

#extract map data for region names
clsc_data<-clsc_mtl@data
clsc_data$id<-row.names(clsc_data)

#standarize clsc code between RAMQ and map:
clsc_code2<-left_join(clsc_data[,1:2]%>%mutate(CLSC_nom=tolower(CLSC_nom)),clsc_code,by=c('CLSC_nom'='descript_clsc'))
clsc_code2$code_clsc[[6]]<-6081
clsc_code2$code_clsc[[7]]<-6082
clsc_code2$code_clsc[[11]]<-6071
clsc_code2$code_clsc[[12]]<-6074
clsc_code2$code_clsc[[19]]<-6092
clsc_code2$code_clsc[[20]]<-6094
clsc_code2$code_clsc[[24]]<-6131
clsc_code2$code_clsc[[28]]<-6061
clsc_code2$code_clsc[[29]]<-6041


#link 'id' in fortified map data to clsc code in RAMQ:
id_clsc<-left_join(clsc_data[,c('id','CLSC_code')],clsc_code2)

#Refer to incidence_calculation.R file for incidence_case and incidence_interv calculation
source('incidence_calculation.R')
######################################################################################################
#link standarized rate of AS (over 10 years) diagnosis to map through CLSC code:
case_final_map<-left_join(clsc_mtl2,incidence_case)
interv_final_map<-left_join(clsc_mtl2,incidence_interv)

###############################################################################################
#make maps
#library(mapproj)
p<-ggplot(data=case_final_map,aes(long,lat,group=group,fill=rate,label=CLSC_nom)) + 
  geom_polygon(color='gray70',size=0.2)+
  #coord_map(projection='albers',lat0=39,lat1=45)+
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        panel.spacing=unit(0, "lines"),
        plot.background=element_blank(),
        legend.justification = c(0.9,0.2),
        legend.position = c(0.95,0.2)
  )+
  ggtitle('Number of diagnoed AS in Quebec \n from 2000-2011')
#+facet_wrap(~cyear)
ggplotly(p)


# p+scale_fill_gradient2(low='grey45', high='blue',mid='yellow',
#                        midpoint=median(case_final_map$n))+ #default color is set to blue, can be set

    
library(leaflet)
#reproject map in WGS84 to be plotted in leaflet:
clsc_map3 <- spTransform(clsc_mtl, CRS("+proj=longlat +datum=WGS84"))
case_count_clsc<-left_join(clsc_code2[,c(1,3)],case_count,by=c('code_clsc'='clsc_rez'))

clsc_map3@data<-left_join(clsc_map3@data,case_count_clsc)

qpal<-colorNumeric(palette= 'Blues',domain= clsc_map3$n)
leaflet()%>%addTiles()%>%
  addPolygons(data=clsc_map3,stroke=FALSE,smoothFactor = 0.2,fillOpacity = 1,
              color=~qpal(n),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))


#############################################################################################################################
#this code created 'cleaned_clsc_data.RData
#transform structure of clsc data from wide to long format, creating year column:
##load terr_clse file:
#clsc<-fread('E:/export_R/clsc.csv',stringsAsFactors = F)
# clsc_clsc<-clsc%>%select(nam,starts_with('clsc'))
# clsc_pc<-clsc%>%select(nam,starts_with('cp'))
# 
# clsc_clsc%<>%gather(key=year,value=code_clsc,clsc_2000:clsc_2010,na.rm=T)
# #transform year into numeric with year only:
# clsc_clsc$year<-as.numeric(gsub('clsc_','',clsc_clsc$year))
# 
# clsc_pc<-clsc_pc%>%gather(key=year,value=pc,cp_2000:cp_2010,na.rm=T)%>%
#                    mutate(year=as.numeric(gsub('cp_','',year)))
# clsc_pc%<>%filter(pc!='')
# 
# clsc<-left_join(clsc_clsc,clsc_pc)
# rm(clsc_clsc,clsc_pc)
# 
# clsc_postal<-clsc%>%distinct(code_clsc,pc)

# #save new version of terr_clsc
# saveRDS(clsc,'../RData files/cleaned_clsc_terr.Rdata')
##################################################################################################################################

# #link postal code to administrative region for mapping:
# #table taken from election quebec
# cp_admin<-read.delim('C:/Users/nzhu/Desktop/Thesis project/CP_CIRC_MUN_MRC_RA_BRUT.txt',header=T,sep=';',stringsAsFactors = F)
# #create 3 digit postal code variable:
# 
# cp_admin$cp3<-substr(cp_admin$CO_POSTL,1,3)

####################################################################################################################