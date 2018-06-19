#map incidence of AS cases to FSA map:
library(pacman)
p_load('dplyr','lubridate','ggplot2','rgdal','maptools','RColorBrewer')

#unzip FSA_map.zip:
unzip('FSA_map.zip',exdir = 'FSA_map')
fsa_map<-readOGR(dsn='FSA_map',layer='gfsa000b11a_e',encoding='UTF-8',stringsAsFactors = F)
fsa_data<-fsa_map@data

#subset map to Montreal region:
#need to get a list of FSA in Montreal (scrap from online)
library(stringr)
fsacode<-read.csv('montreal_fsa.csv',stringsAsFactors = F,header=F)
fsacode<-unlist(str_extract_all(fsacode$V1,'H\\d{1}[[:upper:]]{1}'))
fsa_mtl<-fsa_map[fsa_data$CFSAUID %in% fsacode,]
fsa_data<-fsa_mtl@data
rm(fsa_map)


#merge age standardized rate to mapping:
fsa_mtl2<-fortify(fsa_mtl)
fsa_data$id<-row.names(fsa_data)

fsa_mtl2<-left_join(fsa_mtl2,fsa_data[,c(1,4)])
fsa_mtl2<-left_join(fsa_mtl2,age_standaridized,by=c('CFSAUID'='fsa_clean'))
fsa_mtl2$age.standarized.rate[is.na(fsa_mtl2$age.standarized.rate)]<-0

p<-ggplot(data=fsa_mtl2,aes(long,lat,group=group,fill=age.standarized.rate,label=CFSAUID)) + 
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
        legend.position = c(0.6,0.2)
  )+
  ggtitle('Age-standarized incidence AS rate in Quebec \n from 2000-2011')

p<- p+scale_fill_distiller(type = "seq", palette = 14, direction = 1)

ggplotly(p)
