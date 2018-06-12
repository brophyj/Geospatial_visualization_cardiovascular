#map incidence of AS cases to FSA map:
library(pacman)
p_load('dplyr','lubridate','ggplot2','rgdal','maptools')

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