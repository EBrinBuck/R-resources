library(gtrendsR)
library(ggplot2)
library(tidyverse)
library(usmap)
library(ggthemes)


#select keywords
words=c("flood","drought","dam")
#select region or area
# for state use geo="US-NE" (i.e. Nebraska)
country='US'
#set the time window
#time='all' to set since start of Gtrends
time=("2010-01-01 2020-01-01")
#set source (i.e. web, news, youtube, etc)
source1='news'

#get google trends data
watertrends = gtrends(words, gprop =source1, geo=country, time = time )
#for interest over time 
waterovertime=watertrends$interest_over_time

###plot interest over time
ggplot(data=waterovertime, aes(x=date, y=hits,group=keyword,col=keyword))+
  geom_line()+xlab('Time')+ylab('Relative Interest')+
  theme_clean()+theme(legend.title = element_blank(),legend.position="bottom")+
  ggtitle("Google Trends: news search volume")

##simplified one string of code
plot(gtrendsR::gtrends(keyword = c("irrigation", "dams", "diversions"), geo = "US", time = "2010-01-01 2020-01-01"))

###############################
##Map resources: https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
##code modified from https://www.littlemissdata.com/blog/usmap


###########
##see google trends data for keyword flood, in the us, since google trends began in 2004
blue<-"#03254c" #specify color
flood <- gtrends("flood",geo = "US", time = "all")
##convert to state and then to FIPs code (Federal Information Processing Standards (2 char for state, 5 char for county)
floodStates <- flood$interest_by_region
floodStates$fips <-fips(floodStates$location)
###plot the map
plot_usmap(data = floodStates, values = "hits",  color = blue, labels=FALSE) + 
  scale_fill_continuous( low = "white", high = blue, name = "Popularity") + 
  theme(legend.position = "right") + 
  labs(title = "Popularity of 'Flood' Google Search by State since 2004")

##keyword drought
rust<-"#b7410e" #specify color
drought <- gtrends("drought",geo = "US", time = "all")
##convert to state and then to FIPs code (FIPS codes (2 char for state, 5 char for county)
droughtStates <- drought$interest_by_region
droughtStates$fips <-fips(droughtStates$location)
###plot the map
plot_usmap(data = droughtStates, values = "hits",  color = "#505050", labels=FALSE) + 
  scale_fill_continuous( low = "white", high = rust, name = "Popularity") + 
  theme(legend.position = "right") + 
  labs(title = "Popularity of 'Drought' Google Search by State since 2004")

