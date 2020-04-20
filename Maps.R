##Map resources: https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
##code modified from https://www.littlemissdata.com/blog/usmap

library(gtrendsR)
library(tidyverse)
library(usmap)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)

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

#################
##rivers
##data/code modified from https://www.r-bloggers.com/how-to-map-geospatial-data-usa-rivers/
library(maptools)
library(mapproj)
library(broom)

#load data from sharpsightlabs:
#url.river_data <- url("http://sharpsightlabs.com/wp-content/datasets/usa_rivers.RData")
df.usa_rivers <- fortify(lines.rivers)
UScountry <- map_data("usa")
states <- map_data("state")

PlatteBasin <- subset(states, region %in% c("colorado", "wyoming", "nebraska"))
PBrivers <- subset(lines.rivers, STATE %in% c('NE','CO','WY'))
PBrivers2<-tidy(PBrivers)

ggplot() +
  geom_polygon(data = PlatteBasin, 
  aes(x = long, y = lat, group = group), color= "white", fill = "grey40") +
  geom_path(data = PBrivers3, aes(x = long, y = lat, group = group), color="#99ccff")+
  theme_few()+ theme(legend.position = "none") +ggtitle("Rivers of WY, CO, & NE")
