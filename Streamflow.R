#USGS R packages help, resources, tutorials: 
# https://owi.usgs.gov/R/training-curriculum/intro-curriculum/USGS/


library(dataRetrieval)##USGS streamflow 
library(ggplot2)##graphs
library(pastecs)##descriptive stats
library(reshape2)##melt
library(ggthemes)##themes for graphs
library(dplyr)##organize data
library(ggridges)##ridge plot 
library(viridis)##nicer colors for graphs
library(ggpubr)



#USGS parameter code- variables (not complete)
###pCode	shortName
###00060	Discharge [ft3/s]
###00065	Gage height [ft]
###00010	Temperature [C]
###00045	Precipitation [in]
###00400	pH
###00095  Specific conductance 
###00300  Dissolved oxygen (mg/L)


###Platte River near Ashland, NE 06801000
PRA<-readNWISdv("06801000",parameterCd=c("00010", "00060","00300"),startDate="2019-05-10", endDate="2020-01-01")
head(PRA)
PRA<-PRA %>% 
  rename(PRA,
         Temperature=X_00010_00003,
         Streamflow=X_00060_00003, DissolvedOxygen=X_00300_00003)
PRA2<-select(PRA,Date, Temperature, Streamflow, DissolvedOxygen)
PRA2$month<- format(as.Date(PRA2$Date) , "%b") ##add variable month 


##summary stats
stat.desc(PRA2)
round(stat.desc(PRA2), 2) ##round to 2 decimal places

##basic bubble plot
ggplot(PRA2, aes(x=Date, y=Streamflow, size = DissolvedOxygen, fill=Temperature)) +
  geom_point(alpha=0.7, shape=21, color="black")+theme_clean()

###line plot
ggplot(PRA2, aes(x=Date, y=DissolvedOxygen)) +
  geom_smooth(span=0.01, color="#003366")+theme_clean()


###linear regression with base R
###Note: not necessarily recommended for time-series but here as example
# lm(y~x)
model1<-lm(DissolvedOxygen~Temperature, data=PRA2)
model2<-lm(DissolvedOxygen~Streamflow, data=PRA2)
#get coefficients and fit
summary(model1)
summary(model2)


###Plot line fit and get stats from ggpubr package
#model1
ggscatter(PRA2, x = "Temperature", y = "DissolvedOxygen", add = "reg.line") +
  stat_cor(label.x = 20, label.y = 15) +
  stat_regline_equation(label.x = 15, label.y = 11)
#model2
ggscatter(PRA2, x = "Streamflow", y = "DissolvedOxygen", add = "reg.line") +
  stat_cor(label.x = 40000, label.y = 15) +
  stat_regline_equation(label.x = 20000, label.y = 6)


###Streamflow data
###Platte at Grand Island 06770500 (04/01/1934) or at Kearney 06770200 (only 1980s)
Platte<-readNWISdv("06770500","00060",
                   "1927-01-01","2020-04-01")

###Gila 09430500 (12/01/1927)
Gila<-readNWISdv("09430500","00060",
                 "1927-01-01","2020-04-01")
##Rio Grande 08319000 (01/01/1927)
RG<-readNWISdv("08319000","00060",
               "1927-01-01","2020-04-01")
##rename streamflow variable named 'X_00060_00003' to the name of the river
RG<-rename(RG,RioGrande=X_00060_00003)
Gila<-rename(Gila,Gila=X_00060_00003)
Platte<-rename(Platte,Platte=X_00060_00003)

##merge into one dataset
GRG<-merge(RG, Gila, by.x="Date", by.y="Date")
PGR<-merge(GRG, Platte, by.x="Date", by.y="Date")
streamflow<- subset(PGR, select = c(Date, RioGrande, Gila, Platte))

##format date as date and add 3 additional columns for year, month, and decade
streamflow$Date<-as.Date(streamflow$Date)
streamflow[, "year"] <- format(streamflow[,"Date"], "%Y")
streamflow[, "month"] <- format(streamflow[,"Date"], "%m")
floor_decade    = function(value){ return(value - value %% 10) } #function to calculate decade
streamflow[, "decade"] <- floor_decade(as.numeric(streamflow$year))
streamflow$decade<-as.factor(streamflow$decade)

###descriptive stats
options(scipen=100)
options(digits=0)
stat.desc(streamflow[2:4])

## change data from wide format dataframe to long format
sf.m<-melt(streamflow, value.name="Streamflow", variable.name="River", id.vars=c("Date","year","month","decade"))
##reorder Rivers 
sf.m$River<-factor(sf.m$River, levels=c("Platte","RioGrande","Gila"))


### Line graph
ggplot(sf.m, aes(Date,Streamflow, color=River)) + 
  geom_line(size=1)+theme_hc()+ theme(legend.position="none")+
  facet_wrap(~River, ncol=1)+ 
  theme(text=element_text(size=20), axis.text=element_text(size=15))+
  xlab("Date") + ylab("Streamflow (cfs)")+ 
  scale_color_viridis(option = "D",discrete=TRUE)+theme_clean()+
  theme(legend.position="none")


### Boxplot  (by decade add: facet_wrap(~decade, ncol=3) )
ggplot(sf.m, aes(x=River, y=Streamflow, fill=River)) +
  geom_boxplot() + 
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_clean() + theme(legend.position="none")+
  ggtitle("A simple boxplot") + xlab("")


### Ridge line plot
ggplot(sf.m, aes(x =Streamflow, y=River, fill="Streamflow")) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.0001) +
  theme_minimal() +  scale_fill_viridis(option = "E", discrete=TRUE, alpha=.4)+
  theme(legend.position="none")+ ylab("")+ggtitle("Streamflow Frequency 1934 to 2020")+
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())





