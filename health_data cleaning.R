library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
######(1)Maternal mortality ratio########
# read data
health <- read_csv("GII_HDR2020_040722.csv")
mmr <- health %>% select(country,region,contains("mmr"))
mmr1 <- pivot_longer(mmr,cols = mmr_1995:mmr_2017,
             names_to = "year", values_to = "mmr_value")

# split the column
mmr1$year = str_split_fixed(mmr1$year,"_",2)[,2]

#filter data in EAP region
EAP <- mmr1[mmr1$region=="EAP",]
EAP$year = as.Date(EAP$year, "%Y")
EAP <- na.omit(EAP)
#plot
plot_mmr=ggplot(data = EAP,) + 
  geom_line(mapping=aes(x=year,y=mmr_value,color=country))+
  theme(legend.position = "none")

ggplotly(plot_mmr)

######(2)Adolescent birth rate########
# read data
abr<- health %>% select(country,region,contains("abr"))
abr1 <- pivot_longer(abr,
                     c("abr_1995","abr_2000","abr_2005","abr_2010","abr_2011",	"abr_2012","abr_2013","abr_2014",
                       "abr_2015","abr_2016","abr_2017","abr_2018","abr_2019"),
                     names_to = "year", values_to = "abr_value")

# split the column
abr1$year = str_split_fixed(abr1$year,"_",2)[,2]
abr1$year = as.Date(abr1$year, "%Y")
#select data in EAP region
EAP_abr <- abr1[abr1$region=="EAP",]
EAP_abr <- na.omit(EAP_abr)
#plot
plot_abr=ggplot(data = EAP_abr,) + 
  geom_line(mapping=aes(x=year,y=abr_value,color=country))+
  theme(legend.position = "none")

ggplotly(plot_abr)

#####GII####
#data read
GII <- read_csv("GII_HDR2020_040722.csv")
GII <- GII %>%
  select(country,region,contains("gii"))

GII <- GII %>%
  pivot_longer(cols = gii_1995:gii_2019, names_to = "year",values_to = "gii_value")

GII$year = str_split_fixed(GII$year,"_",2)[,2]


GII <- na.omit(GII)

GII$year <- as.Date(GII$year, "%Y")
GII_EAP <- GII %>% filter(region=="EAP")
  

#plot
plot_GII=ggplot(data = GII %>% filter(region=="EAP"),aes(x=year,y=gii_value,color=country)) + 
  geom_line()+
  xlim(as.Date("1995-07-12"),as.Date("2020-07-12"))+
  #theme(legend.position = "none") +
  ggtitle("Explore GII in EAP area")
ggplotly(plot_GII)


plot_GII =plot_GII +
  geom_point(data = GII %>% filter(region=="EAP"),aes(x=year,y=gii_value),color="purple")


rank = GII %>%
  group_by(country,) %>%
  summarise(rank=mean(gii_rank_2019))
  
