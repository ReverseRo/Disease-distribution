require(rgdal)
require(RColorBrewer)
require(sp)
require(sf)
require(tmap)
require(stringr)
require(data.table)
require(dplyr)

#### load the map file ####
file<- str_c(getwd(), "/World map file/world_country.shp")
world<-readOGR(file, use_iconv = TRUE, encoding = "UTF-8")
row.names(world)<-as.character(world$Country)

#### download COVID-19 data from World Health Organization website ####
# https://covid19.who.int/WHO-COVID-19-global-data.csv; Access date:2022-03-29
file<- str_c(getwd(), "/WHO-COVID-19-global-data.csv")
covid<-read.csv(file)
colnames(covid)[1]<-"date"
covid$date<-as.Date(covid$date, format="%Y/%m/%d")
## only keep the latest data on cumulative cases
d<-arrange(covid, desc(date))
setDT(d);setkey(d, Country)
d[J(unique(Country)), mult = "first"] %>% setDF() -> cum_case
## rename some countries for merging with the map
cum_case[which(cum_case$Country_code=="CW"), "Country"]<-"Curacao"
cum_case[which(cum_case$Country_code=="CI"), "Country"]<-"Ivory Coast"
cum_case[which(cum_case$Country_code=="BL"), "Country"]<-"Saint Barthelemy"
cum_case<- cum_case[,c("Country", "Cumulative_cases")]
cum_case.2<-merge(world@data, cum_case, by=c("Country"), all.x=T, sort=F)
## since WHO donot provide data for some regions of the country (e.g., Hong Kong and Taiwan), we need to impute missing value using country-level data
cum_case.2[cum_case.2$Country %in% c("Hong Kong", "Macao", "Taiwan"),"Cumulative_cases"]<-
  cum_case.2[cum_case.2$Country %in% c("China"),"Cumulative_cases"]
cum_case.2[cum_case.2$Country %in% c("Heard I. and McDonald Is.", "Indian Ocean Ter.", 
                                     "Norfolk Island", "Coral Sea Is.", "Ashmore and Cartier Is."),"Cumulative_cases"]<-
  cum_case.2[cum_case.2$Country %in% c("Australia"),"Cumulative_cases"]
cum_case.2[cum_case.2$Country %in% c("Aland"),"Cumulative_cases"]<-
  cum_case.2[cum_case.2$Country %in% c("Finland"),"Cumulative_cases"]
cum_case.2[cum_case.2$Country %in% c("Fr. S. Antarctic Lands", "Clipperton I."),"Cumulative_cases"]<-
  cum_case.2[cum_case.2$Country %in% c("France"),"Cumulative_cases"]
cum_case.2[cum_case.2$Country %in% c("Dhekelia", "Akrotiri", "Br. Indian Ocean Ter.", "S. Geo. and the Is."),"Cumulative_cases"]<-
  cum_case.2[cum_case.2$Country %in% c("The United Kingdom"),"Cumulative_cases"]
cum_case.2[cum_case.2$Country %in% c("U.S. Minor Outlying Is."),"Cumulative_cases"]<-
  cum_case.2[cum_case.2$Country %in% c("United States of America"),"Cumulative_cases"]
rownames(cum_case.2)<-cum_case.2$Country

#### merge cumulative cases by country and map ####
world_sp<-SpatialPolygonsDataFrame(world, cum_case.2)
world_sf<-st_as_sf(world_sp)

#### plot ####
colors <- colorRampPalette(c("#FFFFE0","#B22222"))(3)
tiff(filename="Cumulative cases of COVID-19.tiff", res=500, pointsize=12, 
     units="in", width=11.69, height=8.27)
tm_shape(world_sf)+
  tm_polygons("Cumulative_cases",
              palette=colors,
              colorNA="white",
              breaks = c(0, 5001, 50001, 500001, 5000001, max(cum_case$Cumulative_cases) ),
              alpha = 1,
              title="COVID-19 \nCumulative Cases",
              legend.is.portrait=T,
              legend.reverse=T,
              id="Country") +
  tm_credits("Accessed Date: 2022-03-28", position=c(0.05, 0.5)) +
  tm_legend(position=c(0.05, 0.15), legend.outside=T, scale=1.3, panel.show=F) +
  tm_layout(attr.outside=T, attr.outside.position="bottom")
dev.off()