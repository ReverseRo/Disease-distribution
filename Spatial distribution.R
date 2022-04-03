require(rgdal)
require(RColorBrewer)
require(sp)
require(sf)
require(tmap)
require(stringr)

#### load the data file ####
file<- str_c(getwd(), "/data2018.RData")
load(file)
data$vision<-factor(data$vision)

#### calculate the prevalence by province ####
pre_byprov<-as.numeric()
a<-as.numeric()
b<-as.numeric()
for(i in as.numeric(names(table(data$prov)))  ){
  d<-data[data$prov==i,]
  population<-nrow(d)
  patients<-nrow(d[d$vision==1,])
  a<-append(a, patients)
  b<-append(b, population-patients)
  pre_byprov<-append(pre_byprov, patients/population*100)     
}

#### load the map file ####
file<- str_c(getwd(), "/China map file/China_prov.shp")
cnprov<-readOGR(file, use_iconv = TRUE, encoding = "UTF-8")
colnames(cnprov@data)<-c("provcode", "name", "type")
row.names(cnprov)<-as.character(cnprov$name)

#### merge the prevalence by province and map ####
prov<-c("北京市", "天津市", "河北省", "山西省", "辽宁省", "吉林省", "黑龙江省", "上海市", 
        "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "河南省", "湖北省", 
        "湖南省", "广东省", "广西壮族自治区", "海南省", "重庆市", "四川省", "陕西省")
data_prov<-data.frame(name=prov, vision=pre_byprov)

data_prov_all<-merge(cnprov@data, data_prov, by=c("name"), all.x=T, sort=F)
rownames(data_prov_all)<-data_prov_all$name
data_prov_sp<-SpatialPolygonsDataFrame(cnprov, data_prov_all)
data_prov_sf<-st_as_sf(data_prov_sp)

#### plot ####
colors <- colorRampPalette(c("#FFFFE0","#B22222"))(3) 
tiff(filename="Vision impairment prevalence by province in 2018.tiff", res=500, pointsize=12, 
     units="in", width=11.69, height=8.27)
tm_shape(data_prov_sf)+
  tm_polygons("vision", palette=colors, colorNA="white", textNA="Missing", alpha =1,
              breaks = c(20, 30, 40, 50, 60, 70), title="", id="name") +
  tm_legend(position=c("left", "bottom"), scale=2)+
  tm_borders()
dev.off()
