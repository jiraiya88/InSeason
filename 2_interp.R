###generate the wind footprints
library(raster)
library(gstat)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
basepath<-'D:/03_JapanAER/IN_SEASON_ANALYSIS'
setwd(basepath)
obs<-read.csv('./3. Reformat_data/eventdata_AMEDAS.csv')
gid<-raster('./data/GID.tif')
r<-raster('./data/AverageTopographyFactor.tif')

evid<-unique(obs[,c("EventID","Name")])

for(i in seq(1,length(evid$EventID)))
{
      jb<-obs[obs$EventID==evid$EventID[i],]


      # convert data to spatial data frame
      spdf<-SpatialPointsDataFrame(coords = jb[,c("Longitude","Latitude")],
                                   data=jb,proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
      
      # reproject points to raster projection
      asp<-spTransform(spdf,proj4string(r))
      
      # pull topo factor from raster in station locations
      asp@data$TopoFac<-extract(r,asp)
      
      # confirm the same order of values in src data and spdf
      print(paste('Event: ',evid$EventID[i],'; is this 0?: ',sum(jb$EventID-asp@data$EventID),sep=''))
      
      # add topo factor to source data frame
      jb$TopoFac<-asp@data$TopoFac
      
      # create gust without topo factor
      asp@data$gr<-asp@data$Gust10_mps/asp@data$TopoFac
      
      # empty raster to hold the result
      g<-r
      values(g)<-NA
      
      # IDW
      # the power of IDW is idp
      # closest 8 stations are used for estimate
      gs <- gstat(formula=gr~1, locations=asp,nmax=8,set=list(idp = 1))
      g<-interpolate(r,gs)
      
      #g<-crop(g,extent(-7000000,-3000000,2000000,6000000))
      g<-g*r
      
      writeRaster(g,filename = paste('./Raster/AMEDAS',evid$EventID[i],'_gust.tif',sep = ''),overwrite=TRUE)
      
      d<-data.frame(GRIDID=values(gid),HAZARD=round(values(g),0))
      d<-d[!is.na(d$HAZARD),]
      d<-d[d$HAZARD>15,]
      
      write.csv(d,file = paste('./hazard/',evid$EventID[i],'.csv',sep=''),row.names = FALSE)
}

names(g)<-'gust'
grc<-projectRaster(g,crs="EPSG:4326")
polys2 = rasterToPolygons(grc)
raster::shapefile(polys2, paste('./Shapes/',evid$Name[i],'_',substr(evid$EventID[i],0,4),'_JMA_gust_footprint.shp',sep=''),overwrite=TRUE)
 
