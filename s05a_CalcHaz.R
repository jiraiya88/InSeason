
calcHaz<-function(src_fpth,dateref,nproc)
{
require(data.table)
require(lubridate)
require(rgdal)
require(foreach)
require(doParallel)
require(stringr)

#####

print('   @@@ Starting track preparation and paralelel computing')
  
#Set model parameters
options(digits=10)
topo_1stcolumn<-13 # start column in model grid file where topo factors 



### ### ### ###

filetrack<-paste(src_fpth,sep='')
filegrid<-'./TK_PROC_Scripts/hazard/data/grid/Japan_Grid_with_topo&dirough.csv'

#####
# create folder to store output
# if(!dir.exists(paste('./PROC',str_sub(linp[m],6,12),sep='')))
# {
#   dir.create(paste('./PROC',str_sub(linp[m],6,12),sep=''))
# }

##### get track data #######
load(filetrack)
track_data<-outtr
rm(outtr)

names(track_data)[names(track_data)=='myID']<-'EventID'
names(track_data)[names(track_data)=='ISO_Time']<-'ISO_time'
names(track_data)[names(track_data)=='CP_hPa']<-'Pc_hPa'
names(track_data)[names(track_data)=='Vmax_kt']<-'Wind'
    
track_data<-track_data[,c("BatchID","EventID","Year","Month","Snap","Longitude","Latitude","Pc_hPa","Wind","Penv","TravelSpeed","TravelAngle","f","Fi1","B1","Fi2","B2","RMW1","RMW2","Vsr","vt","ut")]

###### Get model grid ######
wind_grid<-fread(filegrid)
#wind_grid<-setDF(wind_grid)
#in wind grid Municipality ID mentioned as CountyID
colnames(wind_grid)[8]<-"roughness"
colnames(wind_grid)[10]<-"Long"
wind_grid$Long[wind_grid$Long<0]<-wind_grid$Long[wind_grid$Long<0]+360
wind_grid$w10<-0
wind_grid$w10_topo<-0
wind_grid$Dir<-NA
wind_grid$w10ET<-0
wind_grid$w10ET_topo<-0
wind_grid$DirET<-NA

### intersect with buffer area
########################

if(!dir.exists(str_replace(str_replace(filetrack,'Tracks_',''),'\\.Rdata','')))
{
  dir.create(str_replace(str_replace(filetrack,'Tracks_',''),'\\.Rdata',''))
}

b<-readOGR('./TK_PROC_Scripts/hazard/data/shp/buff_4326.shp')
sp<-SpatialPoints(track_data[,c( "Longitude","Latitude")],proj4string=CRS(proj4string(b)))
# overlay with buffer around Japan (500km)
o<-data.frame(EventID=track_data$EventID,
              Snap=track_data$Snap,
              ind=as.numeric(unlist(over(sp,b))),
              seq=seq(1,length(track_data$EventID),1),stringsAsFactors = FALSE)
o<-o[!is.na(o$ind),]

if(length(o[,1])>0)
{
  # find first and last point which overlays
  o<-aggregate(o$seq,by=list(o$EventID),FUN=function(x) {cbind(MIN=min(x), MAX=max(x))})
  
  
# index to pull between first and last overlaying point
o<-apply(o$x,1,function(x) {seq(x[1],x[2],1)})
o<-sort(unlist(o))

track_data<-track_data[o,]
  x<-seq(1,length(track_data$EventID))
  z<-rle(track_data$EventID)
  if(length(z$values)==1)
  {
    z$values<-0
  
  } else {
    z$values<-c(0,cumsum(z$lengths)[1:(length(z$values)-1)])
  }
  z<-inverse.rle(z)
  track_data$Snap<-x-z
  rm(x,z)

# remove tracks less than 2 points
r<-aggregate(track_data$EventID,by=list(track_data$EventID),FUN=length)
track_data<-track_data[track_data$EventID %in% r$Group.1[r$x>1],]  

rm(o,b,sp,r)

########################

### Fast interpolation into '10 minutes'interval'
##################

if(length(track_data$BatchID)>0)
{
  # linear axis to interpolate from
  x<-seq(1,length(track_data$BatchID),1)
  # interpolate into 10 minute fractions of 6 hour steps
  xout<-seq(1,length(track_data$BatchID),1/6)
    # remove bits where Snap is decending but not equal 1
  out<-approx(x=x,y=track_data$Snap,xout=xout)$y
  xout<-xout[c(TRUE,out[2:length(out)]>out[1:(length(out)-1)]) | out==1]
  
  
  e<-unique(track_data[,c("BatchID","EventID","Year","Month")])
  n<-rle(track_data$EventID)
  n$lengths<-(n$lengths-1)*6+1
  y<-n
  y$values<-e$Year[match(y$values,e$EventID)]
  mm<-n
  mm$values<-e$Month[match(mm$values,e$EventID)]
  b<-n
  b$values<-e$BatchID[match(b$values,e$EventID)]
  
  # interpolated tracks
  out<-data.frame(BatchID=inverse.rle(b),
                  EventID=inverse.rle(n),
                  Year=inverse.rle(y),
                  Month=inverse.rle(mm),
                  Snap=approx(x=x,y=track_data$Snap,xout=xout)$y,
                  Longitude=approx(x=x,y=track_data$Longitude,xout=xout)$y,
                  Latitude=approx(x=x,y=track_data$Latitude,xout=xout)$y,
                  Pc_hPa=approx(x=x,y=track_data$Pc_hPa,xout=xout)$y,
                  Wind=approx(x=x,y=track_data$Wind,xout=xout)$y,
                  Penv=approx(x=x,y=track_data$Penv,xout=xout)$y,
                  TravelSpeed=approx(x=x,y=track_data$TravelSpeed,xout=xout)$y,
                  TravelAngle=approx(x=x,y=(track_data$TravelAngle+720),xout=xout)$y,
                  f=approx(x=x,y=track_data$f,xout=xout)$y,
                  Fi1=approx(x=x,y=track_data$Fi1,xout=xout)$y,
                  B1=approx(x=x,y=track_data$B1,xout=xout)$y,
                  Fi2=approx(x=x,y=track_data$Fi2,xout=xout)$y,
                  B2=approx(x=x,y=track_data$B2,xout=xout)$y,
                  RMW1=approx(x=x,y=track_data$RMW1,xout=xout)$y,
                  RMW2=approx(x=x,y=track_data$RMW2,xout=xout)$y,
                  Vsr=approx(x=x,y=track_data$Vsr,xout=xout)$y,
                  vt=approx(x=x,y=track_data$vt,xout=xout)$y,
                  ut=approx(x=x,y=track_data$ut,xout=xout)$y,
                  stringsAsFactors = FALSE)
  rm(n,mm,y,b,e,x,xout)
  out$TravelAngle<-out$TravelAngle-720
} else {
  out<-track_data
}

track_data<-out
rm(out)

##################

# last amendments before calculation
track_data$Longitude[track_data$Longitude<0]<-track_data$Longitude[track_data$Longitude<0]+360


####
# Parallelization

    if(length(track_data$BatchID))
    {
      # assign events to workers/processors
      leve<-unique(track_data[,c("BatchID","EventID")])
      leve<-leve[order(leve$BatchID,leve$EventID),]
      leve$Worker<-sort((seq(0,length(leve$BatchID)-1,1) %% nproc) + 1)
      
      print(paste('   @@@ Working on forecast time ',dateref,sep=''))
      # create folders if not exist
      
      for(i in seq(1,nproc,1))
      {
        if(!dir.exists(paste(str_replace(str_replace(filetrack,'Tracks_',''),'\\.Rdata',''),'/Worker',str_pad(i,pad=0,width=2),sep='')))
        {
          dir.create(paste(str_replace(str_replace(filetrack,'Tracks_',''),'\\.Rdata',''),'/Worker',str_pad(i,pad=0,width=2),sep=''))
        }
      }
  
      # start paralelization cluster
      cl<-makeCluster(nproc)
      registerDoParallel(cl)
      
      source('./TK_PROC_Scripts/s05b_trackProcessingFaster.R')
      s1<-Sys.time()
      foreach(i=1:nproc,.packages=c('stringr'),.export=c('windCalc','topo_1stcolumn')) %dopar% {
		  print('   @@@ foreach ')
           windCalc(profile=1,
                    track_data=track_data[paste(track_data$BatchID,track_data$EventID,sep='_') %in% paste(leve$BatchID[leve$Worker==i],leve$EventID[leve$Worker==i],sep='_'),],
                    wind_grid,
                    out_path=paste(str_replace(str_replace(filetrack,'Tracks_',''),'\\.Rdata',''),'/Worker',str_pad(i,pad=0,width=2),sep=''))
        }
		
      print(paste('   ... Finished in ',round(difftime(Sys.time(),s1,units="secs"),0),' seconds',sep=''))
      
      stopCluster(cl)
      rm(cl,s1)
    } else {
      print('    No relevant track for footprint generation found')
    }

}
print('    Finished track preparation and paralelel computing')

}
####

