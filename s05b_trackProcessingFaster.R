windCalc <- function(profile,track_data,wind_grid,out_path)
{

  library(geodist)
  cuthazL<-20
  cuthazH<-100
  
############################
# START : Wind simulation
############################

###### call wind simulation function #########
source("./TK_PROC_Scripts/s05c_WindModel.R")
  
evids<-unique(track_data$EventID)

for (j in 1:length(evids)){      #Loop1: over events

  etr<-track_data[track_data$EventID==evids[j],]
  
  #grid reduction based on event extent (allowing 7 degrees on each side)
  idx<-which(wind_grid$Long<(max(etr$Longitude)+7) & 
               wind_grid$Lat<(max(etr$Latitude)+7) & 
               wind_grid$Long>(min(etr$Longitude)-7) & 
               wind_grid$Lat>(min(etr$Latitude)-7))
  
  if (length(idx)>0)
  {
  #Initialization
  wind_grid1<-wind_grid[idx,]

  for(i in 1:length(etr$Latitude)){   #loop2 over all steps of one event

      s1<-Sys.time()
      print(paste("   @@@ Event: ",evids[j]," (",round(100*j/length(evids),2),"% of events); Step: ",i," (",round(100*i/length(etr$Latitude),2),"% of nodes)",sep=''))
      
      # only points within 500km
      wind_grid1$r<-c(geodist(cbind(lon=etr$Longitude[i],lat=etr$Latitude[i]),cbind(lon=wind_grid1$Long,lat=wind_grid1$Lat),measure="haversine"))
      idx<-which(wind_grid1$r<(500*1000))  #500 km threshould for wind computation
      
      #if there are any such points then ...
      s2<-0
      if(length(idx)>0){
        
        # get the wind field in given step
         s2<-Sys.time()
        wind<-wind_gen(etr$Longitude[i],
                       etr$Latitude[i],
                       etr$Pc_hPa[i]*100,
                       etr$RMW1[i],
                       etr$RMW2[i],
                       etr$Fi1[i],
                       etr$Fi2[i],
                       etr$B1[i],
                       etr$B2[i],
                       etr$TravelSpeed[i],
                       etr$TravelAngle[i],
                       wind_grid1$Long[idx],
                       wind_grid1$Lat[idx],
                       wind_grid1$roughness[idx],
                       wind_grid1$r[idx],
                       profile,
                       etr$vt[i],
                       etr$ut[i],
                       etr$Penv[i]*100)
        s2<-round(as.numeric(Sys.time()-s2),2)

        # apply topography factor
          #print("applying topo factor")
          wind$Dir[which(wind$Dir<0)]<-360+wind$Dir[which(wind$Dir<0)]   #in case the wind dir -180 to 180 need to change in 360 deg format as the angle will be used to select the column for topography factor from grid file
          sector<-trunc((wind$Dir/30)+0.5)   # rounding has issue in R hence this work-around
          sector[sector>11]<-sector[sector>11]-12  # directions >345
          #index<-cbind(seq(1,length(sector),1),(sector+topo_1stcolumn))  #create index to get factors from grid table
          w10_topo<-wind$w10*as.matrix(wind_grid1)[cbind(idx,sector+topo_1stcolumn)]   # wind with topo effect


          ### temporary comment
          wind$DirET[which(wind$DirET<0)]<-360+wind$DirET[which(wind$DirET<0)]   #in case the wind dir -180 to 180 need to change in 360 deg format as the angle will be used to select the column for topography factor from grid file
          sector<-trunc((wind$DirET/30)+0.5)   # rounding has issue in R hence this work-around
          sector[sector>11]<-sector[sector>11]-12  # directions >345
          #index<-cbind(seq(1,length(sector),1),(sector+topo_1stcolumn))  #create index to get factors from grid table
          w10ET_topo<-wind$w10ET*as.matrix(wind_grid1)[cbind(idx,sector+topo_1stcolumn)]   # wind with topo effect

        # keep the maximum value
        wind_grid1$w10[idx]<-pmax(wind_grid1$w10[idx],wind$w10,na.rm = T)
        wind_grid1$Dir[idx]<-pmax(wind_grid1$Dir[idx],wind$Dir,na.rm = T)
        wind_grid1$w10ET[idx]<-pmax(wind_grid1$w10ET[idx],wind$w10ET,na.rm = T)
        wind_grid1$DirET[idx]<-pmax(wind_grid1$DirET[idx],wind$DirET,na.rm = T)
        wind_grid1$w10_topo[idx]<-pmax(wind_grid1$w10_topo[idx],w10_topo,na.rm = T)
        wind_grid1$w10ET_topo[idx]<-pmax(wind_grid1$w10ET_topo[idx],w10ET_topo,na.rm = T)

        rm(wind,w10_topo,w10ET_topo,sector)
		

      } else {print("   ... all points beyond 500km")}
      
      s1<-round(as.numeric(Sys.time()-s1),2)
      print(paste('   ... time elapsed ',s1,' seconds (',100*round(s2/s1,3),'% on wind field calculation)',sep=''))
  }  #loop2 over all steps of one event
  
  print(paste("   @@@ Event: ",evids[j]," ... WRITING .... ",sep=''))
  
  wind_grid2<-wind_grid1[round(wind_grid1$w10ET_topo,0)>=cuthazL,]
  wind_grid2$w10ET_topo<-round(wind_grid2$w10ET_topo,0)
  wind_grid2$w10ET_topo[wind_grid2$w10ET_topo>cuthazH]<-cuthazH

  #-- writing oasis output
  if(length(wind_grid2$w10ET_topo)>0)
  {
    oa<-data.frame(Event_ID=evids[j],
                   AREAPERIL_ID=wind_grid2$ID*100000+10392,
                   INTENSITY_BIN_INDEX=wind_grid2$w10ET_topo,
                   PROB=1,
                   stringsAsFactors = FALSE)
    
    haz_file_out<-paste0(out_path,"/",str_pad(evids[j],pad=0,width = 7),"_EventFootprint_Grid_Peril_1.csv",sep="")
    
    write.csv(oa,file = haz_file_out,quote = FALSE,row.names = FALSE)
    
  }

  rm(wind_grid1,wind_grid2)
  }
  
  print(paste("   @@@ Event: ",evids[j]," ... COMPLETED .... ",sep=''))
} #Loop1: over events end

######################### END ##########################################
}

