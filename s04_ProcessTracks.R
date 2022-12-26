
TrackProcess<-function(label,fdate,storm)
{
  require(stringr)
  require(raster)
  require(lubridate)
  require(geodist)
  require(geosphere)
  #library(rvest)
  library(data.table)
  
  source('./TK_PROC_Scripts/hazard/Functions/_fun.R')
  
  nomp<-1013 # nominal environmental pressure to be assigned when outside of the range of data
  
  lst<-list.files(path = './TK_PROC_Scripts/TRACKS/Combined/',pattern = paste('Season',str_sub(label,1,4),'_CombinedTracks_',storm,'_',label,'\\.csv',sep=''))
  
  for (k in seq(1,length(lst),1))
  {
  # load the file
  otr<-fread(paste('./TK_PROC_Scripts/TRACKS/Combined/',lst[k],sep=''))
  otr$DateTimeUTC<-as.POSIXct(otr$DateTimeUTC,format="%Y-%m-%dT%H:%M:%SZ",tz='UTC')
  snam<-otr$StormName[1]

  otr$vmax<-otr$vmax/0.51444
  names(otr)<-c("Perspective","Source","StormName","Number","ISO_time","Number","Longitude","Latitude","CP","Vmax_kt","EventInfo_EventName","EventID")
  setorderv(otr,cols=c("EventInfo_EventName","ISO_time"))

  rtr<-NULL
  for(evnam in unique(otr$StormName))
  {
        # evnam<-unique(otr$StormName)
       tr<-otr[otr$StormName==evnam,]      
    
        ###!!! REMOVE THIS AS SOON AS YOU CAN
        # maniulating LatLon coordinates
        # tr$Latitude<-tr$Latitude+10     # sending storm to Northern Hemisphere
        #tr$Longitude<-tr$Longitude+85     # shifting storm to the east
        # maniulating LatLon coordinates
        ###!!! REMOVE THIS AS SOON AS YOU CAN
        
        tr$Month<-month(tr$ISO_time[which.min(tr$ISO_time)])
        if(length(tr$ISO_time)>0)
        {
          
          # assign Penv (long term monthly average: 1951 - 2019 )
          p<-stack('./TK_PROC_Scripts/hazard/data/SurfPress/MSLP_NCEP1.25deg_1951-2019_MEAN.tif')
          parr<-aperm(as.array(p),c(2,1,3))
          parr<-apply(parr,3,c)
          
          # interpolate Penv from long term monthly means (average of 500km to 800km radius)
          tr$Penv<-NA
          for(i in seq(1,length(tr$Perspective)))
            # assign Penv to forecast 
          {
            d<-geodist(tr[i,c("Longitude","Latitude")],coordinates(p),measure='haversine')
            d<-d>=500000 & d<=800000
            
            if(sum(d)>0)  
            {
                if(day(tr$ISO_time[i])>=15)
                {
                  deltat<-as.numeric(as.POSIXct(paste(year(tr$ISO_time[i])+as.numeric(month(tr$ISO_time[i])==12),'-',(month(tr$ISO_time[i]) %% 12)+1,'-15 00:00:00',sep=''),tz='UTC')-
                                       as.POSIXct(paste(year(tr$ISO_time[i]),'-',month(tr$ISO_time[i]),'-15 00:00:00',sep=''),tz='UTC'))
                  
                  a<-as.numeric(tr$ISO_time[i]-as.POSIXct(paste(year(tr$ISO_time[i]),'-',month(tr$ISO_time[i]),'-15 00:00:00',sep=''),tz='UTC'))/deltat
                  #tr$Penv[i]<-(1-a)*mean(values(p[[tr$Month[i]]])[d])+a*mean(values(p[[(tr$Month[i] %% 12) + 1]])[d])
                  tr$Penv[i]<-(1-a)*mean(parr[d,tr$Month[i]])+a*mean(parr[d,(tr$Month[i] %% 12) + 1])
                } else {
                  deltat<-as.numeric(as.POSIXct(paste(year(tr$ISO_time[i]),'-',month(tr$ISO_time[i]),'-15 00:00:00',sep=''),tz='UTC')-
                                     as.POSIXct(paste(year(tr$ISO_time[i])-as.numeric(month(tr$ISO_time[i])==1),'-',month(tr$ISO_time[i])-1+as.numeric(month(tr$ISO_time[i])==1)*12,'-15 00:00:00',sep=''),tz='UTC'))
                
                a<-as.numeric(as.POSIXct(paste(year(tr$ISO_time[i]),'-',month(tr$ISO_time[i]),'-15 00:00:00',sep=''),tz='UTC')-tr$ISO_time[i])/deltat
                #tr$Penv[i]<-a*mean(values(p[[tr$Month[i]-1+as.numeric(month(tr$ISO_time[i])==1)*12]])[d])+(1-a)*mean(values(p[[tr$Month[i]]])[d])
                tr$Penv[i]<-a*mean(parr[d,tr$Month[i]-1+as.numeric(month(tr$ISO_time[i])==1)*12])+(1-a)*mean(parr[d,tr$Month[i]])
                
                }
            } else {
              tr$Penv[i]<-nomp
            }
          }
          rm(i,parr,p,a,d,deltat)
    
          # assign travel speed
          tr$TravelSpeed<-NA
          setorderv(tr,cols=c("EventInfo_EventName","ISO_time"))
          
          dist<-c(0,geodist(cbind(lon=tr$Longitude[2:length(tr$ISO_time)],lat=tr$Latitude[2:length(tr$ISO_time)]),
                            cbind(lon=tr$Longitude[1:(length(tr$ISO_time)-1)],lat=tr$Latitude[1:(length(tr$ISO_time)-1)]),paired=TRUE,measure='haversine'))
          x<-c(1,which(tr$EventInfo_EventName[2:length(tr$ISO_time)]!=tr$EventInfo_EventName[1:(length(tr$ISO_time)-1)])+1)
          dist[x]<-dist[x+1]
          dt<-c(0,tr$ISO_time[2:length(tr$ISO_time)]-tr$ISO_time[1:(length(tr$ISO_time)-1)])
          dt[x]<-dt[x+1]
          tr$TravelSpeed<-dist/(dt*3600)
          
          tr$TravelAngle<-c(bearing(cbind(tr$Longitude[1:(length(tr$ISO_time)-1)],tr$Latitude[1:(length(tr$ISO_time)-1)]),
                                    cbind(tr$Longitude[2:length(tr$ISO_time)],tr$Latitude[2:length(tr$ISO_time)])),0)
          x<-c(which(tr$EventInfo_EventName[2:length(tr$ISO_time)]!=tr$EventInfo_EventName[1:(length(tr$ISO_time)-1)]),length(tr$ISO_time))
          tr$TravelAngle[x]<-tr$TravelAngle[x-1]
          rm(x,dist,dt)
          
          # add Vmak_kt
          # regression from 1,782 events during 1951-2018
          c0<-0.9301528   
          c1<-0.5493580 
          tr$Vmax_kt<-10^(c0+c1*log10(pmax(0.1,1010-tr$CP)))
          rm(c0,c1)
          
          tr$Penv <- tr$Penv + 4 + 5.09600593 - 0.16112896*tr$Latitude -0.07733656*tr$Vmax_kt*0.51444 -0.18275613*tr$TravelSpeed
          tr$Penv[is.na(tr$Penv)]<-1012
          tr$Penv[tr$Penv<tr$CP_hPa]<-tr$CP_hPa[tr$Penv<tr$CP_hPa]+0.5
          
          # interpolation into 1 hour step
          eid<-sort(unique(tr$EventInfo_EventName))
          ktr<-tr[0,c('EventID','EventInfo_EventName','Longitude','Latitude','CP','ISO_time','Penv','Perspective',
                      'TravelSpeed','TravelAngle','Vmax_kt')]
          for(i in eid)
          {
            etr<-tr[tr$EventInfo_EventName==i,]
            etr<-etr[,c('EventID','EventInfo_EventName','Longitude','Latitude','CP','ISO_time','Penv','Perspective',
                        'TravelSpeed','TravelAngle','Vmax_kt')]
            x<-difftime(etr$ISO_time,etr$ISO_time[1],units='hours')
            xout<-difftime(seq(min(etr$ISO_time),max(etr$ISO_time),60*60),etr$ISO_time[1],units='hours')
            if(length(x)==1 && x==0)
            {
              ktr<-rbind(ktr,etr)
            } else {
              ktr<-rbind(ktr,data.table(EventID=etr$EventID[1],
                                        EventInfo_EventName=etr$EventInfo_EventName[1],
                                        Longitude=approx(x,etr$Longitude,xout)$y,
                                        Latitude=approx(x,etr$Latitude,xout)$y,
                                        CP=approx(x,etr$CP,xout)$y,
                                        ISO_time=xout+etr$ISO_time[1],
                                        Penv=approx(x,etr$Penv,xout)$y,
                                        Perspective=etr$Perspective[length(etr$ISO_time)],
                                        TravelSpeed=approx(x,etr$TravelSpeed,xout)$y,
                                        TravelAngle=(approx(x,etr$TravelAngle+720,xout)$y %% 360) - 360,
                                        Vmax_kt=approx(x,etr$Vmax_kt,xout)$y))
            }
          }
          tr<-ktr
          rm(ktr,eid,x,xout,i,etr)
          
          # add snap
          setorderv(tr,cols = c("EventInfo_EventName","ISO_time"))
          tr$Snap<-NA
          tr$Snap[1]<-1
          for(i in seq(2,length(tr$EventInfo_EventName),1))
          {
            if(tr$EventInfo_EventName[i]==tr$EventInfo_EventName[i-1]) 
            {tr$Snap[i]<-tr$Snap[i-1]+1} 
            else {
              tr$Snap[i]<-1
            }
          }
	      rm(i)
          
          # start adding parameters
          outtr<-data.table(EventID=tr$EventID,
                            EventName=tr$EventInfo_EventName,
                            Snap=tr$Snap,
                            Year=year(tr$ISO_time),
                            Month=month(tr$ISO_time),
                            Longitude=tr$Longitude,
                            Latitude=tr$Latitude,
                            CP_hPa=tr$CP,
                            Type=tr$Perspective,
                            Penv=tr$Penv,
                            TravelSpeed=tr$TravelSpeed,
                            TravelAngle=tr$TravelAngle,
                            f=2*7.29*(10^(-5))*sin(tr$Latitude*pi/180),
                            Vmax_kt=tr$Vmax_kt,
                            Fi1=NA,
                            B1=NA,
                            Fi2=NA,
                            B2=NA,
                            RMW1=NA,
                            RMW2=NA,
                            BatchID=1,
                            ISO_time=tr$ISO_time)
          
          ### Add parameters
          #############################################
          
          outtr<-setDF(outtr)
          load('./TK_PROC_Scripts/hazard/Regression/RMW1_VickeryReg_below980hPa.Rdata')
          minR1<-5000 # minimum allowed R1
          WPBL<-0.8
          
          fac<-1.5 # multiple of standard deviation to be included
          ernddev<-unique(outtr[,c("BatchID","EventName")])
          ernddev$devi<-10
          while(sum(abs(ernddev$devi)-fac*sd(r1fit$residuals)>0)>0)
          {
            idx<-abs(ernddev$devi)-fac*sd(r1fit$residuals)>0
            ernddev$devi[idx]<-rnorm(sum(idx),mean=0,sd=sd(r1fit$residuals))
          }
          save(ernddev,file='./TK_PROC_Scripts/hazard/Regression/StochEvent_rndDeviationRmax1.Rdata')
          nums<-rle(outtr$EventName)
          ernddev$LENGTH<-nums$lengths[match(ernddev$EventName,nums$values)]
          nums$values<-ernddev$devi
          nums$lengths<-ernddev$LENGTH
          
          # find Vmax of stationary vortex
          outtr$Vsr<-getVm(outtr$Vmax_kt*0.51444*1,outtr$TravelSpeed,method=1)
          
          # Regressed Rmax estimate + RMW2 estimate
          
          # codeRMW1 calculated with randomly sampled Rmax1 value
          # outtr$RMW1<-pmax(minR1,exp(c(inverse.rle(nums))+coefficients(r1fit)[1]+
          #                              coefficients(r1fit)[2]*(outtr$Penv - outtr$CP_hPa)^0.5+
          #                              coefficients(r1fit)[3]*pmin(2.0,1.15*exp(1)*((outtr$Vsr/WPBL)^2)/(outtr$Penv*100-outtr$CP_hPa*100))))
          outtr$RMW1<-pmax(minR1,exp(coefficients(r1fit)[1]+
                                       coefficients(r1fit)[2]*(outtr$Penv - outtr$CP_hPa)^0.5+
                                       coefficients(r1fit)[3]*pmin(2.0,1.15*exp(1)*((outtr$Vsr/WPBL)^2)/(outtr$Penv*100-outtr$CP_hPa*100))))
          outtr$RMW2<-pmax(outtr$RMW1,outtr$RMW1*getR2_R1(outtr$CP_hPa,outtr$TravelSpeed,outtr$Latitude))
          
          dp<-100*(outtr$Penv-outtr$CP_hPa)
          dp2<-getDP2_hPa(outtr$Penv,outtr$CP_hPa,outtr$Latitude)*100
          dp1<-pmax(0,dp-dp2)
          
          D<-1
          outtr$Fi1<-1
          outtr$Fi2<-outtr$Fi1
          outtr$B1<-pmax(0.5,pmin(2.5,((1.15*exp(1)*(outtr$Vsr/WPBL)^2)/(dp))*(1+outtr$f*outtr$RMW1/(outtr$Vsr/WPBL))*exp(outtr$Fi1-1)/(outtr$Fi1*D)))
          outtr$B2<-outtr$B1
          
          c<-1
          while(c<150) # iteration to precision of 10cm to be sure other parameters (Fi,B) converged too
          {
            D<-dp1/dp + dp2*((outtr$RMW2/outtr$RMW1)^outtr$B2)*exp((-1)*outtr$Fi1*(((outtr$RMW2/outtr$RMW1)^outtr$B2)-1))/dp
            outtr$Fi1<-(1+(outtr$f*outtr$RMW1/(outtr$Vsr/WPBL))/(outtr$B1*(1+outtr$f*outtr$RMW1/(outtr$Vsr/WPBL))))/(dp1/(dp*D)+((outtr$RMW2/outtr$RMW1)^outtr$B2)*(1-dp1/(dp*D)))
            outtr$Fi2<-outtr$Fi1
            
            outtr$B1<-pmax(0.5,pmin(2.5,((1.15*exp(1)*(outtr$Vsr/WPBL)^2)/dp)*(1+outtr$f*outtr$RMW1/(outtr$Vsr/WPBL))*exp(outtr$Fi1-1)/(outtr$Fi1*D)))
            outtr$B2<-outtr$B1
            
            
            c<-c+1
          }
          rm(c,D,dp,dp1,dp2,WPBL,minR1)
          
          ###################
          ###################
          
          outtr<-outtr[order(outtr$EventName,outtr$Snap),]
          
          idx<-outtr$CP_hPa<=1005
          
          # figure out first and last position of CP less or equal 1000hPa
          mmin<-aggregate(seq(1,length(outtr$EventName),1)[idx],by=list(outtr$EventName[idx]),FUN=min)
          mmax<-aggregate(seq(1,length(outtr$EventName),1)[idx],by=list(outtr$EventName[idx]),FUN=max)
          
          if(sum(mmin$Group.1==mmax$Group.1)==length(mmax$Group.1))
          {
            scoop<-data.frame(EventName=mmin$Group.1,
                              from=mmin$x,
                              to=mmax$x,
                              stringsAsFactors = FALSE)
          }
          # create indices
          idx<-sort(unlist(mapply(seq, scoop$from,scoop$to)))
          
          # apply index on track
          outtr<-outtr[idx,]
          
          # remove tracks shorter than 3 steps
          llen<-aggregate(outtr$EventName,by=list(outtr$EventName),FUN=length)
          
          outtr<-outtr[outtr$EventName %in% llen$Group.1[llen$x>2],]
          outtr<-outtr[order(outtr$EventName,outtr$Snap),]
          rm(idx,scoop,mmax,mmin,llen)
          
          outtr$Fi1[is.na(outtr$Fi1)]<-1
          outtr$B1[is.na(outtr$B1)]<-1
          outtr$Fi2[is.na(outtr$Fi2)]<-1
          outtr$B2[is.na(outtr$B2)]<-1
          
          outtr$vt<-0
          outtr$ut<-0

          rtr<-rbind(rtr,outtr)
        }
  }
  
  outtr<-rtr
  rm(rtr)
  # Assign EventID
  # e<-data.table(EventID=seq(1,length(unique(otr$EventInfo_EventName))),EventInfo_EventName=sort(unique(otr$EventInfo_EventName)))
  e<-unique(outtr[,c("EventID","EventName")])
  # outtr$EventID<-e$EventID[match(outtr$EventName,e$EventInfo_EventName)]
  outtr<-outtr[,c("EventID","BatchID","ISO_time","EventID","Year","Month","Snap","Longitude","Latitude","CP_hPa","Vmax_kt","Penv","TravelSpeed","TravelAngle","f","Fi1","B1","Fi2","B2","RMW1","RMW2","Vsr","vt","ut")]
  # create folder specific for forecast time (can contain more events than one)
  if (!dir.exists(paste('./TK_PROC_Scripts/hazard/OasisTables/',snam,'_',label,sep='')))
    {
      dir.create(paste('./TK_PROC_Scripts/hazard/OasisTables/',snam,'_',label,sep=''))
    }
  s<-otr[,.(Year=year(min(ISO_time)),Month=month(min(ISO_time)),Day=day(min(ISO_time))),by=list(EventInfo_EventName)]
  einfo<-data.table(EVENT_ID=e$EventID,
                    EVENT_NAME=e$EventName,
                    EVENT_FREQ=0,
                    OCC_YEAR_FREQ=0,
                    OCC_YEAR=s$Year[match(e$EventName,s$EventInfo_EventName)],
                    OCC_MONTH=s$Month[match(e$EventName,s$EventInfo_EventName)],
                    OCC_DAY=s$Day[match(e$EventName,s$EventInfo_EventName)],
                    EVENT_DURATION=0,
                    DEMAND_SURGE=1)
  fwrite(einfo,file=paste('./TK_PROC_Scripts/hazard/OasisTables/',snam,'_',label,'/EventInfo.csv',sep=''))
  rm(einfo,e,s)
  
  save(outtr,file=paste('./TK_PROC_Scripts/TRACKS/Ready/Tracks_',snam,'_',label,'.Rdata',sep=''))
  }
}

