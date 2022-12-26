
TrackCombine<-function(dsh,fdate,YYYY)
{
library(data.table)
library(stringr)
library(lubridate)
    
TrfillBlank<-function(track)
{
    if(length(track$lat)>0)
    {
        # forecast only allows one missing location, if there is more consecutive missing locations of a storm it will be cut-off
        setorderv(track,cols=c('StormName','Number','DateTimeUTC'))
        eid<-unique(track[,c('StormName','Number')])
        
        # convert to non-negative format of longitude
        track$lon[track$lon<0 & !is.na(track$lon)]<-360+track$lon[track$lon<0 & !is.na(track$lon)]
        
        TrfillBlank<-NULL
        for(i in seq(1,length(eid$StormName),1))
        {
            dat<-track[StormName==eid$StormName[i] & Number==eid$Number[i],]
            idx<-sort(unique(c(which(is.na(dat$lat)),which(is.na(dat$lon)))))
            #idx<-sort(unique(c(which(is.na(dat$lat[1:15])),which(is.na(dat$lon[1:15])))))
            if(length(idx)==0)
            {
                # no need for fixing NA
                TrfillBlank<-rbind(TrfillBlank,dat)
            } else {
                if(length(idx)==1)
                {
                    if(idx==length(dat$lat))
                    {
                        # when there is one missing position value at the very end of track then cut off
                        TrfillBlank<-rbind(TrfillBlank,dat[1:(length(dat$lat)-1),])
                    } else {
                        # when there is one missing position value then interpolate
                        TrfillBlank<-rbind(TrfillBlank,data.table(basin=dat$basin[1],
                                                                  TC_num=dat$TC_num[1],
                                                                  DateTimeUTC=dat$DateTimeUTC,
                                                                  StormName=dat$StormName[1],
                                                                  tech=dat$tech[1],
                                                                  lat=approx(dat$DateTimeUTC[-idx],dat$lat[-idx],xout=dat$DateTimeUTC)$y,
                                                                  lon=approx(dat$DateTimeUTC[-idx],dat$lon[-idx],xout=dat$DateTimeUTC)$y,
                                                                  vmax=dat$vmax,
                                                                  mslp=dat$mslp,
                                                                  Source=dat$Source[1],
                                                                  Perspective=dat$Perspective[1],
                                                                  Number=dat$Number[1]))
                    }

                } else {
                    cut<-idx[which(idx[2:length(idx)]-1==idx[1:(length(idx)-1)])]
                    if(length(cut)==0)
                    {
                        # when there is more than one missing position value, but never in two consecutive positions then interpolate
                        TrfillBlank<-rbind(TrfillBlank,data.table(basin=dat$basin[1],
                                                                  TC_num=dat$TC_num[1],
                                                                  DateTimeUTC=dat$DateTimeUTC,
                                                                  StormName=dat$StormName[1],
                                                                  tech=dat$tech[1],
                                                                  lat=approx(dat$DateTimeUTC[-idx],dat$lat[-idx],xout=dat$DateTimeUTC)$y,
                                                                  lon=approx(dat$DateTimeUTC[-idx],dat$lon[-idx],xout=dat$DateTimeUTC)$y,
                                                                  vmax=dat$vmax,
                                                                  mslp=dat$mslp,                                                                  Source=dat$Source[1],
                                                                  Perspective=dat$Perspective[1],
                                                                  Number=dat$Number[1]))
                    } else {
                        cut<-min(cut)-1
                        # when there is more than one missing position value and consecutive at least once,
                        # then cut-off first then intterpolate the rest
                        dat<-dat[1:cut]
                        idx<-sort(unique(c(which(is.na(dat$lat)),which(is.na(dat$lon)))))
                        if(length(idx)==0)
                        {
                            TrfillBlank<-rbind(TrfillBlank,dat)
                        } else {
                            TrfillBlank<-rbind(TrfillBlank,data.table(basin=dat$basin[1],
                                                                      TC_num=dat$TC_num[1],
                                                                      DateTimeUTC=dat$DateTimeUTC,
                                                                      StormName=dat$StormName[1],
                                                                      tech=dat$tech[1],
                                                                      lat=approx(dat$DateTimeUTC[-idx],dat$lat[-idx],xout=dat$DateTimeUTC)$y,
                                                                      lon=approx(dat$DateTimeUTC[-idx],dat$lon[-idx],xout=dat$DateTimeUTC)$y,
                                                                      vmax=dat$vmax,
                                                                      mslp=dat$mslp,                                                                      Source=dat$Source[1],
                                                                      Perspective=dat$Perspective[1],
                                                                      Number=dat$Number[1]))
                        }
                    }
                }
            }
        }
        TrfillBlank$lon[TrfillBlank$lon>180]<-TrfillBlank$lon[TrfillBlank$lon>180]-360
        
    } else {
        TrfillBlank<-track
    }
    return(TrfillBlank)
}

dsh<-dsh[dsh$Active>0,]

    if(length(dsh$Basin)>0)   # proceed only if there are  active TCs
    {
        TCname<-dsh$StormName[dsh$Active==1]  # names of active storms
        
        # extract active storms from JTWC data
        jtwc<-fread(paste('./TK_PROC_Scripts/TRACKS/JTWC/Season',YYYY,'_ObservedTracks_JTWC_',label,'.csv',sep=''))
        jtwc<-jtwc[jtwc$StormName %in% TCname,]
        jtwc$DateTimeUTC<-as.POSIXct(jtwc$DateTimeUTC,format="%Y-%m-%dT%H:%M:%SZ",tz='UTC')
        jtwc<-jtwc[jtwc$DateTimeUTC<=fdate,]
        # as it happens, sometimes for one date there is multiple records, Let's take and average of such cases
        jtwc<-jtwc[,.(lat=round(mean(Latitude,na.rm=TRUE),1),
                      lon=round(mean(Longitude,na.rm=TRUE),1),
                      vmax=round(0.88*mean(Vmax/0.514444,na.rm=TRUE),2),    # convert m/s from knots and convert to 10-minute average from 1-minute average by factor 0.88
                      mslp=round(mean(Cp,na.rm=TRUE),1)),
                   by=list(Basin,StormNumber,DateTimeUTC,Source,StormName,Agency,StormNameOther)]
        jtwc$Source<-'JTWC'
        jtwc$Perspective<-'OPR'
        jtwc$Number<-1
        
        # extract active storms from HRES (ECMWF) data
        # the name of the file starts with ECMWF_HRES later there is the name of the storm in the name and even later there is forecast time
        hres<-NULL
        for(i in seq(1,length(TCname),1))
        {
          lst<-list.files(path='./TK_PROC_Scripts/TRACKS/ECMWF/',pattern=paste('^ECMWF_HRES_',TCname[i],'.*',label,'.*\\.csv$',sep=''))
          if(length(lst)>0)
          {
            tmp<-fread(paste('./TK_PROC_Scripts/TRACKS/ECMWF/',lst,sep=''))
            tmp$StormName<-TCname[i]    
            hres<-rbind(hres,tmp)
          }
        }
        rm(lst,tmp,i)
        # make sure format of data is right
        hres$lat<-as.double(hres$lat)
        hres$lon<-as.double(hres$lon)
        hres$mslp<-as.double(hres$mslp)
        hres$vmax<-as.double(hres$vmax)
        
        if(!is.null(hres))
        {
            hres$DateTimeUTC<-as.POSIXct(paste(str_sub(hres$forecast_date,1,8),str_sub(hres$forecast_date,9,10),sep=' '),format="%Y%m%d %H",tz='UTC')+hres$forecast_hours*3600
            # the format of the track reports wind radii in next line which can make mulitple rows with same date
            hres<-hres[,.(lat=round(mean(lat,na.rm=TRUE),1),
                          lon=round(mean(lon,na.rm=TRUE),1),
                          vmax=round(mean(vmax,na.rm=TRUE),2),    
                          mslp=round(mean(mslp,na.rm=TRUE),1)),
                       by=list(basin,TC_num,DateTimeUTC,StormName)]
            hres$Source<-'ECMWF'
            hres$Perspective<-'HRES'
            hres$Number<-51
        } else 
        {
            hres<-data.table(basin=as.character("A"),
                             TC_num=as.integer(1),
                             DateTimeUTC=as.POSIXct(Sys.time(),tz='UTC'),
                             StormName=as.character("A"),
                             lat=as.double(1.5),
                             lon=as.double(1.5),
                             vmax=as.double(1.5),
                             mslp=as.double(1.5),
                             Source=as.character("A"),
                             Perspective=as.character("A"),
                             Number=as.integer(1))
            hres<-hres[0,]
        }
        # correct missing track positions if any (assumes that one missing location is allowed and interpolated, if there is two consecutive, track is cut-off)
        hres<-TrfillBlank(hres)
        
        # extract active storms from ENS (ECMWF) data
        # the name of the file starts with ECMWF_HRES later there is the name of the storm in the name and even later there is forecast time
        ens<-NULL
        for(i in seq(1,length(TCname),1))
        {
          lst<-list.files(path='./TK_PROC_Scripts/TRACKS/ECMWF/',pattern=paste('^ECMWF_ENS_',TCname[i],'.*',label,'.*\\.csv$',sep=''))
          if(length(lst)>0)
          {
            tmp<-fread(paste('./TK_PROC_Scripts/TRACKS/ECMWF/',lst,sep=''))
            tmp$StormName<-TCname[i]    
            ens<-rbind(ens,tmp)

          }
        }
        # make sure format of data is right
        ens$lat<-as.double(ens$lat)
        ens$lon<-as.double(ens$lon)
        ens$mslp<-as.double(ens$mslp)
        ens$vmax<-as.double(ens$vmax)

        rm(lst,tmp,i)
        if(!is.null(ens))
        {
            ens$DateTimeUTC<-as.POSIXct(paste(str_sub(ens$forecast_date,1,8),str_sub(ens$forecast_date,9,10),sep=' '),format="%Y%m%d %H",tz='UTC')+ens$forecast_hours*3600
            # the format of the track reports wind radii in next line which can make mulitple rows with same date
            ens<-ens[,.(lat=round(mean(lat,na.rm=TRUE),1),
                        lon=round(mean(lon,na.rm=TRUE),1),
                        vmax=round(mean(vmax,na.rm=TRUE),2),    
                        mslp=round(mean(mslp,na.rm=TRUE),1)),
                     by=list(basin,TC_num,DateTimeUTC,StormName,tech)]
            ens$Source<-'ECMWF'
            ens$Perspective<-'ENS'
            ens$Number<-as.integer(str_replace(ens$tech,'ENS',''))
            
        } else 
        {
            ens<-data.table(basin=as.character("A"),
                             TC_num=as.integer(1),
                             DateTimeUTC=as.POSIXct(Sys.time(),tz='UTC'),
                             StormName=as.character("A"),
                             lat=as.double(1.5),
                             lon=as.double(1.5),
                             vmax=as.double(1.5),
                             mslp=as.double(1.5),
                             Source=as.character("A"),
                             Perspective=as.character("A"),
                             Number=as.integer(1))
            ens<-ens[0,]
        }
        ens<-TrfillBlank(ens)
        
        tr<-rbind(hres[,c('Perspective','Source','StormName','Number','DateTimeUTC','Number','lon','lat','mslp','vmax')],
                  ens[,c('Perspective','Source','StormName','Number','DateTimeUTC','Number','lon','lat','mslp','vmax')])
        tr$Number<-str_pad(tr$Number,pad=0,width=2)
        tr$EventInfo_EventName<-paste(tr$Perspective,tr$Source,tr$StormName,tr$Number,sep = '_')
        u<-sort(unique(tr$EventInfo_EventName))
    
        # add to each forecast observed part of the curve
        add<-NULL

        # observed part for 50 ensambles and one HRES of given event
        for(i in seq(1,51,1))
        {
            bit<-jtwc[,c('Perspective','Source','StormName','Number','DateTimeUTC','Number','lon','lat','mslp','vmax')]
            if(as.logical(1-((i-1) %/% 50)))
                {pipik<-'ENS'} else {pipik<-'HRES'}
            bit$EventInfo_EventName<-paste(pipik,'ECMWF',bit$StormName,str_pad(i,pad=0,width=2),sep='_')
            add<-rbind(add,bit)        
        }
        # if(length(tr$Perspective)>0)
        # {
        #     for(i in seq(1,length(u),1))
        #     {
        #         cstr<-unlist(str_split(u[i],'_'))
        #         tmp<-jtwc[jtwc$StormName==cstr[3],c('Perspective','Source','StormName','Number','DateTimeUTC','Number','lon','lat','mslp','vmax')]
        #         tmp$EventInfo_EventName<-paste(cstr[1],cstr[2],tmp$StormName,cstr[4],sep = '_')
        #         add<-rbind(add,tmp)
        #     }
        # } else {
        #     add<-jtwc[,c('Perspective','Source','StormName','Number','DateTimeUTC','Number','lon','lat','mslp','vmax')]
        #     add$EventInfo_EventName<-
        # }
        
        # in case per event  per perspective ('ENS' or 'HRES') there is not a single row of forecast then use all 
        #   rows from 'add' for that given event and perspective
        # in case per event  per perspective ('ENS' or 'HRES') there is at least one row of forecast then 
        #   check which events have a forecast and delete all other events from 'add' for that given event and perspective
        for(i in seq(1,length(TCname),1))
        {
            # check this event for ENS
            strg<-paste('ENS','ECMWF',TCname[i],sep='_')
            if(sum(str_detect(tr$EventInfo_EventName,strg))==0)
            {
                tr<-rbind(tr,add[str_detect(add$EventInfo_EventName,strg)])
            } else 
            {
                # detect all records for given event and perspective
                nam<-unique(tr$EventInfo_EventName[str_detect(tr$EventInfo_EventName,strg)])
                tr<-rbind(tr,add[add$EventInfo_EventName %in% nam,])
                rm(nam)
            }
            
            # check this event for HRES
            strg<-paste('HRES','ECMWF',TCname[i],sep='_')
            if(sum(str_detect(tr$EventInfo_EventName,strg))==0)
            {
                tr<-rbind(tr,add[str_detect(add$EventInfo_EventName,strg)])
            } else 
            {
                # detect all records for given event and perspective
                nam<-unique(tr$EventInfo_EventName[str_detect(tr$EventInfo_EventName,strg)])
                tr<-rbind(tr,add[add$EventInfo_EventName %in% nam,])
                rm(nam)
            }
            
        }

        rm(u,i,add,tmp,strg)
        
        setorderv(tr,cols=c('EventInfo_EventName','DateTimeUTC'),order=c(1,1))
    
        # Assign EventID per Storm name
        # at this moment we are splitting the result per storm to account for eventuality that for one forecast date
        # there can be more than one tracks affecting Japan 
        e<-data.table(EventInfo_EventName=sort(unique(tr$EventInfo_EventName)))
        x<-str_split(e$EventInfo_EventName,'_',simplify=TRUE)
        e$EventID=as.numeric(x[,4])
        #e$EventID[x[,1]=='HRES']<-e$EventID[x[,1]=='HRES']+50
        e$StormName<-x[,3]
        rm(x)
        tr$EventID<-e$EventID[match(tr$EventInfo_EventName,e$EventInfo_EventName)]
        
        # save each event separately
        for(i in seq(1,length(unique(tr$StormName)),1))
        {
          str<-tr[tr$StormName==unique(tr$StormName)[i],]  
          setorderv(str,cols=c("EventID","DateTimeUTC"))
          #fwrite(str,file = paste('./TK_PROC_Scripts/TRACKS/Combined/CombinedTracks_',unique(tr$StormName)[i],'_',label,'.csv',sep=''),row.names = FALSE,quote = FALSE)
		  fwrite(str,file = paste('./TK_PROC_Scripts/TRACKS/Combined/Season',YYYY,'_CombinedTracks_',unique(tr$StormName)[i],'_',label,'.csv',sep=''),row.names = FALSE,quote = FALSE)
          
        }

    }
}
