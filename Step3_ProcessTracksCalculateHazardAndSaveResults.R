###################################################
### Following changes have been done in this script.
###
### 1. Added Command line argument to get the active storm name. 
### 2. Added function to get the label,fdate and YYYY from _AER_Parameters.csv file.
### 3. Changed the folder names from 
###    Logs to logs
### 4. Changes to open the log file in append mode
###################################################

args <- commandArgs()
argStorm<-args[2]
basePath<-args[3]

###############
### LIBRARIES
###############
    library(data.table)
    library(stringr)

##################
setwd(basePath)

###############
### FUNCTIONS
###############

    # get the last date and time which is a multiple of given hour
    FloorDateTime<-function(posixctdate,step,hdelay)
    {
      # step is whole multiple of hour before current time minus hdelay
      # hdelay is delay in hours to the past
      
      require(lubridate)
      FloorDateTime<-floor_date(posixctdate,unit = "days")+floor(hour(posixctdate)/step)*step*3600 
      return(FloorDateTime)
    }

    print('@@@ Read AER parameters')
    print('    ...')
	nproc<-26
    param<-fread('./TK_PROC_Scripts/_AER_Parameters.csv')
    fdate<-param$ForecastDateIfAny
    hdelay<-7  # seven hours delay between forecast time and current time, only works for 'forecast' option
    
    # based on Mode decides whether to look for actual forecast or forecasts from the past (hindcast)
    label<-FloorDateTime(as.POSIXct(paste(str_sub(fdate,1,4),'-',
	                               str_sub(fdate,5,6),'-',
	                               str_sub(fdate,7,8),' ',
	                               str_sub(fdate,9,10),':00',sep=''),tz='UTC'),6,0)
    YYYY<-year(label)   # season in which to look for answer
    fdate<-label
    label<-format(fdate,format='%Y%m%d%H')
    rm(param)

###################################################
#### Establish connection to logfile in append mode
###################################################   
    con<-file(paste('./TK_PROC_Scripts/logs/Run_',label,'.txt',sep=''),'a')
    sink(con, append=TRUE)
    sink(con, append=TRUE, type="message")

######################################################################################
####  process track, find track parameters and prepare for hazard calculation
######################################################################################
      print('@@@ Executing storm parameter estimate')
	  print(argStorm)
      print('    ...')
      source('./TK_PROC_Scripts/s04_processTracks.R')
      TrackProcess(label,fdate,argStorm)
      rm(TrackProcess)
      print('    ...')
      print('@@@ Finished storm parameter estimate')

#####################################
####  calculate hazard
#####################################
      print('@@@ Executing hazard calculation')
	  print(argStorm)
      print('    ...')
      source('./TK_PROC_Scripts/s05_HazardCalc.R')
      HazardCalc(fdate,label,nproc,argStorm)
      rm(HazardCalc)
      print('    ...')
      print('@@@ Finished hazard calculation')

#########################################
####  save results in one place
#########################################
      print('@@@ Executing data transfer')
	  print(argStorm)
      print('    ...')
      # check whether OASIS table exists for given forecast date
      dr<-list.files(path='./TK_PROC_Scripts/hazard/OasisTables/',pattern=paste(argStorm,'_',label,sep=''),recursive = TRUE, include.dirs = TRUE)
      dr<-dr[ file.info(paste('./TK_PROC_Scripts/hazard/OasisTables/',dr,sep = ''))$isdir ]
      dr<-dr[!is.na(dr)]
      
      if(length(dr)>0)
      {
          for(i in seq(1,length(dr),1))
          {
            dir.create(paste('./TK_PROC_Scripts/OUTPUT/',dr[i],sep=''))
            # copy EventInfo
            file.copy(paste('./TK_PROC_Scripts/hazard/OasisTables/',dr[i],'/EventInfo.csv',sep=''),
                      paste('./TK_PROC_Scripts/OUTPUT/',dr[i],'/EventInfo.csv',sep=''))
            # copy EventMultiperil
            file.copy(paste('./TK_PROC_Scripts/hazard/OasisTables/',dr[i],'/EventFootprint_Multiperil.csv',sep=''),
                      paste('./TK_PROC_Scripts/OUTPUT/',dr[i],'/EventFootprint_Multiperil.csv',sep=''))
            # copy EventFootprint_Grid_Peril_1
            file.copy(paste('./TK_PROC_Scripts/hazard/OasisTables/',dr[i],'/EventFootprint_Grid_Peril_1.csv',sep=''),
                      paste('./TK_PROC_Scripts/OUTPUT/',dr[i],'/EventFootprint_Grid_Peril_1.csv',sep=''))
            # copy map of tracks
            #file.copy(paste('./TK_PROC_Scripts/hazard/Plots/',dr[i],'/TrackPlot.png',sep=''),
            #          paste('./TK_PROC_Scripts/OUTPUT/',dr[i],'/TrackPlot.png',sep=''))
          }
      } else {
          dir.create(paste('./TK_PROC_Scripts/OUTPUT/NOTHING_',label,sep=''))
      }      
      print('    ...')
      print('@@@ Finished data transfer')
	  
##########################################
#### Restore output to console
##########################################
	  sink() 
      sink(type="message")
