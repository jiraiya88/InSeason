###################################################
### Following changes have been done in this script.
###
### 1. Changed the folder names from 
###    Logs to logs
###    track to TRACKS
###################################################

args <- commandArgs()
basePath<-args[2]

##############
### LIBRARIES
##############
    library(data.table)
    library(stringr)

##################
setwd(basePath)

##############
### FUNCTIONS
##############
    # get the last date and time which is a multiple of given hour
    FloorDateTime<-function(posixctdate,step,hdelay)
    {
      # step is whole multiple of hour before current time minus hdelay
      # hdelay is delay in hours to the past
      
      require(lubridate)
      FloorDateTime<-floor_date(posixctdate,unit = "days")+floor(hour(posixctdate)/step)*step*3600 
      return(FloorDateTime)
    }

###########
### INPUTS
###########
    print('@@@ Setting parameters')
    print('    ...')
    nproc<-26
    param<-fread('./TK_PROC_Scripts/_AER_Parameters.csv')
    XX<-param$XX
    fdate<-param$ForecastDateIfAny
    hdelay<-7  # seven hours delay between forecast time and current time, only works for 'forecast' option
    nattempt<-6
    
    # based on Mode decides whether to look for actual forecast or forecasts from the past (hindcast)
    if(param$Mode=='hindcast')
    {
      label<-FloorDateTime(as.POSIXct(paste(str_sub(fdate,1,4),'-',
                                       str_sub(fdate,5,6),'-',
                                       str_sub(fdate,7,8),' ',
                                       str_sub(fdate,9,10),':00',sep=''),tz='UTC'),6,0)
    } else {
      if(param$Mode=='forecast')
      {
        fdate<-format(Sys.time()-hdelay*3600,format = '%Y%m%d%H',tz='UTC')
        label<-FloorDateTime(as.POSIXct(paste(str_sub(fdate,1,4),'-',
                                         str_sub(fdate,5,6),'-',
                                         str_sub(fdate,7,8),' ',
                                         str_sub(fdate,9,10),':00',sep=''),tz='UTC'),6,hdelay)
        # if forecast is required make note in parameters file of the required forecast date
        param$ForecastDateIfAny<-fdate
        fwrite(param,file = './TK_PROC_Scripts/_AER_Parameters.csv')
      } else {
        print('Forecast or hindcast?')
      }
    }
    YYYY<-year(label)   # season in which to look for answer
    fdate<-label
    label<-format(fdate,format='%Y%m%d%H')
    rm(param)

###################################################
#### STEP 0: Establish connection to logfile
###################################################    
	closeAllConnections() 
    con<-file(paste('./TK_PROC_Scripts/logs/Run_',label,'.txt',sep=''))
    sink(con, append=TRUE)
    sink(con, append=TRUE, type="message")
    
########################################    
#### STEP 1: download JTWC observations
########################################
    
    print('@@@ Executing download of observed tracks')
    print('    ...')
    source('./TK_PROC_Scripts/s01_download_tcvitals_JTWC.R')

    ## check if target folder exists
    if(!dir.exists(paste('./TK_PROC_Scripts/TRACKS/',as.character('JTWC'),sep='')))
    {
      dir.create(paste('./TK_PROC_Scripts/TRACKS/',as.character('JTWC'),sep=''))
    }
	if(!dir.exists(paste('./TK_PROC_Scripts/TRACKS/',as.character('ECMWF'),sep='')))
    {
    	dir.create(paste('./TK_PROC_Scripts/TRACKS/',as.character('ECMWF'),sep=''))
    }
	if(!dir.exists(paste('./TK_PROC_Scripts/TRACKS/',as.character('Ready'),sep='')))
    {
      dir.create(paste('./TK_PROC_Scripts/TRACKS/',as.character('Ready'),sep=''))
    }
    ## gets a list of files on JTWC server, compares with list of files already downloaded (in ./TRACKS/JTWC/`YYYY`/)
    ## tracks of all storms is now downloaded, up to the date suggested in parameter file
    ## Dashboard file shows active storms as those which have a valid observation at the data suggested in parameter file
    dsh<-try(getTrackJTWC(XX,YYYY,fdate,label))
    rm(getTrackJTWC)
    print('    ...')
    print('@@@ Finished download of observed tracks')
    
#########
#### STEP 0: Restore output to console
#########    
    sink() 
    sink(type="message")
