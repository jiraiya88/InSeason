###################################################
### Following changes have been done in this script.
###
### 1. Command line argument to indicate whether the script should download the forecasts or not. This value will be 0 (Don't download)
### 2. Added function to get the label,fdate and YYYY from _AER_Parameters.csv file.
### 3. Changed the folder names from 
###    Logs to logs
###    track to TRACKS
### 4. Changes to open the log file in append mode
###################################################

args <- commandArgs()
canDownload<-args[2]
basePath<-args[3]

### LIBRARIES
#########
    library(data.table)
    library(stringr)
    

##################
setwd(basePath)
	
##############
### FUNCTIONS
#############
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
    
    download.forecast<-function(label,stormname,dest)
	  {
      library(httr)
      library(xml2)
      library(stringr)
      
      # source of ECMWF forecasts 
	  date1=paste(substr(as.character(label),1,4),substr(as.character(label),5,6),substr(as.character(label),7,8),sep='-')
	  date1=as.POSIXct(date1,format="%Y-%m-%d",tz="UTC")
	  ECWMF_end_date='2022-10-18'
	  if (date1>=as.POSIXct(ECWMF_end_date,format="%Y-%m-%d",tz="UTC")){
      urllist<-'https://diss.ecmwf.int/ecpds/file'
      urlfile<-'https://diss.ecmwf.int/ecpds/home/wmo'
	  }else {
	       # source of ECMWF forecasts  
      urllist<-'https://dissemination.ecmwf.int/ecpds/data/list'
      urlfile<-'https://dissemination.ecmwf.int/ecpds/data/file'}
      
      
      stormname<-str_to_upper(stormname)
      
      # set up proxy gate
      set_config(config(proxy = "proxy.rpp.aon.net:8080", 
                        proxyuserpwd = "aonnet\\svc-INNH848622:qU2WIIUY", 
                        proxyauth = 4))
      
      ForecastList<-'Access denied'
      c<-1
      print('   ... Getting list of forecasts')
      while(ForecastList=='Access denied' & c<50)
      {
        print(paste0('   ... trying to get list of forecasts from the server: ',c,'/50'))
        ForecastList<-content(GET(paste0(urllist,'/',label,'0000/'),config=authenticate('wmo','essential', type = "basic")),"text",encoding = "UTF-8")
        Sys.sleep(1)
        c<-c+1  
      }
      rm(c)
      if(ForecastList!='Access denied')
      {
        print(paste0('   ... list of forecasts succesfully retrieved from server'))
      }
      
      if(!(ForecastList %in% c('Access denied','No such file or directory')))
      {
        # ForecastList is not very clean xml, so we need to improvise
        # search for string with 
        #         - start with > and and with < indicating link within html tags
        #         - consists of '_', digits, leters, upper and lowerbound
        #         - ends with bin
        #         - allow empty space ' ' just in case > and < are offsetted by some
        ForecastList<-unlist(str_extract_all(as.character(unlist(ForecastList)),'>[ _0-9a-zA-Z/.bin]+<'))
        # clean excess characters: ' ','<','>'
        ForecastList<-str_replace_all(ForecastList,'>|<| ','')
        ForecastList<-ForecastList[str_detect(ForecastList,paste0('_',stormname,'_'))]
        
        if(length(ForecastList)>0)
        {
          for(i in seq(1,length(ForecastList)))
          {
            print(paste0('   ... Downloading file ',ForecastList[i]))
            c<-1
            r<-data.frame(status_code=0)
            while(r$status_code!=200 & c<40)
            {
              print(paste0('     attempt ',c,' out of 20'))
              r<-GET(paste0(urlfile,'/',label,'0000/',ForecastList[i]),authenticate("wmo", "essential"),write_disk(path=paste0(dest,'/',ForecastList[i]),overwrite = TRUE))
              c<-c+1
            }
            if(r$status_code==200)
            {
              print(paste0('       file succesfully retrieved from server'))
            }
          }
          download.forecast<-1
        } else {
          download.forecast<-NULL
          print('   No forecast is available')
        }
        
      } else {
        download.forecast<-NULL
        print('   No forecast is available')
      }
      
    }

###################################################
#### Establish connection to logfile
###################################################    
    con<-file(paste('./TK_PROC_Scripts/logs/Run_',label,'.txt',sep=''),'a')
    sink(con, append=TRUE)
    sink(con, append=TRUE, type="message")

#############################################################
#### download ECMWF forecast
#############################################################
    dsh<-read.csv('./TK_PROC_Scripts/TRACKS/Track_Dashboard.csv')
    if(!is.null(dsh))
    {
    print('@@@ Executing download of forecasts')
    print('    ...')
    if(sum(dsh$Active)>0)
        {
	  if(canDownload=='1')
	  {	
          	# run R script to download ECMWF forecasts
          	for(i in seq(1,sum(dsh$Active),1))
          	{
            	download.forecast(label,dsh$StormName[dsh$Active==1][i],'./temp')
          	}
          	print('@@@ Finished download of forecasts')
          	print('    ...')
	  } else {
		print('@@@ Forecast files already downloaded')
	  }
	  
          # run python script for decoding ENS and HRES tracks from ECMWF server
	    #if(!dir.exists(paste('./TK_PROC_Scripts/TRACKS/',as.character('ECMWF'),sep='')))
      		#{
      	#		dir.create(paste('./TK_PROC_Scripts/TRACKS/',as.character('ECMWF'),sep=''))
      	#	}
		  # decmwf<-paste('C:/Miniconda3/python.exe ','"',basePath,'\\TK_PROC_Scripts\\s02b_decode_ECMWF.py"',sep='')
		  # d<-try(system(decmwf, intern = TRUE))

    } else {
          print('@@@ No active storms in the basin')
                    }
    } else {
      print('    ...')
      print('@@@ No forecast downloaded, because observation file is missing')
    } 

    #lst<-list.files(path='./temp/',pattern='*')
    #if(length(lst)>0) file.remove(paste('./temp/',lst,sep=''))   # remove temporary files
    #rm(lst)
	check<-sum(dsh$Active)>0
	if(check)
	{
	##########################################################
	####  combine ECMWF forecast and JTWC observation
	##########################################################
		  # Create combined folder before calling the script
		  if(!dir.exists(paste('./TK_PROC_Scripts/TRACKS/',as.character('Combined'),sep='')))
				{
					dir.create(paste('./TK_PROC_Scripts/TRACKS/',as.character('Combined'),sep=''))
				}
		  print('@@@ Executing combination of observed and forecasted tracks')
		  print('    ...')
		  source('./TK_PROC_Scripts/s03_CombineTracks.R')
		  TrackCombine(dsh,fdate,YYYY)
		  rm(TrackCombine)
		  print('    ...')
		  print('@@@ Finished combination of observed and forecasted tracks')
		  
		  # plot the tracks
		  #print('@@@ Executing track plotting')
		  #print('    ...')
		  #try(system("C:/Miniconda3/python.exe  C:/ABIF/SW_Tools/IF-AutoEventResponsePlatform-JPN/IF-AER-CommonLibraryTests/bin/Debug/TK_PROC_Scripts/s03b_PlotCombinedTracks.py"))
		  #print('    ...')
		  #print('@@@ Finished track plotting')
		  
	}

##########################################
#### Restore output to console
##########################################    
      sink() 
      sink(type="message")
