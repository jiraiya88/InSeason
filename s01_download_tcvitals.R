################################################################################
### Following changes have been done in this script.
###  
### Changed the name of the track file from Track_Dashboard.csv to StormList.csv, 
### which will have the list of all active storms.
### Before executing the next subsequent scripts, a new Track_Dashboard.csv will be created with just one record for each active storm
### so that the scripts will only process one storm at a time.    
################################################################################

getTrackJTWC<-function(XX,YYYY,fdate,label)

{
###
# Script below downloads observed tracks from b-deck (original data from JTWC) and saves them after extraction of useful information
# input comes from: http://hurricanes.ral.ucar.edu/repository/data/bdecks_open/
# output saved at:  ./TRACKS/JTWC/
# Files are only downloaded if they are either missing (that means not yet downloaded previously) or time stamp of the file on the server
#    is newer than time stamp on the local file. That means, the file on the server has been updated and contains new information

library(data.table)
library(lubridate)
library(stringr)
library(RCurl)
#library(XML)
#library(httr)

        ### SETTING PROXY CONFIG
        #set_config(use_proxy(url="http://pac.aon.net",port=9001,auth = "basic"))
        #options(RCurlOptions = list(proxy="http://pac.aon.net:9001"))
        #Sys.setenv(http_proxy = "http://pac.aon.net:9001")
        
        ### FUNCTIONS
        
        # conversion of text format of Latitude/Longitude do decimal numbers
        LatTxt2Double<-function(Latitude)
        {
          require(stringr)
          
          lnk<-data.frame(Char=c('S','N'),Var=c(-1,1))
          sgn<-str_sub(Latitude,str_length(Latitude),str_length(Latitude))
          sgn<-as.integer(lnk$Var[match(sgn,lnk$Char)])
          LatTxt2Double<-sgn*as.double(str_sub(Latitude,1,str_length(Latitude)-1))/10
          
          return(LatTxt2Double)
        }
        LonTxt2Double<-function(Longitude)
        {
          require(stringr)
          
          lnk<-data.frame(Char=c('E','W'),Var=c(1,-1))
          sgn<-str_sub(Longitude,str_length(Longitude),str_length(Longitude))
          sgn<-as.integer(lnk$Var[match(sgn,lnk$Char)])
          LonTxt2Double<-sgn*as.double(str_sub(Longitude,1,str_length(Longitude)-1))/10
          
          return(LonTxt2Double)
        }
        
        #########

# get list of files on JTWC repository matching requirement
# the structure of the file name is bXXNNYYYY.dat
# XX     - two digits identifying basin: sh - South Hemisphere; wp - West Pcific; al - Atlantic;
#          ep - East Pacific; cp - Central Pacific; io - Indian Ocean
# NN     - two digits for sequential number of storm 
# YYYY   - 4 digits for year 
        
# file with basic parameters such as basin (wp = West Pacific), 
# whether we want actual forecasts of forecast from the past (Mode='hindcast' or Mode='forecast') and
# date of forecast validity

### read the list of events/files on the server and download those which are newer than those we already have 

        url<-paste('http://hurricanes.ral.ucar.edu/repository/data/tcvitals_open/combined_tcvitals.',YYYY,'.dat',sep='')
        # will try to get the list of events 20 times

# list of storm names which should never go for further processing, because
#   - the name is just a number or some version of a number ("TWO" or "02W"). This means the storm is insignificant
#   - the name is "INVEST". This name is used for multiple tracks and messes everything up
donts<-c("INVEST",
         "ONE","01W",
         "TWO","02W",
         "THREE","03W",
         "FOUR","04W",
         "FIVE","05W",
         "SIX","06W",
         "SEVEN","07W",
         "EIGHT","08W",
         "NINE","09W",
         "TEN","10W",
         "ELEVEN","11W",
         "TWELVE","12W",
         "THIRTEEN","13W",
         "FOURTEEN","14W",
         "FIFTEEN","15W",
         "SIXTEEN","16W",
         "SEVENTEEN","17W",
         "EIGHTEEN","18W",
         "NINETEEN","19W",
         "TWENTY","20W",
         "TWENTY-ONE","21W",
         "TWENTY-TWO","22W",
         "TWENTY-THR","23W",
         "TWENTY-FOU","24W",
         "TWENTY-FIV","25W",
         "TWENTY-SIX","26W",
         "TWENTY-SEV","27W",
         "TWENTY-EIG","28W",
         "TWENTY-NIN","29W",
         "THIRTY","30W",
         "TWENTY-ON",
         "TWENTY-TW",
         "TWENTY-TH",
         "TWENTY-FO",
         "TWENTY-FI",
         "TWENTY-SI",
         "TWENTY-SE",
         "TWENTY-EI",
         "TWENTY-NI")
        
### DOWNLOAD JTWC TRACKS        
        

                # try to download multiple times if needed
                r <- NULL
                attempt <- 1
                xisnot<-!file.exists('./TK_PROC_Scripts/temp/x.dat')
                while( xisnot && attempt <= 10 ) 
                {
                        if(attempt>1)
                        {
                          print('Download of obseved track halted for 8 minutes, waiting for better reception')
                          Sys.sleep(8*60)
                        }
                        attempt <- attempt + 1
                        
                        print("   ... Trying R inbuilt download: direct proxy and user")
			                  r <-try(download.file(url,'./TK_PROC_Scripts/temp/x.dat',method='wininet',quiet = TRUE,
			                  http_proxy='proxy.rpp.aon.net:8080',
			                  https_proxy_user='aonnet/svc-INNH848622:qU2WIIUY'))
			                  
			                  xisnot<-!file.exists('./TK_PROC_Scripts/temp/x.dat')
			                  if(xisnot)
			                  {
			                    print("   ... Trying R inbuilt download: direct proxy no user")
			                    r <-try(download.file(url,'./TK_PROC_Scripts/temp/x.dat',method='wininet',quiet = TRUE,
			                                          http_proxy='proxy.rpp.aon.net:8080'))
			                  }
			                  
			                  xisnot<-!file.exists('./TK_PROC_Scripts/temp/x.dat')
			                  if(xisnot)
			                  {
			                    print("   ... Trying R inbuilt download: proxy script no user")
			                    r <-try(download.file(url,'./TK_PROC_Scripts/temp/x.dat',method='wininet',quiet = TRUE,
			                                          http_proxy='http://pac.aon.net:9001'))
			                  }
			                  
# 			                  xisnot<-!file.exists('./temp/x.dat')
# 			                  if(xisnot)
# 			                  {
#   			                  print("XXXXXX  Trying just simple read")
#   			                  r<-try(read.csv(url,header=FALSE))
#   			                  if(is.data.frame(r))
#   			                  {
#   			                    write.csv(r,file='./temp/x.dat',quote = FALSE,row.names = FALSE)    
#   			                  }
# 			                  }
                }

                rm(attempt,r,xisnot)
                
                if(file.exists('./TK_PROC_Scripts/temp/x.dat'))
                {
                        tr<-readChar('./TK_PROC_Scripts/temp/x.dat', file.info('./TK_PROC_Scripts/temp/x.dat')$size)
						# some errors may occur in the file so need to check the file for them
                        # if JTWC or NHC is not at the beginning of a line
						tr<-str_replace_all(tr,' JTWC','\nJTWC')
                        tr<-str_replace_all(tr,' NHC','\nNHC')
						
                        tr<-str_split(tr,'\n',simplify=TRUE)
                        ##tr<-str_replace_all(tr,'\t','')
                        ##tr<-str_replace_all(tr,'\n','')
                        tr<-tr[str_sub(tr,1,4)=='JTWC']
                        #tr<-str_squish(tr)
					    tr<-str_split(tr,' +',simplify=TRUE)
                        ##tr<-str_split(tr,' ',simplify=TRUE)
                        tr<-data.table(Basin=str_sub(tr[,2],3,3),
                                       StormNumber=as.integer(str_sub(tr[,2],1,2)),
                                       DateTimeUTC=as.POSIXct(paste(str_sub(tr[,4],1,4),'-',str_sub(tr[,4],5,6),'-',str_sub(tr[,4],7,8),' ',str_sub(tr[,5],1,2),':00:00',sep=''),format="%Y-%m-%d %H:%M:%S",tz='UTC'),
                                       Source=tr[,1],
                                       Latitude=LatTxt2Double(tr[,6]),
                                       Longitude=LonTxt2Double(tr[,7]),
                                       Vmax=1.94384*as.double(tr[,13]),  # convert m/s to knots
                                       Cp=as.double(tr[,10]),
                                       Penv=tr[,11],
                                       StormName=tr[,3],
                                       Agency=tr[,1],
                                       StormNameOther=NA)
                        tr<-tr[str_to_lower(tr$Basin)==str_to_lower(str_sub(XX,1,1)),]
                        tr$Basin<-str_to_upper(XX)
                        setorderv(tr,cols=c("StormNumber","DateTimeUTC"))
                        
                        # if there are multiple storm names use the last one, but retain previous names
                        x<-tr[,.(maxdate=max(DateTimeUTC)),by=list(StormNumber)]
                        x$Name<-tr$StormName[match(paste(x$StormNumber,x$maxdate,sep='_'),paste(tr$StormNumber,tr$DateTimeUTC,sep='_'))]
                        y<-unique(tr[,c("StormNumber","StormName")])
                        
                        z<-data.frame(StormNumber=as.integer(unique(y$StormNumber)),StormName=as.character(NA))
                        for(i in seq(1,length(z$StormNumber))){
                                l<-y$StormName[y$StormNumber==z$StormNumber[i]][1]
                                if(sum(y$StormNumber==z$StormNumber[i])>1)
                                {
                                        for(j in seq(2,sum(y$StormNumber==z$StormNumber[i])))
                                        {
                                                l<-paste(l,y$StormName[y$StormNumber==z$StormNumber[i]][j],sep='; ')
                                        }
                                }
                                z$StormName[i]<-l
                        }
                        
                        tr$StormNameOther<-z$StormName[match(tr$StormNumber,z$StormNumber)]
                        tr$StormName<-x$Name[match(tr$StormNumber,x$StormNumber)]
                        rm(x,y,z)
                   
                        # figure out time stamp (last recorded time of observation of 12 hour cycle; that is either midnight or noon)
                        tr$DateTimeUTC<-as.POSIXct(tr$DateTimeUTC,format="%Y-%m-%dT%H:%M:%SZ",tz='UTC')
                        tr<-tr[tr$DateTimeUTC<=fdate,]
                        fwrite(tr,file=paste('./TK_PROC_Scripts/TRACKS/JTWC/Season',YYYY,'_ObservedTracks_JTWC_',label,'.csv',sep=''))
                        
                        
                        # cleaning of temporary files
                        #lst<-list.files(path = './temp/',full.names = TRUE)
                        #file.remove(lst)
                        #rm(lst,url)
                        
                        # create Event Dashboard
                        s<-tr[tr$DateTimeUTC<=fdate,.(FirstObservation=min(DateTimeUTC),LastObservation=max(DateTimeUTC),MinimumPressure=min(Cp,na.rm=TRUE),MaximumWind=max(Vmax,na.rm=TRUE)),by=list(Basin,StormNumber,StormName,StormNameOther,Agency)]
                        rm(tr)
                        s$Active<-0
                        # allows the last observation to be 12 hours off, indicating missing two 6 hour steps of track
                        s$Active[(s$LastObservation+12*3600+1)>=fdate]<-1
                        # change last observation to fdate for active storms
                        s$LastObservation[s$Active==1]<-fdate
                        
                        # before finishing make sure forbidden event names are not processed
                        s$Active[s$StormName %in% donts]<-0
                        fwrite(s,file = './TK_PROC_Scripts/TRACKS/StormList.csv',quote = FALSE,row.names = FALSE)
                        getTrackJTWC<-s     
                } else {
                        print('Failed to download JTWC observed tracks!')
                        getTrackJTWC<-NULL
						
                        # push NULL into file for the rest of the code to fail
                        fwrite(getTrackJTWC,file = './TK_PROC_Scripts/TRACKS/StormList.csv',quote = FALSE,row.names = FALSE)
                }



return(getTrackJTWC)
}
