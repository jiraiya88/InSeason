###################################################
### Following changes have been done in this script.
###
### 1. Change the folder name from track to TRACKS
###################################################

HazardCalc<-function(fdate,label,nproc,storm)
{
library(data.table)
library(lubridate)
library(stringr)

source('./TK_PROC_Scripts/s05a_CalcHaz.R')
source('./TK_PROC_Scripts/s05d_createMultiPerilTable.R')

lst<-list.files(path='./TK_PROC_Scripts/TRACKS/Ready/',pattern=paste('Tracks_',storm,'_',label,'\\.Rdata',sep=''))
print(lst)

for(i in seq(1,length(lst),1))
{
  ### STEP 1:
  ### Calculate footprints in Japan accounting for terrain and directional roughness
  
  calcHaz(paste('./TK_PROC_Scripts/TRACKS/Ready/',lst[i],sep=''),
          label,
          nproc)
  
  ### STEP 2:
  ### Calculate OASIS hazard tables for Japan
  sloc<-paste('./TK_PROC_Scripts/hazard/OasisTables/',str_replace(str_replace(lst[i],'Tracks_',''),'\\.Rdata',''),'/',sep='')
  getOASIS(paste('./TK_PROC_Scripts/TRACKS/Ready/',str_replace(str_replace(lst[i],'Tracks_',''),'\\.Rdata',''),'/',sep=''),sloc)
}

}

