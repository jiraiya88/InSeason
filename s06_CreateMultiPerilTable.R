
getOASIS<-function(opth,sloc)
{
library(data.table)
library(stringr)

options(scipen=99)

w<-fread('./hazard/data/consolidated_weight.csv')

lst<-list.files(path=opth,pattern='*EventFootprint_Grid_Peril_1.csv',recursive = TRUE)
for(i in seq(1,length(lst),1))
{
  print(lst[i])
  c<-Sys.time()
  h<-fread(paste(opth,lst[i],sep=''))
  
  h<-merge(h[,c("Event_ID","AREAPERIL_ID","INTENSITY_BIN_INDEX")],w,by.x=c("AREAPERIL_ID"),by.y=c("GRIDID"),all.x=FALSE,all.y=FALSE,allow.cartesian=TRUE)
  h<-h[,.(PROB=sum(PROB)),by=.(Event_ID,AdminUnit,INTENSITY_BIN_INDEX)]

  h<-data.table(Event_ID=h$Event_ID,
                AREAPERIL_ID=h$AdminUnit,
                INTENSITY_BIN_PERIL_1=h$INTENSITY_BIN_INDEX,
                INTENSITY_BIN_PERIL_2=0,
                INTENSITY_BIN_PERIL_3=0,
                PROB=round(h$PROB,6))
  h<-h[h$PROB>0,]
  fname<-paste(opth,str_replace(lst[i],'_Grid_Peril_1','_Multiperil'),sep='')
  fwrite(h,file=fname,row.names = FALSE,quote = FALSE,append=FALSE)
  print(Sys.time()-c)
}
rm(h,c,fname,i)

}
