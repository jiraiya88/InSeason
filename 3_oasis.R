
library(stringr)

options(digits=22)

lst<-list.files(path='./hazard/',pattern = '*.csv')

hold<-data.frame(Event_ID=integer(0),
                 AREAPERIL_ID=integer(0),
                 INTENSITY_BIN_INDEX=integer(0),
                 PROB=double(0),
                 stringsAsFactors = FALSE)

for(i in seq(1,length(lst)))
{
  print(i/length(lst))
  bit<-read.csv(paste('./hazard/',lst[i],sep=''))
  hold<-rbind(hold,data.frame(Event_ID=as.integer(str_sub(lst[i],1,6)),
                              AREAPERIL_ID=bit$GRIDID*100000+10392,
                              INTENSITY_BIN_INDEX=bit$HAZARD,
                              PROB=1,
                            stringsAsFactors = FALSE))
}
rm(i,lst,bit)
hold<-hold[hold$INTENSITY_BIN_INDEX>20,]

write.csv(hold,file = './oasis/EventFootprint_Grid_Peril_1.csv',row.names = FALSE,quote = FALSE)

# create EVENTINFO file and save
ei<-data.frame(EVENT_ID=sort(unique(hold$Event_ID)),
               EVENT_NAME=sort(unique(hold$Event_ID)),
               EVENT_FREQ=0,
               OCC_YEAR_FREQ=0,
               OCC_YEAR=str_sub(as.character(Sys.Date()),1,4),
               OCC_MONTH=0,
               OCC_DAY=0,
               EVENT_DURATION=0,
               DEMAND_SURGE=1,
               stringsAsFactors = FALSE)
write.csv(ei,file = './oasis/EventInfo.csv',row.names = FALSE,quote = FALSE)
rm(ei)

source('s06_createMultiPerilTable.R')
sloc<-paste('./oasis/',sep='')
getOASIS(paste('./oasis/',sep=''),sloc)
