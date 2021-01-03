setwd("C:/Users/Vince/Desktop/report/workstation")
library(readxl)
library(RSQLite)
library(tidyr)
library(zoo)
library(xts)
library(data.table)
library(dplyr)
library(lubridate)
library(plotly)
library(moments)
library(rollRegres)

####read mapping
mapping=read_excel("sector_mapping.xlsx", sheet="Sheet2")

### 1. fetch data
con <- dbConnect(SQLite(), "ohlc.sqlite")

#### select universe
#"SH000300"300/ "SH000905"500/ "SH000906"800/
#finally decide use ZZ800 as it is more balanced compared with other two
index_name <-"SH000906" 
sql=paste(paste('"',mapping$Code,'"',sep=''),collapse=',')
index_weight <- dbSendQuery(con,paste0("select Date,Code,",index_name," from index_wei where Code in (",sql,") and date > '2010-01-01';"))
index_all = dbFetch(index_weight)
dbClearResult(index_weight)
universe=na.omit(index_all)
universe=unique(universe$Code)
####all stock universe
#universe=mapping$Code

####sanity check stock counts
aaa=na.omit(index_all)
check_stock_counts = group_by(aaa,Date,add=T) %>%
  summarize(n=n()) %>%
  as.data.frame()

####pull daily return
sql=paste(paste('"',universe,'"',sep=''),collapse=',')
res <- dbSendQuery(con, paste0("select Date, Code, Ret from eqchina where Code in (",sql,") and date > '2010-01-01';"))
pm = dbFetch(res)
dbClearResult(res)

pm1=merge.data.frame(pm,mapping,all.x=T)
pm1$Ret=pm1$Ret/100
pm1=na.omit(pm1)


######build corresponding index portfolio
mark = pm1$Date[1]
for (each in unique(pm1$Date)){
  
  if (each%in%index_all$Date){
    DateCode <- index_all[index_all$Date==each,]
    DateCode <- na.omit(DateCode)
    mark = each
  }else{
    DateCode <- index_all[index_all$Date==mark,]
    DateCode <- na.omit(DateCode)
  }
  pm1[(pm1$Date==each)&(pm1$Code %in%DateCode$Code),'index']=1
}
pm2=pm1[pm1$index==1,]

check_stock_counts = group_by(pm2,Date,add=T) %>%
  summarize(n=n()) %>%
  as.data.frame()
check_stock_counts = group_by(pm2,Date,Sector,add=T) %>%
  summarize(n=n()) %>%
  as.data.frame()

######generate sector portfolio returns
sector_ret = group_by(pm2,Sector,Date,add=T) %>%
  summarize(Ret=mean(Ret)) %>%
  as.data.frame()
sector_ret

### pull ff5
res <- dbSendQuery(con,"select * from ff5 where date > '2010-01-01' ;")
ff5 = dbFetch(res)
dbClearResult(res)


####put everything together
raw1=merge.data.frame(sector_ret,ff5,all.x=T)
raw1=na.omit(raw1)



#####generate FF5 alpha
allalpha=NULL
for(i in unique(mapping$Sector)){
  stk=raw1[raw1$Sector==i,]
  row.names(stk)=stk$Date
  
  #use trailing 500 trading days/24 month window to obtain alpha
  xxx1=roll_regres(Ret~cma+hml+mkt+rmw+smb,stk,min(nrow(stk),500))$coefs %>% as.data.frame()
  xxx1=na.omit(xxx1)
  xxx1$Date=rownames(xxx1)
  xxx1$Sector=i
  allalpha=rbind(allalpha,xxx1)
}
allalpha$Alpha=allalpha$`(Intercept)`
alpha=allalpha[,-1:-6]
rownames(alpha)=NULL
check_sector_counts = group_by(alpha,Date,add=T) %>%
  summarize(n=n()) %>%
  as.data.frame()

factornames = names(alpha)
alpha_stats=group_by(alpha,Sector,add=T) %>%
  summarize(n=n(),
            mean=mean(get(factornames[3])),
            sd=sd(get(factornames[3])),
            min=min(get(factornames[3])),
            max=max(get(factornames[3]))) %>%
  as.data.frame()

saveRDS(alpha,file="ZZ800_alpha.Rdata")
#boxplot(alpha[alpha$Sector=="交通运输",]$Alpha,frame = T)

###sanity check regression#####
x1=alpha[alpha$Sector==i,]
stk1=stk[stk$Date<='2012-12-18',]
lm(Ret~cma+hml+mkt+rmw+smb,stk1)

