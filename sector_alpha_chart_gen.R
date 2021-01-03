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
source('factor_fun.R')

####read mapping
mapping=read_excel("sector_mapping.xlsx", sheet="Sheet2")

### 1. fetch data
con <- dbConnect(SQLite(), "ohlc.sqlite")


### select universe
#"SH000300"300/ "SH000905"500/ "SH000906"800/
index_name <-"SH000300" 
sql=paste(paste('"',mapping$Code,'"',sep=''),collapse=',')
index_weight <- dbSendQuery(con,paste0("select substr(Date,1,7) as Date,Code,",index_name," from index_wei where Code in (",sql,") and date > '2013-01-01';"))
index_all = dbFetch(index_weight)
dbClearResult(index_weight)

### pull end of month stock close price
sql=paste(paste('"',mapping$Code,'"',sep=''),collapse=',')
res <- dbSendQuery(con, paste0("select substr(Date,1,7) as Date,Code,Close from eqchina_month where Code in (",sql,") and date > '2013-01-01';"))
pm = dbFetch(res)
dbClearResult(res)
### for forward return calculation purpose
Ret=pm[,1:2]
pm = as.data.frame(pivot_wider(pm,names_from = Code,values_from = Close))
pm = na.locf(pm,na.rm = FALSE)

### pull pre-calculated alpha for stock universe
sql=paste(paste('"',mapping$Code,'"',sep=''),collapse=',')
res <- dbSendQuery(con,paste0("select distinct(Date) from alpha where Code in (",sql,") and date > '2013-01-01';"))
factor = dbFetch(res)
dbClearResult(res)
####read pre-calculated alpha dataset
alpha=readRDS("ZZ800_alpha.Rdata")
factor=merge.data.frame(factor,alpha,all.x=T)
factor$Date=substr(factor$Date,1,7)


# get forward returns with freqs
freqs = c(1,3,6)

for (freq in freqs){
  t=pm[,-1]
  a=as.data.frame(shift(t,n=-freq))
  r=pm
  r[,-1]=(a-t)/t
  r1 = r %>% pivot_longer(-Date, names_to = "Code", values_to = paste0("Ret",freq))  %>% as.data.frame()
  Ret = merge.data.frame(Ret,r1,all.x=T)
}

Ret= merge.data.frame(Ret,mapping,all.x=T)
Ret=Ret[order(Ret$Code,Ret$Date),]
Ret1=Ret

######build corresponding index portfolio
mark = Ret$Date[1]
for (each in unique(Ret$Date)){
  
  if (each%in%index_all$Date){
    DateCode <- index_all[index_all$Date==each,]
    DateCode <- na.omit(DateCode)
    mark = each
  }else{
    DateCode <- index_all[index_all$Date==mark,]
    DateCode <- na.omit(DateCode)
  }
  Ret[(Ret$Date==each)&(Ret$Code %in%DateCode$Code),'index']=1
}
aaa=Ret[!is.na(Ret$index),]
check_stock_counts = group_by(aaa,Date,add=T) %>%
  summarize(n=n()) %>%
  as.data.frame()

####generate sector portfolio returns
sector_ret = group_by(aaa,Sector,Date,add=T) %>%
  summarize(Ret1=mean(Ret1),
            Ret3=mean(Ret3),
            Ret6=mean(Ret6)) %>%
  as.data.frame()
sector_ret
factor=merge.data.frame(factor,sector_ret,all.x=T)

####generate market returns
market_ret = group_by(aaa,Date,add=T) %>%
  summarize(mRet1=mean(Ret1),
            mRet3=mean(Ret3),
            mRet6=mean(Ret6)) %>%
  as.data.frame()
market_ret
factor=merge.data.frame(factor,market_ret,all.x=T)

#####remove sector not in index constituents
factor=factor[!is.na(factor$Ret1),]

### rank sector alpha in terciles
quantiles = c(0,0.33,0.67,1)
q_names = c(1,2,3)
mark = factor$Date[1]
t=c(0,0,0,0,0,0)

for (each in unique(factor$Date)){
  
  t = factor[(factor$Date==each),]
  factor[(factor$Date==each),'factor_quantile'] = cut(t[,3],quantile(t[,3],names=FALSE,na.rm=T,probs=quantiles),labels=q_names,include.lowest = T)
  factor[(factor$Date==each),'rRet1'] <-  factor[(factor$Date==each),'Ret1']- factor[(factor$Date==each),'mRet1']
  factor[(factor$Date==each),'rRet3'] <-  factor[(factor$Date==each),'Ret3']- factor[(factor$Date==each),'mRet3']
  factor[(factor$Date==each),'rRet6'] <-  factor[(factor$Date==each),'Ret6']- factor[(factor$Date==each),'mRet6']
  print(quantile(t[,3],names=FALSE,na.rm=T,probs=quantiles))
  
}

### check alpha quantile statistics
factornames = names(factor)

factor_stats = group_by(factor,factor_quantile,add=T) %>%
  summarize(n=n(),
            mean=mean(get(factornames[3])),
            sd=sd(get(factornames[3])),
            min=min(get(factornames[3])),
            max=max(get(factornames[3]))) %>%
  as.data.frame()
factor_stats



# plot tercile average relative return to index
#### a
retnames=c('rRet1','rRet3','rRet6')
ret.table = group_by(factor,Date,add=T) %>%
  group_by(factor_quantile,add=T) %>%
  summarize(a = mean(get(retnames[1]),na.rm = T),
            b = mean(get(retnames[2]),na.rm = T),
            c = mean(get(retnames[3]),na.rm = T)) %>%
  as.data.frame()
colnames(ret.table) = c('Date','factor_quantile',retnames)
#### b
ret.table.mean = group_by(ret.table,factor_quantile,add=T) %>%
  summarize(a = mean(get(retnames[1]),na.rm = T),
            b = mean(get(retnames[2]),na.rm = T),
            c = mean(get(retnames[3]),na.rm = T)) %>%
  as.data.frame()
colnames(ret.table.mean) = c('factor_quantile',retnames)

ret.plotly <- plot_ly(data = ret.table.mean,x=~factor_quantile)
for (i in 2:ncol(ret.table.mean)){
  ret.plotly <- ret.plotly %>%
    add_bars(y = ret.table.mean[,i],name = retnames[i-1])
}
ret.plotly

write.csv(ret.table.mean,paste0(index_name,"_ret_table_mean.csv"))

### 5. IC,IR
factornames = names(factor)
retnames = c('rRet1','rRet3','rRet6')

df_ic=data.frame()
for (each in unique(factor$Date)){
  for (i in retnames){
    df_ic[each,i] = cor(rank(factor[factor$Date==each,3]),rank(factor[factor$Date==each,i]))
  }
  i = retnames[1]
}
ic.table=as.data.frame(matrix(nrow=0,ncol=3))
colnames(ic.table) = retnames
ic.table['IC Mean',] = apply(df_ic, 2, function(x) mean(x,na.rm=T))
ic.table['IC Std.',] = apply(df_ic, 2, function(x) sd(x,na.rm=T))
ic.table['t-stat(IC)',] = apply(df_ic, 2, function(x) t.test(x)$statistic)
ic.table['p-value(IC)',] = apply(df_ic, 2, function(x) t.test(x)$p.value)
ic.table['IC Skew',] = apply(df_ic, 2, function(x) moments::skewness(x,na.rm=T))
ic.table['IC Kurtosis',] = apply(df_ic, 2, function(x) moments::kurtosis(x,na.rm=T))
ic.table

write.csv(ic.table,paste0(index_name,"_ic_table.csv"))

###output raw files

retnames=c('Ret1','mRet1')
ret.table = group_by(factor,Date,add=T) %>%
  group_by(factor_quantile,add=T) %>%
  summarize(a = mean(get(retnames[1]),na.rm = T),
            b = mean(get(retnames[2]),na.rm = T)) %>%
  as.data.frame()
colnames(ret.table) = c('Date','factor_quantile',retnames)

ret.table=ret.table[order(ret.table$factor_quantile,ret.table$Date),]
write.csv(ret.table,paste0(index_name,"_raw.csv"))



cur=factor[factor$Date=='2020-03',]
cur=cur[order(cur$Alpha),]

rownames(cur)=NULL






####################################################cumulative return plot########################################
###plot results
# plot 1-3 and longshort monthly cumulative return relative to index
retnames=c('Ret1','Ret3','Ret6')
ret.table = group_by(factor,Date,add=T) %>%
  group_by(factor_quantile,add=T) %>%
  summarize(a = mean(get(retnames[1]),na.rm = T),
            b = mean(get(retnames[2]),na.rm = T),
            c = mean(get(retnames[3]),na.rm = T)) %>%
  as.data.frame()
colnames(ret.table) = c('Date','factor_quantile',retnames)

cumrt.combine = pivot_wider(ret.table[,c('Date','factor_quantile',retnames[1])],names_from = factor_quantile,values_from = retnames[1]) %>%as.data.frame()
cumrt.combine = merge.data.frame(cumrt.combine,market_ret[,1:2],all.x=T)

rownames(cumrt.combine) = cumrt.combine$Date
cumrt.combine$Date = apply(cumrt.combine[,2:length(cumrt.combine)],1,mean) # trick, just use $Date to do something,create new column used for longshort
cumrt.combine$Date = cumrt.combine$`3` - cumrt.combine$`1`
colnames(cumrt.combine) = c('longshort','1','2','3','mkt')
cumrt.combine = cumprod(cumrt.combine+1)-1###calculate cumulative return
#calculate cum return relative to index average
cumrt.combine$'1'=cumrt.combine$'1'-cumrt.combine$mkt
cumrt.combine$'2'=cumrt.combine$'2'-cumrt.combine$mkt
cumrt.combine$'3'=cumrt.combine$'3'-cumrt.combine$mkt
cumrt.combine=cumrt.combine[,1:4]

cumrt_group = plot_ret(cumrt.combine)
cumrt_group #chart of cumulative return for each quintile and longshort

write.csv(cumrt.combine,paste0(index_name,"_cumulative.csv"))






