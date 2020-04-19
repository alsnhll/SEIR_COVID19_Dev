library(plotly)
library(dplyr)
library(reshape)
library(scales)
library(readxl)
library(httr)
library(RCurl)
library(tools)
library(stringr)

filePath="~/Documents/Research/Github Local/SEIR_COVID19_Dev/COVID19seir"

# ----------- Load US county-level data from the New York Times database and collapse for each state, filling in missing dates

#Load data
usaCountyNYT = read.csv(text = getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))

db=usaCountyNYT
db=subset(db,state != "American Samoa" & state !="Guam" & state !="Northern Mariana Islands" & state!="Virgin Islands") #get rid of overseas territories

varNames=c("state", "county","fips","date","cases", "deaths") #sort columns
db = db[varNames]

# Group counties into metro areas 

msaKey=read_excel(paste0(filePath,"/Data/USAPopulation/MSAByCountyCityLab.xlsx"))
msaKey=subset(msaKey,select=c("FIPS","Popln2018","CBSACode","CBSATitle",'Top53'))
msaKey=rename(msaKey, c(FIPS="fips",Popln2018='populationCounty'))

db=merge(db,msaKey,by="fips")
db=subset(db,Top53==1)
db=db[order(db$CBSACode,db$date),]

db=aggregate(cbind(cases,deaths) ~ date + CBSATitle + Top53, db,sum)
db=subset(db,select=-c(Top53))
db$date=as.Date(db$date) # put in date data type
db$time=rep(0,nrow(db))
db$newCases=rep(0,nrow(db))
db$newDeaths=rep(0,nrow(db))

varNames=c("CBSATitle", "date", "time", "cases", "newCases", "deaths","newDeaths") #sort columns
db = db[varNames]
db=rename(db, c(CBSATitle='metro'))
db$metro=as.factor(db$metro)

# Calculate daily new cases and deaths for each metro

uniqMetro=as.character(unique(db$metro)) #unique metro
nMetro=length(uniqMetro)
db$population=rep(0,nrow(db))

for (metro in uniqMetro){
  
  print(metro)
  thisMetro = which(db$metro == metro) #indices of that metro area
  
  # get total popualation of that metro
  db[thisMetro,"population"]=sum(msaKey$populationCounty[msaKey$CBSATitle==metro], na.rm=TRUE)
  
  # first reporting day
  db[thisMetro[1],"newCases"]=db[thisMetro[1],"cases"] 
  db[thisMetro[1],"deaths"]=db[thisMetro[1],"deaths"] 
  
  #all other days
  db[thisMetro[2:length(thisMetro)],"newCases"]=db[thisMetro[2:length(thisMetro)],"cases"]-db[thisMetro[1:(length(thisMetro)-1)],"cases"]
  db[thisMetro[2:length(thisMetro)],"newDeaths"]=db[thisMetro[2:length(thisMetro)],"deaths"]-db[thisMetro[1:(length(thisMetro)-1)],"deaths"]
  
  db[thisMetro,"time"]=seq(1,length(thisMetro))
  
}

# Find and correct negative values

negNewCases=which(db$newCases < 0)

if (length(negNewCases!=0)){
  
  for (index in negNewCases){
    
    print("Negative new cases occured:")
    print(db[(index-2):(index+1),])
    
    lesserCases=which(db$cases<=db$cases[index] & db$metro==db$metro[index]) # find all dates with deaths less than or equal to the value that was corrected
    lesserCases=lesserCases[lesserCases<index] # within these dates, find the last one to occur before the messed up date
    lastCorrect=lesserCases[length(lesserCases)] #last value
    
    db$cases[lastCorrect:index]=round(seq(db$cases[lastCorrect],db$cases[index],length.out=(index-lastCorrect+1)))
    db$newCases[(lastCorrect+1):index]=db$cases[(lastCorrect+1):index]-db$cases[lastCorrect:(index-1)]
    
    print("corrected to:")
    print(db[(index-2):(index+1),])
  }
}

negNewDeaths=which(db$newDeaths < 0)

if (length(negNewDeaths!=0)){
  
  for (index in negNewDeaths){
    
    print("Negative new deaths occured:")
    print(db[(index-2):(index+1),])
    
    lesserDeaths=which(db$deaths<=db$deaths[index] & db$metro==db$metro[index]) # find all dates with deaths less than or equal to the value that was corrected
    lesserDeaths=lesserDeaths[lesserDeaths<index] # within these dates, find the last one to occur before the messed up date
    lastCorrect=lesserDeaths[length(lesserDeaths)] #last value
    
    db$deaths[lastCorrect:index]=round(seq(db$deaths[lastCorrect],db$deaths[index],length.out=(index-lastCorrect+1)))
    db$newDeaths[(lastCorrect+1):index]=db$deaths[(lastCorrect+1):index]-db$deaths[lastCorrect:(index-1)]
    
    print("corrected to:")
    print(db[(index-2):(index+1),])
  }
}

# Make long form vectors that combine cases and deaths as factors Cumulative and Daily variables

db.long.cumul=melt(subset(db,select=-c(newCases,newDeaths)),id=c("date","time","metro","population"))
db.long.daily=melt(subset(db,select=-c(cases,deaths)),id=c("date","time","metro","population"))
levels(db.long.daily$variable)=c("cases","deaths")

# -----------  Plot cases and deaths over time for each state (plotly) ----------- 

# plotMetro=uniqMetro[5]
# 
# pCum=plot_ly(data=subset(db.long.cumul,metro==plotMetro), x =~time, y=~value, color=~variable, legendgroup=~variable, type='scatter', mode='lines+markers', colors=hue_pal()(2))
# pCum=layout(pCum,xaxis=list(title="Time since first reported case (days)"),yaxis=list(title="Cumulative number",type="log"), title = plotMetro
# )
# 
# pDaily=plot_ly(data=subset(db.long.daily,metro==plotMetro), x =~time, y=~value, color=~variable, legendgroup=~variable, type='scatter', mode='lines+markers', colors=hue_pal()(2), showlegend = FALSE)
# pDaily=layout(pDaily,xaxis=list(title="Time since first reported case (days)"),yaxis=list(title="Daily new",type="log"), title = plotMetro
# )
# 
# fig=subplot(pCum,pDaily, shareX = TRUE, titleX=TRUE, shareY=FALSE, titleY=TRUE, margin = 0.05)
# fig

# ---------------------- Repeat wth regular plotting---------------------- 

# # cumulative cases & deaths
# pdf(file=paste0("plots/metros_cumulative.pdf"),width=7.5, height=10, paper="letter")
# par(mfrow=c(5,4))
# 
# for (plotMetro in uniqMetro){
#   data=subset(db.long.cumul,metro==plotMetro)
#   tvec=data$time[data$variable=='cases']
#   case_vec=data$value[data$variable=='cases']
#   death_vec=data$value[data$variable=='deaths']
#   ymax=max(ceiling(log10(max(case_vec))),1) # upper limit of y axis, max included to deal with states with no cases
#   plot(tvec, log10(case_vec), main=paste(str_wrap(plotMetro,width=22),collapse = "\n"), ylab="Log10 Cumulative #", 
#        xlab="Time since 1st case (days)", ylim=c(0, 6), pch=10, cex=0.5, col=hue_pal()(2)[1], cex.main=1)
#   points(tvec, log10(death_vec),col=hue_pal()(2)[2], pch=7,  cex=0.5)
# }
# 
# dev.off() #close pdf
# 
# # daily cases and deaths
# 
# pdf(file=paste0("plots/metros_daily.pdf"),width=7.5, height=10, paper="letter")
# par(mfrow=c(5,4))
# 
# for (plotMetro in uniqMetro){
#   data=subset(db.long.daily,metro==plotMetro)
#   tvec=data$time[data$variable=='cases']
#   case_vec=data$value[data$variable=='cases']
#   death_vec=data$value[data$variable=='deaths']
#   ymax=max(ceiling(log10(max(case_vec))),1) # upper limit of y axis, max included to deal with states with no cases
#   plot(tvec, log10(case_vec), main=paste(str_wrap(plotMetro,width=22),collapse = "\n"), ylab="Log10 Cumulative #", 
#        xlab="Time since 1st case (days)", ylim=c(0, 6), pch=10, cex=0.5, col=hue_pal()(2)[1], cex.main=1)
#   points(tvec, log10(death_vec),col=hue_pal()(2)[2], pch=7,  cex=0.5)
# }
# 
# dev.off() #close pdf

# ----------------------  Standardize times ----------------------  

# standardize all times relative to March 1, remove times before that, 
startDate="2020-03-01"
db$time=as.numeric(db$date-as.Date(startDate))
db=db[db$time>=0,]

# add in zeros for before startDate if day of 1st case was afterStartDate

for (thisMetro in uniqMetro){
  metroData=subset(db,db$metro==thisMetro)
  firstTime=metroData$time[1]
  if (firstTime>0){
    temp=as.data.frame(matrix(0,firstTime,ncol(db))) #create empty matrix to concatenate as new columns
    colnames(temp)=colnames(db)
    temp$metro=thisMetro
    temp$time=0:(firstTime-1)
    temp$date=as.Date(temp$time,origin=startDate)
    temp$population=metroData$population[1]
    db=rbind(db,temp)
  }
}

# now fix the order
db=db[order(db$metro,db$date),]

# ----------------------  Add in interventions/social distancing data ---------------------- 

# read in and bind to state social distancing data 

intData=read_excel(paste0(filePath,"/Data/Interventions/interventions.xlsx"))
intData=as.data.frame(intData) #need for some unknown reason
intData[, c(4:11)] = sapply(intData[, c(4:11)], as.numeric) # interpret dates as numbers
for (i in 4:11){ # convert to date string
  intData[, i] = as.Date(intData[, i],origin="01-01-01")
}
colnames(intData)=c(c("fips","state","county"),colnames(intData)[4:11])
intData$county=toTitleCase(intData$county)

# "combined" interventions that almost always occured at the same time
intData$banGatherings=intData$stayAtHome # combine ban50gatherings and ban500gatherings, which usually happened at very similar times and are highly correlated. Choose midpoint
intData$closeSocialRetail=intData$stayAtHome # combine closeRestoDineIn and closeGymEntertain, which usually happened at very similar times and are highly correlated. Choose midpoint
for (i in 1:nrow(intData)){
  intData$banGatherings[i]=mean.Date(c(intData$ban50gathering[i],intData$ban500gathering[i]))
  intData$closeSocialRetail[i]=mean.Date(c(intData$closeRestoDineIn[i],intData$closeGymEntertain[i]))
}

# assign an intervention to each MSA

FIPSlist=rep(0,nMetro)

for (i in 1:nMetro){
  
  thisMetro=uniqMetro[i]
  
  #find the list of FIPS codes for counties within this metro
  thisMetroInfo=msaKey[msaKey$CBSATitle==thisMetro,]
  thisMetroInfo=na.omit(thisMetroInfo)
  thisMetroInfo=subset(thisMetroInfo,select=c("fips","populationCounty"))
  
  #find intervention times for these counties
  thisMetroInt=subset(intData,fips %in% thisMetroInfo$fips)
  thisMetroInt=merge(thisMetroInt,thisMetroInfo)
  
  FIPSlist[i]=thisMetroInt$fips[which.max(thisMetroInt$populationCounty)] # get intervention times for county in MSA with biggest population

}

intData.metro=subset(intData,fips %in% FIPSlist) #chose those FIPS counties to represent inteventions for these metros
intData.metro$metro=cbind(uniqMetro)
intData.metro=subset(intData.metro,select=-c(fips,state,county))
intData.metro=intData.metro[c("metro",colnames(intData.metro[1:(ncol(intData.metro)-1)]))] #fix column order

# merge with case + death data
temp=as.data.frame(matrix(0,nrow(db),10)) #create empty matrix to concatenate as new columns
colnames(temp)=colnames(intData.metro)[2:11]

db=merge(db,temp,by="row.names", sort=TRUE)
db=subset(db,select=-c(Row.names))
db=db[order(db$metro,db$date),]

uniqInt=colnames(intData.metro)[2:11]

for (thisMetro in uniqMetro){
  for (thisInt in uniqInt){
    db[db$metro==thisMetro,thisInt]=ifelse(db[db$metro==thisMetro,"date"]> intData.metro[intData.metro$metro==thisMetro,thisInt],1,0)
  }
}

# turn all NAs into zeros. NAs occur if the state has not yet implemented that intervention as of today's date
db[is.na(db)] = 0

# ----------------------  Save files ---------------------- 

# save version with 5 interventions
intNames=c("banForeignTravel","closePublicSchools","banGatherings","closeSocialRetail","stayAtHome")
varNames=c("metro","date","time","cases", "newCases", "deaths","newDeaths")
db.save=subset(db,select=c(varNames,"population",intNames))
write.csv(db.save,"files/metro_timecourse_int5.csv", row.names = FALSE)

# save version with 4 interventions

intNames=c("closePublicSchools","banGatherings","closeSocialRetail","stayAtHome")
db.save=subset(db,select=c(varNames,"population",intNames))
write.csv(db.save,"files/metro_timecourse_int4.csv", row.names = FALSE)

# save version with 3 interventions

intNames=c("banForeignTravel","closePublicSchools","stayAtHome")
db.save=subset(db,select=c(varNames,"population",intNames))
write.csv(db.save,"files/metro_timecourse_int3.csv", row.names = FALSE)

# save version with 2 interventions only

intNames=c("closePublicSchools","stayAtHome")
db.save=subset(db,select=c(varNames,"population",intNames))
write.csv(db.save,"files/metro_timecourse_int2.csv", row.names = FALSE)
