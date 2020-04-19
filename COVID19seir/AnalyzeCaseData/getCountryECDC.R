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

# ----------- Load country-level data from the ECDC database --------------------------------- 


ecdcData = read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

# add and reorder new columns
db=ecdcData
db=subset(db,select=-c(day,month,year,geoId,countryterritoryCode)) #get rid of some variables

colnames(db)=c("date","newCases","newDeaths","country","population")

db$date=as.Date(db$date,"%d/%m/%Y") # put in date data type
db$time=rep(0,nrow(db))
db$cases=rep(0,nrow(db))
db$deaths=rep(0,nrow(db))

varNames=c("country","date", "time", "cases", "newCases", "deaths","newDeaths","population") #sort columns
db = db[varNames]

countryListUse=c("Austria","Belgium","Denmark","France","Germany","Italy","Norway","Spain","Sweden","United_Kingdom")
#EU contries I want to use, select only these
db=subset(db,country %in% countryListUse)
db=db[order(db$country,db$date),]

# Calculate total cumulative cases and deaths for each country

uniqCountry=as.character(unique(db$country)) #unique countrys
nCountry=length(uniqCountry)

for (country in uniqCountry){
  
  print(country)
  thisCountry = which(db$country == country)
  
  # first reporting day
  db[thisCountry[1],"cases"]=db[thisCountry[1],"newCases"] 
  db[thisCountry[1],"deaths"]=db[thisCountry[1],"newDeaths"] 
  
  #all other days
  for (i in 2:length(thisCountry)){
    db[thisCountry[i],"cases"]=db[thisCountry[i],"newCases"]+db[thisCountry[i]-1,"cases"]
    db[thisCountry[i],"deaths"]=db[thisCountry[i],"newDeaths"]+db[thisCountry[i]-1,"deaths"]
  }
  
  db[thisCountry,"time"]=seq(1,length(thisCountry))
  
}

# Make long form vectors that combine cases and deaths as factors Cumulative and Daily variables

db.long.cumul=melt(subset(db,select=-c(newCases,newDeaths)),id=c("date","time","country","population"))
#names(db.long.cumul)[names(db.long.cumul)==variable]=
db.long.daily=melt(subset(db,select=-c(cases,deaths)),id=c("date","time","country","population"))
levels(db.long.daily$variable)=c("cases","deaths")

# -----------  Plot cases and deaths over time for each country (plotly) ----------- 

plotCountry="Italy"

pCum=plot_ly(data=subset(db.long.cumul,country==plotCountry), x =~time, y=~value, color=~variable, legendgroup=~variable, type='scatter', mode='lines+markers', colors=hue_pal()(2))
pCum=layout(pCum,xaxis=list(title="Time since first reported case (days)"),yaxis=list(title="Cumulative number",type="log"), title = plotCountry
)

pDaily=plot_ly(data=subset(db.long.daily,country==plotCountry), x =~time, y=~value, color=~variable, legendgroup=~variable, type='scatter', mode='lines+markers', colors=hue_pal()(2), showlegend = FALSE)
pDaily=layout(pDaily,xaxis=list(title="Time since first reported case (days)"),yaxis=list(title="Daily new",type="log"), title = plotCountry
)

fig=subplot(pCum,pDaily, shareX = TRUE, titleX=TRUE, shareY=FALSE, titleY=TRUE, margin = 0.05)
fig

# ----------------------- Repeat wth regular plotting------------------------ 

# cumulative cases & deaths
pdf(file=paste0("plots/countries_cumulative.pdf"),width=7.5, height=10, paper="letter")
par(mfrow=c(4,3))

for (plotCountry in uniqCountry){
  data=subset(db.long.cumul,country==plotCountry)
  tvec=data$time[data$variable=='cases']
  case_vec=data$value[data$variable=='cases']
  death_vec=data$value[data$variable=='deaths']
  ymax=max(ceiling(log10(max(case_vec))),1) # upper limit of y axis, max included to deal with countrys with no cases
  plot(tvec, log10(case_vec), main=plotCountry, ylab="Log10 Cumulative #",
       xlab="Time since Jan 1 (days)", ylim=c(0, 6), pch=10, cex=0.5, col=hue_pal()(2)[1], cex.main=1)
  points(tvec, log10(death_vec),col=hue_pal()(2)[2], pch=7,  cex=0.5)
}

dev.off() #close pdf

# daily cases and deaths

pdf(file=paste0("plots/countries_daily.pdf"),width=7.5, height=10, paper="letter")
par(mfrow=c(4,3))

for (plotCountry in uniqCountry){
  data=subset(db.long.daily,country==plotCountry)
  tvec=data$time[data$variable=='cases']
  case_vec=data$value[data$variable=='cases']
  death_vec=data$value[data$variable=='deaths']
  ymax=max(ceiling(log10(max(case_vec))),1) # upper limit of y axis, max included to deal with countrys with no cases
  plot(tvec, log10(case_vec), main=plotCountry, ylab="Log10 Cumulative #",
       xlab="Time since Jan 1 (days)", ylim=c(0, 6), pch=10, cex=0.5, col=hue_pal()(2)[1], cex.main=1)
  points(tvec, log10(death_vec),col=hue_pal()(2)[2], pch=7,  cex=0.5)
}

dev.off() #close pdf

# ----------------------  Standardize times ----------------------  

# standardize all times relative to Feb 15, remove times before that, 
startDate="2020-02-15"
db$time=as.numeric(db$date-as.Date(startDate))
db=db[db$time>=0,]

# ----------------------  Add in interventions/social distancing data ---------------------- 

# read in and bind to state social distancing data 

intData=read_excel(paste0(filePath,"/Data/Interventions/interventions_europe.xlsx"))
intData=as.data.frame(intData) #need for some unknown reason

uniqInt=colnames(intData)[2:ncol(intData)]
nInt=length(uniqInt)

for (thisInt in uniqInt){
  intData[,thisInt] = as.Date(intData[,thisInt],"%d/%m/%Y") # interpret as dates
}

# merge with case + death data
temp=as.data.frame(matrix(0,nrow(db),nInt)) #create empty matrix to concatenate as new columns
colnames(temp)=uniqInt


db=cbind(db,temp)

for (thisCountry in uniqCountry){
  for (thisInt in uniqInt){
    db[db$country==thisCountry,thisInt]=ifelse(db[db$country==thisCountry,"date"]> intData[intData$country==thisCountry,thisInt],1,0)
  }
}

# turn all NAs into zeros. NAs occur if the state has not yet implemented that intervention as of today's date
db[is.na(db)] = 0

# ----------------------  Save files ---------------------- 

# save version with 5 interventions
intNames=c("closeSchool","banEvent","lockdown","socialDistancing","caseBased")
db.save=subset(db,select=c(varNames,"population",intNames))
write.csv(db.save,"files/euro_timecourse_int5.csv", row.names = FALSE)

# save version with 2 interventions only

intNames=c("closeSchool","lockdown")
db.save=subset(db,select=c(varNames,"population",intNames))
write.csv(db.save,"files/euro_timecourse_int2.csv", row.names = FALSE)

