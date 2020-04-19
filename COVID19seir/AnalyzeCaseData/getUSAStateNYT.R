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

# ----------- Load US state-level data from the New York Times database ----------- ----------- ----------- 

#Load data
usaStateNYT = read.csv(text = getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))

# add and reorder new columns
db=usaStateNYT
db=subset(db,select=-c(fips)) #get rid of FIPS code
db=subset(db,state != "American Samoa" & state !="Guam" & state !="Northern Mariana Islands" & state!="Virgin Islands") #get rid of overseas territories
db$date=as.Date(db$date) # put in date data type
db$time=rep(0,nrow(db))
db$newCases=rep(0,nrow(db))
db$newDeaths=rep(0,nrow(db))

varNames=c("state","date", "time", "cases", "newCases", "deaths","newDeaths") #sort columns
db = db[varNames]

db=db[order(db$state,db$date),]

# Calculate daily new cases and deaths for each state

uniqStates=as.character(unique(db$state)) #unique states
nStates=length(uniqStates)

for (state in uniqStates){
  
  print(state)
  thisState = which(db$state == state)
  
  # first reporting day
  db[thisState[1],"newCases"]=db[thisState[1],"cases"] 
  db[thisState[1],"deaths"]=db[thisState[1],"deaths"] 
  
  #all other days
  db[thisState[2:length(thisState)],"newCases"]=db[thisState[2:length(thisState)],"cases"]-db[thisState[1:(length(thisState)-1)],"cases"]
  db[thisState[2:length(thisState)],"newDeaths"]=db[thisState[2:length(thisState)],"deaths"]-db[thisState[1:(length(thisState)-1)],"deaths"]
  
  db[thisState,"time"]=seq(1,length(thisState))
  
}

# Find and correct negative values

negNewCases=which(db$newCases < 0)

if (length(negNewCases!=0)){
  
  for (index in negNewCases){
    
    print("Negative new cases occured:")
    print(db[(index-2):(index+1),])
    
    lesserCases=which(db$cases<=db$cases[index] & db$state==db$state[index]) # find all dates with deaths less than or equal to the value that was corrected
    lesserCases=lesserCases[lesserCases<index] # within these dates, find the last one to occur before the messed up date
    lastCorrect=lesserCases[length(lesserCases)] #last value
    
    db$cases[lastCorrect:index]=seq(db$cases[lastCorrect],db$cases[index],length.out=(index-lastCorrect+1))
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
    
    lesserDeaths=which(db$deaths<=db$deaths[index] & db$state==db$state[index]) # find all dates with deaths less than or equal to the value that was corrected
    lesserDeaths=lesserDeaths[lesserDeaths<index] # within these dates, find the last one to occur before the messed up date
    lastCorrect=lesserDeaths[length(lesserDeaths)] #last value
    
    db$deaths[lastCorrect:index]=seq(db$deaths[lastCorrect],db$deaths[index],length.out=(index-lastCorrect+1))
    db$newDeaths[(lastCorrect+1):index]=db$deaths[(lastCorrect+1):index]-db$deaths[lastCorrect:(index-1)]
    
    print("corrected to:")
    print(db[(index-2):(index+1),])
  }
}

# Make long form vectors that combine cases and deaths as factors Cumulative and Daily variables

db.long.cumul=melt(subset(db,select=-c(newCases,newDeaths)),id=c("date","time","state"))
#names(db.long.cumul)[names(db.long.cumul)==variable]=
db.long.daily=melt(subset(db,select=-c(cases,deaths)),id=c("date","time","state"))
levels(db.long.daily$variable)=c("cases","deaths")

# -----------  Plot cases and deaths over time for each state (plotly) ----------- 

# plotState="Massachusetts"
# 
# pCum=plot_ly(data=subset(db.long.cumul,state==plotState), x =~time, y=~value, color=~variable, legendgroup=~variable, type='scatter', mode='lines+markers', colors=hue_pal()(2))
# pCum=layout(pCum,xaxis=list(title="Time since first reported case (days)"),yaxis=list(title="Cumulative number",type="log"), title = plotState
# )
# 
# pDaily=plot_ly(data=subset(db.long.daily,state==plotState), x =~time, y=~value, color=~variable, legendgroup=~variable, type='scatter', mode='lines+markers', colors=hue_pal()(2), showlegend = FALSE)
# pDaily=layout(pDaily,xaxis=list(title="Time since first reported case (days)"),yaxis=list(title="Daily new",type="log"), title = plotState
# )
# 
# fig=subplot(pCum,pDaily, shareX = TRUE, titleX=TRUE, shareY=FALSE, titleY=TRUE, margin = 0.05)
# fig

#orca(fig, "state.pdf")

# Plot for all states (not good with Plotly though)

# statePlots = lapply(uniqStates, function(var) {
#   plot_ly(data=subset(db.long.daily,state==var), x =~time, y=~value, color=~variable, legendgroup=~variable, type='scatter', mode='lines+markers', colors=hue_pal()(2), showlegend = FALSE) %>%
#   layout(xaxis=list(title="Time since first reported case (days)"),yaxis=list(title="Daily new",type="log")
#   )%>%
#     add_annotations(
#       text = var,
#       x = 0.5,
#       y = 1,
#       yref = "paper",
#       xref = "paper",
#       xanchor = "middle",
#       yanchor = "top",
#       showarrow = FALSE,
#       font = list(size = 12))
# })
# 
# subplot(statePlots, nrows = 14, margin = 0.05, shareX = TRUE, titleX=TRUE, shareY=TRUE, titleY=TRUE) 

# -----------  ----------- Repeat wth regular plotting-----------  ----------- 

# # cumulative cases & deaths
# pdf(file=paste0("plots/states_cumulative.pdf"),width=7.5, height=10, paper="letter")
# par(mfrow=c(5,4))
# 
# for (plotState in uniqStates){
#   data=subset(db.long.cumul,state==plotState)
#   tvec=data$time[data$variable=='cases']
#   case_vec=data$value[data$variable=='cases']
#   death_vec=data$value[data$variable=='deaths']
#   ymax=max(ceiling(log10(max(case_vec))),1) # upper limit of y axis, max included to deal with states with no cases
#   plot(tvec, log10(case_vec), main=plotState, ylab="Log10 Cumulative #", 
#        xlab="Time since 1st case (days)", ylim=c(0, 6), pch=10, cex=0.5, col=hue_pal()(2)[1], cex.main=1)
#   points(tvec, log10(death_vec),col=hue_pal()(2)[2], pch=7,  cex=0.5)
# }
# 
# dev.off() #close pdf
# 
# # daily cases and deaths
# 
# pdf(file=paste0("plots/states_daily.pdf"),width=7.5, height=10, paper="letter")
# par(mfrow=c(5,4))
# 
# for (plotState in uniqStates){
#   data=subset(db.long.daily,state==plotState)
#   tvec=data$time[data$variable=='cases']
#   case_vec=data$value[data$variable=='cases']
#   death_vec=data$value[data$variable=='deaths']
#   ymax=max(ceiling(log10(max(case_vec))),1) # upper limit of y axis, max included to deal with states with no cases
#   plot(tvec, log10(case_vec), main=plotState, ylab="Log10 Cumulative #", 
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

for (thisState in uniqStates){
  stateData=subset(db,state==thisState)
  firstTime=stateData$time[1]
  if (firstTime>0){
    temp=as.data.frame(matrix(0,firstTime,ncol(db))) #create empty matrix to concatenate as new columns
    colnames(temp)=colnames(db)
    temp$state=thisState
    temp$time=0:(firstTime-1)
    temp$date=as.Date(temp$time,origin=startDate)
    db=rbind(db,temp)
  }
}

# now fix the order
db=db[order(db$state,db$date),]

# ----------------------  Add in population data ---------------------- 

# read in and bind to state population data

popData=read_excel(paste0(filePath,"/Data/USAPopulation/USAPopByState.xlsx"))
popData=subset(popData,STATE!=0,select=c("NAME","POPESTIMATE2019"))
colnames(popData)=c("state","population")
db=merge(db,popData)


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

## select out state .. FIPS is a multiple of 1000
intData.state=subset(intData,fips %% 1000 == 0)
colnames(intData.state)=c(c("fips","stateAbbrev","state"),colnames(intData.state)[4:13])

# merge with case + death data
temp=as.data.frame(matrix(0,nrow(db),10)) #create empty matrix to concatenate as new columns
colnames(temp)=colnames(intData.state)[4:13]

db=merge(db,temp,by="row.names", sort=TRUE)
db$Row.names=as.numeric(db$Row.names)
db=db[order(db$Row.names),]
db=subset(db,select=-c(Row.names))

uniqInt=colnames(intData.state)[4:13]

for (thisState in uniqStates){
  for (thisInt in uniqInt){
    db[db$state==thisState,thisInt]=ifelse(db[db$state==thisState,"date"]> intData.state[intData.state$state==thisState,thisInt],1,0)
  }
}

# turn all NAs into zeros. NAs occur if the state has not yet implemented that intervention as of today's date
db[is.na(db)] = 0

# ----------------------  Save files ---------------------- 

# save version with 5 interventions
intNames=c("banForeignTravel","closePublicSchools","banGatherings","closeSocialRetail","stayAtHome")
db.save=subset(db,select=c(varNames,"population",intNames))
write.csv(db.save,"files/states_timecourse_int5.csv", row.names = FALSE)

# save version with 4 interventions

intNames=c("closePublicSchools","banGatherings","closeSocialRetail","stayAtHome")
db.save=subset(db,select=c(varNames,"population",intNames))
write.csv(db.save,"files/states_timecourse_int4.csv", row.names = FALSE)

# save version with 3 interventions

intNames=c("banForeignTravel","closePublicSchools","stayAtHome")
db.save=subset(db,select=c(varNames,"population",intNames))
write.csv(db.save,"files/states_timecourse_int3.csv", row.names = FALSE)

# save version with 2 interventions only

intNames=c("closePublicSchools","stayAtHome")
db.save=subset(db,select=c(varNames,"population",intNames))
write.csv(db.save,"files/states_timecourse_int2.csv", row.names = FALSE)



# Write functions to fit exponential in different ways

# Write function to do breakpoint analysis

# Repeate this for MSA data
