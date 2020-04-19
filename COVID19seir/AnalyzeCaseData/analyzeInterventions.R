library(plotly)
library(dplyr)
library(reshape)
library(scales)
library(readxl)
library(httr)
library(RCurl)
library(tools)

intData=read_excel(paste0(filePath,"/Data/Interventions/interventions.xlsx"))

intData=as.data.frame(intData) #need for some unknown reason
intData[, c(4:11)] = sapply(intData[, c(4:11)], as.numeric)
colnames(intData)=c(c("fips","state","county"),colnames(intData)[4:11])
intData$county=toTitleCase(intData$county)

intData.long=melt(intData,id=c("fips","state","county"))
colnames(intData.long)=c(c("fips","state","county","intervene","startDateNum"))
intData.long$startDate=as.Date(intData.long$startDateNum,origin="01-01-01")
intData.long=as.data.frame(intData.long)

# ----------------- Plot distributions of times interventions were implemented at county/state level -----------

p=plot_ly(data=intData.long, x =~startDate, color=~intervene, type='histogram', opacity=0.7)
p=layout(p,xaxis=list(title="Date of implementation"),yaxis=list(title="# of counties"),barmode = "overlay")
p

intPlots = lapply(levels(intData.long$intervene), function(var) {
  plot_ly(data=subset(intData.long,intervene==var), x =~startDate, color=~intervene, type='histogram', bingroup =~intervene) %>%
    layout(xaxis=list(title="Date of implementation",range=as.Date(c("2020-03-01", "2020-04-15"))),yaxis=list(title="# of counties"))
})

subplot(intPlots, nrows = 3, margin = 0.05, titleY=TRUE)


# Correlelation plots at county level

uniqInt=levels(intData.long$intervene)
nInt=length(uniqInt)

dayZero=as.Date("2020-03-01")-as.Date("0001-01-01")

pdf(file=paste0("plots/interventions_counties_correl.pdf"),width=30, height=30)
par(mfrow=c(8, 8))

for (var1 in uniqInt){
  for(var2 in uniqInt){
    x=as.numeric(intData[,var1]-dayZero)
    y=as.numeric(intData[,var2]-dayZero)
    diff=mean(y-x,na.rm=TRUE)
    x=x + rnorm(length(x), sd=0.2)
    y=y + rnorm(length(y), sd=0.2)
    corResult=cor.test(x,y, na.action=na.exclude)
    plot(x, y, pch=19, xlab=var1, ylab = var2, cex=1, cex.axis=1.5, cex.lab=1.5, ylim=c(0, 40), xlim=c(0, 40), col=alpha(hue_pal()(3)[3],0.1))
    lines(seq(1,40),seq(1,40),lty=2, col="black")
    text(1, 38, paste0("r = ", formatC(corResult$estimate,digits=2,format = "f"),", p = ",formatC(corResult$p.value, format = "g", digits = 1),", dt = ",formatC(diff,digits=1, format = "f")),cex = 1.5, pos=4)
  }
}

dev.off()

# ----------------- Produce reduced list of interventions  -----------
intData.reduced=intData
intData.reduced$banGatherings = round((intData.reduced$ban50gathering+intData.reduced$ban500gathering)/2)
intData.reduced$closeSocialRetail = round((intData.reduced$closeRestoDineIn+intData.reduced$closeGymEntertain)/2)
intData.reduced=subset(intData.reduced, select=c("fips","state","county","banForeignTravel","closePublicSchools","banGatherings","closeSocialRetail","stayAtHome"))

# save file with 5 interventions

intData.save=intData.reduced

# save file with 4 interventions

intData.save=subset(intData.reduced, select=-c("banForeignTravel"))

# save file with 3 interventions
intData.save=subset(intData.reduced, select=c("fips","state","county","banForeignTravel","closePublicSchools","stayAtHome"))

# save file with 2 interventions
intData.save=subset(intData.reduced, select=c("fips","state","county","closePublicSchools","stayAtHome"))
