#UPDATE: Includes pre-symptomatic and asymptomatic transmission

library(deSolve)
library(plotly)
library(dplyr)
library(reshape)
library(htmltools)
source("code/functionsAP.R")

# Set parameters
# (in Shiny app these are input by sliders)

IncubPeriod= 5 #Duration of incubation period
DurMildInf= 7 #Duration of mild infections
FracSevere= 15 #% of symptomatic infections that are severe
FracCritical= 5 #% of symptomatic infections that are critical
ProbDeath= 40 #Death rate for critical infections
DurHosp= 5 #Duration of severe infection/hospitalization
TimeICUDeath= 8 #Duration critical infection/ICU stay

FracAsym=25 #Fraction of all infections that are asymptomatic
PresymPeriod=2 #Length of infectious phase of incubation period
DurAsym=7 #Duration of asympatomatic infection

b1= 0.3 #Transmission rate (mild infections)
b21= 0.25 #Transmission rate (severe infections, relative to mild)
b31= 0.25 #Transmission rate (critical infections, relative to mild)
be1 = 1 #Transmission rate (pre-symptomatic, relative to mild)
b01 = 1 #Transmission rate (asymptomatic infections, relative to mild)
LogN= 3 #Total population size (log10)
Tmax= 300 #Initial # infected
InitInf= 1 #Maximum time
yscale="linear"

#Put these into an input structure
input=list("IncubPeriod"=IncubPeriod,"DurMildInf"=DurMildInf,"FracSevere"=FracSevere,"FracCritical"=FracCritical,"ProbDeath"=ProbDeath,"DurHosp"=DurHosp,"TimeICUDeath"=TimeICUDeath,"FracAsym"=FracAsym, "PresymPeriod"=PresymPeriod, "DurAsym"=DurAsym, "be1"=be1,"b01"=b01, "b1"=b1,"b21"=b21,"b31"=b31,"LogN"=LogN,"Tmax"=Tmax,"InitInf"=InitInf,"yscale"=yscale)

CFR=(input$ProbDeath/100)*(input$FracCritical) #Case fatality rate

# Run simulations

sim=SimSEIR_AP(input)

out.df=sim$out.df
N=sim$N
Ro=sim$Ro
r=sim$r
DoublingTime=sim$DoublingTime

#combine exposed classes for plotting
out.df$E0=out.df$E0+out.df$E1
out.df$E1=NULL
out.df=rename(out.df, c(E0="E"))

#reformat data for plotting
out.df2=rename(out.df, c(S="Susceptible", E="Exposed", I0="Infected.Asymptomatic", I1="Infected.Mild", I2="Infected.Severe", I3="Infected.Critical", R="Recovered", D="Dead"))
out=melt(out.df,id="time")
out2=melt(out.df2,id="time")
out$variableName=out2$variable
out$variableLegend = paste0(out$variableName,' (',out$variable,')')
out$variableLegend = factor(out$variableLegend, levels = unique(out[["variableLegend"]]))

#plot
p=plot_ly(data = out, x=~time, y=~value, color=~variableLegend, type='scatter', mode='lines')

p=layout(p,xaxis=list(title="Time since introduction (days)"),yaxis=list(title=paste("Number per",formatC(N,big.mark=",",format="f",digits=0),"people"),type=input$yscale),
         annotations=list(text=HTML(paste("R", tags$sub(0),'=',formatC(Ro,digits=2)," <br>r =", formatC(r,digits=2)," per day <br>T",tags$sub(2)," = ",formatC(DoublingTime,digits=2)," days")),
                          showarrow=FALSE,xref="paper",xanchor="left",x=1.05, yref="paper", yanchor="center",y=0.4, align="left"))
p
