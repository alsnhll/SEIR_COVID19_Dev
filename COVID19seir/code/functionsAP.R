#UPDATE: Includes pre-symptomatic and asymptomatic transmission

# This file defines all the functions that are used in the simulation

# ----------------------------------------------------------------------------
# SetODEs_SEIR_AP function:
# -----------------------
# Defines the system of differential equations describing the SEIR model, including asymptomatic and pre-symptomatic infection 
# INPUT: p - named list of parameter values
# OUTPUT: list of derivatives of each variable

SetODEs_SEIR_AP=function(t,y,p){
  S = y[1]
  E0 = y[2]
  E1 = y[3]
  I0 = y[4]
  I1 = y[5]
  I2 = y[6]
  I3 = y[7]
  R = y[8]
  D = y[9]
  
  with(as.list(p),{
    
    dS.dt = -(be*E1+b0*I0+b1*I1+b2*I2+b3*I3)*S
    dE0.dt=(be*E1+b0*I0+b1*I1+b2*I2+b3*I3)*S-a0*E0
    dE1.dt=a0*E0-a1*E1
    dI0.dt=f*a1*E1-g0*I0
    dI1.dt=(1-f)*a1*E1-g1*I1-p1*I1
    dI2.dt=p1*I1-g2*I2-p2*I2
    dI3.dt=p2*I2-g3*I3-u*I3
    dR.dt=g0*I0+g1*I1+g2*I2+g3*I3
    dD.dt=u*I3
    
    return(list(c(dS.dt, dE0.dt, dE1.dt,dI0.dt,dI1.dt, dI2.dt, dI3.dt, dR.dt, dD.dt)))
  })
}


# ----------------------------------------------------------------------------
# GetSpread_SEIR_AP function:
# --------------------
# This function numerically intergrates the system of differential equations for a given set of parameter values, initial conditions, and maximum time,  for model including asymptomatic and pre-symptomatic infection 
# INPUT: p- named list of parameter values
#        Tmax - max time to integrate for
#        y0 - named list of initial conditions for each variable
# OUTPUT: Dataframe with rows as timepoints and columns as variables

GetSpread_SEIR_AP = function(p,Tmax,y0){
  
  t = seq(from=0, to=Tmax, by=1)
  
  out = ode(y=y0, times=t, func=SetODEs_SEIR_AP, parms=p)
  
  df = as.data.frame(out)
  
  return(df)
}


# ----------------------------------------------------------------------------
# GetModelParamsAP function:
# --------------------
# Function to take the parameters entered by the user and turn them into the rate parameters used by the model,  for model including asymptomatic and pre-symptomatic infection 
# INPUT: input - structure containing all the user entered information
# OUTPUT: named list consisting of the population size N and another list of the model parameters, pModel

GetModelParamsAP = function(input){
  
  IncubPeriod=input$IncubPeriod  #Incubation period, days
  DurMildInf=input$DurMildInf #Duration of mild infections, days
  FracSevere=input$FracSevere/100 #Fraction of infections that are severe
  FracCritical=input$FracCritical/100 #Fraction of infections that are critical
  FracMild=1-FracSevere-FracCritical  #Fraction of infections that are mild
  ProbDeath=input$ProbDeath  #Probability of dying given critical infection
  CFR=ProbDeath*FracCritical/100 #Case fatality rate (fraction of infections resulting in death)
  TimeICUDeath=input$TimeICUDeath #Time from ICU admission to death, days
  DurHosp=input$DurHosp #Duration of hospitalization, days
  
  FracAsym=input$FracAsym/100 #Fraction of all infections that are asymptomatic
  PresymPeriod=input$PresymPeriod #Length of infections phase of incubation period
  DurAsym=input$DurAsym #Duration of asympatomatic infection

  pClin=c(IncubPeriod=IncubPeriod, DurMildInf=DurMildInf,FracMild=FracMild, FracSevere=FracSevere,FracCritical=FracCritical,CFR=CFR,TimeICUDeath=TimeICUDeath,DurHosp=DurHosp,FracAsym=FracAsym,  PresymPeriod=PresymPeriod,DurAsym=DurAsym)

  # Turn these clinical parameters into the rate constants of the model
  pModel=GetParams_SEIR_AP(pClin)
  
  N=10^(input$LogN)
  
  # The transmission rates are changed from values per time to values per capita per time
  b1=input$b1/N
  b2=input$b21*b1
  b3=input$b31*b1
  be=input$be1*b1
  b0=input$b01*b1
  pModel=c(be=be,b0=b0,b1=b1,b2=b2,b3=b3,pModel)

  return(list("N"=N,"pModel"=pModel))
  
}

# ----------------------------------------------------------------------------
# GetParams_SEIR_AP function:
# --------------------
# Function to relate the clinical parameters entered by the user into the rate parameters used by the model,for model including asymptomatic and pre-symptomatic infection 
# INPUT: pClin - named list of the clinical parameters
# OUTPUT: named list of the model rate parameters, excluding the Betas

GetParams_SEIR_AP = function(pClin){
  
  with(as.list(pClin),{
    
    a1=1/PresymPeriod
    a0=(IncubPeriod-PresymPeriod)^(-1)
    
    f=FracAsym
    
    g0=(1/DurAsym)
    
    g1=(1/DurMildInf)*FracMild
    p1=(1/DurMildInf)-g1
    
    p2=(1/DurHosp)*(FracCritical/(FracSevere+FracCritical))
    g2=(1/DurHosp)-p2
    
    if(FracCritical==0){
      u=0
    }else{
      u=(1/TimeICUDeath)*(CFR/FracCritical)
    }
    
    g3=(1/TimeICUDeath)-u
    
    return(c(a0=a0,a1=a1,f=f,g0=g0,g1=g1,g2=g2,g3=g3,p1=p1,p2=p2,u=u))
  })
  
}

# ----------------------------------------------------------------------------
# GetRo_SEIR_AP function:
# --------------------
# Function to calculate the basic reporductive ratio (Ro) for the model including asymptomatic and pre-symptomatic infection 
# INPUT: p - named list of the clinical parameters
#        N - total population size
# OUTPUT: Ro

GetRo_SEIR_AP = function(p,N){
  
  with(as.list(p),{
    
    Ro=N*((be/a1)+f*(b0/g0)+(1-f)*((b1/(p1+g1))+(p1/(p1+g1))*(b2/(p2+g2)+ (p2/(p2+g2))*(b3/(u+g3)))))
    
    return(Ro)
  })
  
}


Getr_SEIR = function(out.df,V){
  
  tpeak=out.df[which.max(select(out.df,"time",V)[,2]),"time"]; # find location of peak infection
  
  t2=tpeak/2 # choose timepoints long before peak infection
  t1=tpeak/4
  
  outV=subset(out.df, select=c("time",V))
  colnames(outV)=c("time","value")
  value1=outV$value[which.min(abs(t1-outV$time))]
  value2=outV$value[which.min(abs(t2-outV$time))]
  r=(log(value2)-log(value1))/(t2-t1)
  
  DoublingTime=log(2)/r
  
  return(list("r"=r,"DoublingTime"=DoublingTime))
  
}


# ----------------------------------------------------------------------------
# SetHospitalCapacity function:
# --------------------
# Function to determine the capacity for hospital beds and ICU beds based on total beds and availability, and to # get ventilator capacity
# INPUT: input - structure containing all the user entered information
# OUTPUT: named list consisting of all the healthcare capacity parameters

SetHospCapacity=function(input){
  
  AvailHospBeds=input$HospBedper*(100-input$HospBedOcc*(1+input$IncFluOcc/100))/100 #Available hospital beds per 1000 ppl in US based on total beds and occupancy
  AvailICUBeds=input$ICUBedper*(100-input$ICUBedOcc*(1+input$IncFluOcc/100))/100 #Available ICU beds per 1000 ppl in US, based on total beds and occupancy. Only counts adult not neonatal/pediatric beds
  ConvVentCap=input$ConvMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using conventional protocols
  ContVentCap=input$ContMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using contingency protocols
  CrisisVentCap=input$CrisisMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using crisis protocols
  
  capParams=c("AvailHospBeds"=AvailHospBeds,"AvailICUBeds"=AvailICUBeds,"ConvVentCap"=ConvVentCap,"ContVentCap"=ContVentCap,"CrisisVentCap"=CrisisVentCap)
  
  return(capParams)
}

# ----------------------------------------------------------------------------
# SimSEIR_AP function:
# --------------------
# Function to simulate the spread of infection using the model including asymptomatic and pre-symptomatic infection 
# INPUT: input - structure containing all the user entered information
# OUTPUT: named list consisting of df - wide format of the timecourse of each variable, N, Ro, r, and doubling time

SimSEIR_AP = function(input){
  
  ParamStruct=GetModelParamsAP(input)
  pModel=ParamStruct$pModel
  N=ParamStruct$N
  Tmax=input$Tmax
  
  # Set initial conditions and time interval
  E00=input$InitInf
  S0 = N-E00
  y0 = c(S=S0, E0=E00,  E1=0, I0=0, I1=0, I2=0, I3=0, R=0, D=0)
  
  #get Ro value
  Ro=GetRo_SEIR_AP(pModel,N)
  
  #run ODEs
  out.df=GetSpread_SEIR_AP(pModel,Tmax,y0)
  
  #get r value
  V="I1" #variable to calculate r for
  
  r.out=Getr_SEIR(out.df,V)
  r=r.out$r
  DoublingTime=r.out$DoublingTime
  
  return(list("out.df"=out.df,"N"=N,"Ro"=Ro,"r"=r,"DoublingTime"=DoublingTime))
  
}

# ----------------------------------------------------------------------------
# SimSEIRintB_AP function:
# --------------------
# Function to simulate the spread of infection using the model, including asymptomatic and pre-symptomatic infection, when an intervention to reduce Beta is implemented
# INPUT: input - structure containing all the user entered information
# OUTPUT: named list consisting of df - wide format of the timecourse of each variable, N, Ro, r, and doubling time

SimSEIRintB_AP = function(input){
  
  ParamStruct=GetModelParamsAP(input)
  pModel=ParamStruct$pModel
  N=ParamStruct$N
  Tmax=input$Tmax
  
  # start/end time of intervention
  Tint=input$Tint
  Tend=input$Tend
  
  if(Tint==Tend){ # If the intervention starts and ends at the same time, just return baseline values
    
    # Set initial conditions and time interval
    E00=input$InitInf
    S0 = N-E00
    y0 = c(S=S0, E0=E00,  E1=0, I0=0, I1=0, I2=0, I3=0, R=0, D=0)
    
    #run ODEs
    outInt.df=GetSpread_SEIR_AP(pModel,Tmax,y0)
    
  }else{
    
    # First simulate model without intervention, if Tint>0
    
    if(Tint>0){
      
      E00=input$InitInf
      S0 = N-E00
      y0 = c(S=S0, E0=E00,  E1=0, I0=0, I1=0, I2=0, I3=0, R=0, D=0)
      out.df=GetSpread_SEIR_AP(pModel,Tint,y0)
      
      # Set initial conditions and time interval
      iInt=nrow(out.df)
      S0 = out.df[iInt,"S"]
      E00 = out.df[iInt,"E0"]
      E10 = out.df[iInt,"E1"]
      I00 = out.df[iInt,"I0"]
      I10 = out.df[iInt,"I1"]
      I20 = out.df[iInt,"I2"]
      I30 = out.df[iInt,"I3"]
      D0 = out.df[iInt,"D"]
      R0 = out.df[iInt,"R"]
      y0 = c(S=S0, E0=E00, E1=I10, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      
    }else{
      
      E00=input$InitInf
      S0 = N-E00
      y0 = c(S=S0, E0=E00,  E1=0, I0=0, I1=0, I2=0, I3=0, R=0, D=0)
      
    }
    
    # intervention parameters
    pModelInt=pModel
    ModelInt["be"]=pModelInt["be"]*(1-input$se/100)
    pModelInt["b0"]=pModelInt["b0"]*(1-input$s0/100)
    pModelInt["b1"]=pModelInt["b1"]*(1-input$s1/100)
    pModelInt["b2"]=pModelInt["b2"]*(1-input$s2/100)
    pModelInt["b3"]=pModelInt["b3"]*(1-input$s3/100)
    
    # intervention Ro
    RoInt=GetRo_SEIR_AP(pModelInt,N)
    
    #Run intervention time course until Tend. Up to time Tint, use baseline solution
    Trun=Tend-Tint
    
    outInt.df=GetSpread_SEIR_AP(pModelInt,Trun,y0)
    outInt.df$time=outInt.df$time+Tint
    
    # combine data from before and after intervention, if the intervention didn't start right away
    
    if(Tint>0){
      outInt.df=rbind(out.df,outInt.df)
    }
    
    #--After intervention ends, run with regular parameters up to time Tmax
    
    Trun2=Tmax-Tend
    
    if(Trun2==0){
      
    }else{
      #Set initial conditions and time interval
      #Round all numbers to lowest intergar, so if less than 1, go to zero
      iEnd=nrow(outInt.df)
      
      if(input$RoundOne=="True"){
        S0 = round(outInt.df[iEnd,"S"])
        E00 = round(outInt.df[iEnd,"E0"])
        E10 = round(outInt.df[iEnd,"E1"])
        I00 = round(outInt.df[iEnd,"I0"])
        I10 = round(outInt.df[iEnd,"I1"])
        I20 = round(outInt.df[iEnd,"I2"])
        I30 = round(outInt.df[iEnd,"I3"])
        D0 = round(outInt.df[iEnd,"D"])
        R0 = round(outInt.df[iEnd,"R"])
      }else{
        S0 = outInt.df[iEnd,"S"]
        E00 = outInt.df[iEnd,"E0"]
        E10 = outInt.df[iEnd,"E1"]
        I00 = outInt.df[iEnd,"I0"]
        I10 = outInt.df[iEnd,"I1"]
        I20 = outInt.df[iEnd,"I2"]
        I30 = outInt.df[iEnd,"I3"]
        D0 = outInt.df[iEnd,"D"]
        R0 = outInt.df[iEnd,"R"]
      }
      
      y0 = c(S=S0, E0=E00, E1=E10, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      
      #run with parameters back to baseline

      outIntOff.df=GetSpread_SEIR_AP(pModel,Trun2,y0)
      outIntOff.df$time=outIntOff.df$time+Tend
      
      #combine data
      outInt.df=rbind(outInt.df,outIntOff.df)
    }
    
  }
  
  #get r value for intervention
  #note, to do this need to simulate from beginning with intervention parameters
  
  E00=input$InitInf
  S0 = N-E00
  y0 = c(S=S0, E0=E00,  E1=0, I0=0, I1=0, I2=0, I3=0, R=0, D=0)
  outIntZero.df=GetSpread_SEIR_AP(pModelInt,Tmax,y0)
  outIntZero=melt(outIntZero.df,id="time")
  
  V="I1" #variable to calculate r for
  r.out=Getr_SEIR(outIntZero.df,V)
  rInt=r.out$r
  DoublingTimeInt=r.out$DoublingTime
  
  return(list("out.df"=outInt.df,"N"=N,"Ro"=RoInt,"r"=rInt,"DoublingTime"=DoublingTimeInt))
  
}



