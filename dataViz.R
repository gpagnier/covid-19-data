#### Installing packages ####
install.packages('tidyverse')
install.packages('brms')
install.packages('lme4')
install.packages('optimx')
install.packages('shiny')
install.packages('installr')
install.packages('pracma')

library(tidyverse)
library(reshape2)
library(brms)
library(lme4)
library(optimx)
library(shiny)
library(installr)
library(pracma)

#Changing some R defaults
## Base R parameters and loading custom functions ####

options(scipen=999)
source("C:/Users/Guillaume/Dropbox (Brown)/miscRFunctions/softmax.R")
d0=read.csv("/Users/Guillaume/Documents/GitHub/covid-19-data/us-counties.csv")
d0$date<-sub("2020-","",d0$date)
##Declaring simple county or state visualziation function ####
#Can plot by county (default) or state
plotByTime<-function(countyInput,stateInput){
  #This is for entire states
  if(missing(countyInput)&& !missing(stateInput)){
    dTemp<-filter(d0,state==stateInput)
    dTemp$date<-sub("-","/",dTemp$date)
    dTemp$date<-sub("0","",dTemp$date)
    if (nrow(dTemp)==0){
      print("That's not a state")
      stop()
    }
      
    dTemp <- dTemp %>%
      group_by(date)%>%
      summarize(deaths=sum(deaths),
                cases=sum(cases)) %>% 
      pivot_longer(cols=c(deaths,cases), names_to="type", values_to="vals")
    dTemp<-dTemp[(nrow(dTemp)/2):nrow(dTemp),]
    ggplot(dTemp,aes(x=factor(date),y=vals,col=as.factor(type)))+
      geom_point(
        size=1.75
      )+ylim(0,(max(dTemp$vals)+3000))+ylab("")+xlab("Date")+ggtitle(paste(c(stateInput," cases and deaths")))
    #print('oneCond')
    #THis is there's an error somewhere
  }else if(missing(countyInput) && missing(stateInput)){
    dTemp=d0
    dTemp$date<-sub("-","/",dTemp$date)
    dTemp$date<-sub("0","",dTemp$date)
    dTemp <- dTemp %>%
      group_by(date)%>%
      summarize(deaths=sum(deaths),
                cases=sum(cases)) %>% 
      pivot_longer(cols=c(deaths,cases), names_to="type", values_to="vals")
    dTemp<-dTemp[(nrow(dTemp)/2):nrow(dTemp),]
    ggplot(dTemp,aes(x=factor(date),y=vals,col=as.factor(type)))+
      geom_point(
        size=1.75
      )+ylim(0,(max(dTemp$vals)+3000))+ylab("")+xlab("Date")+ggtitle("US cases and deaths")
  }else{
    #This is for when there's a county and state
    dTemp<-filter(d0,state==stateInput, county==countyInput)
    dTemp$date<-sub("-","/",dTemp$date)
    dTemp$date<-sub("0","",dTemp$date)
    if (nrow(dTemp)==0){
      print("Typo somewhere")
      stop()
    }
    
    dTemp <- dTemp %>%
      group_by(date)%>%
      summarize(deaths=sum(deaths),
                cases=sum(cases)) %>% 
      pivot_longer(cols=c(deaths,cases), names_to="type", values_to="vals")
    dTemp<-dTemp[(nrow(dTemp)/2):nrow(dTemp),]
    ggplot(dTemp,aes(x=factor(date),y=vals,col=as.factor(type)))+
      geom_point(
        size=1.75
      )+ylim(0,(max(dTemp$vals)+3000))+ylab("")+xlab("Date")+ggtitle(paste(countyInput,stateInput))
  }
}

plotByTime()

plotByTime(stateInput="Rhode Island")
plotByTime(countyInput="Norfolk",stateInput="Massachusetts")

plotByTime(countyInput="Providence",stateInput="Rhode Island")
