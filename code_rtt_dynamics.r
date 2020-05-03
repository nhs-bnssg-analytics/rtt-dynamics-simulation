# name: discrete-time simulation for modelling rtt dynamics 
# version: v1.1
# date: may 2020
# contact: richard.wood16@nhs.net
# note: data included for illustration

rm(list=ls())
library(truncdist)
library(parallel)
library(dplyr)
library(ggplot2)
library(tidyr)

######################################################################################################################################################
### INPUTS ###

clock.starts<-rep(0.8*54784,200+365)  # vector containing num clock starts for each day in considered period
clock.stops<-rep(54784,200+365)   # vector containing max num clock stops (capacity) for each day in considered period
priority.prob<-c(0.33,0.33,0.34)  # probability of being in considered priority classes (sampled on referral)
priority.points<-c(0,92,190)      # initial points awarded to referrals belonging to each priority class
initial.waiting.list<-4425306     # initial number of incomplete referrals (i.e. with no clock stop)
initial.performance<-0.832        # initial performance (to target)

n.runs<-100
scaler<-1000
warm.up.period<-200

######################################################################################################################################################
### SIMULATION ###

simfn<-function(SEED) {
  set.seed(SEED)
  #initial conditions
  initial.waiting.list<-initial.waiting.list/scaler
  clock.starts<-clock.starts/scaler
  clock.stops<-clock.stops/scaler
  nwl18<-round(initial.waiting.list*initial.performance)
  nwm18<-round(initial.waiting.list-nwl18)
  #simulate waiting list times using exp dist (note 126 is num of days in 18wks)
  lambda<- -(1/126)*log(1-nwl18/(nwl18+nwm18))
  waits<-c(round(rtrunc(nwl18,"exp",0,126,rate=lambda)),round(rtrunc(nwm18,"exp",126,Inf,rate=lambda)))
  waits<-sort(waits,decreasing=TRUE)
  #add to active list
  rates<-data.frame(arr=clock.starts,srv=clock.stops)
  res<-data.frame(id=1:initial.waiting.list,time_arr=0-waits
                  ,priority=sample(1:length(priority.prob),
                                   initial.waiting.list,replace=TRUE,priority.prob),stringsAsFactors=FALSE)
  ID<-nwl18+nwm18+1
  outp<-data.frame(day=0,nwl18=length(which(waits<=126)),
                   nwm18=length(which(waits>126)),act=NA,maxwt=NA,meanwt=NA,medianwt=NA)
  #step through and simulate each day in considered period
  for (d in 1:nrow(rates)) {
    #arrivals
    rate.arr<-rates$arr[d]
    d_arr<-rpois(1,lambda=rate.arr)
    if (d_arr>0) {
      res<-rbind(res,data.frame(
        id=ID:(ID+d_arr-1),time_arr=d,priority=sample(1:length(priority.prob),
        d_arr,replace=TRUE,priority.prob),stringsAsFactors=FALSE))
    }
    ID<-ID+1
    #services
    rate.srv<-rates$srv[d]
    d_srv<-min(floor(rate.srv)+rbinom(1,size=1,prob=(rate.srv-floor(rate.srv))),nrow(res))
    if (d_srv>0) {
      pri_ls<-d-res$time_arr+priority.points[res$priority]
      names(pri_ls)<-1:nrow(res)
      srv_ls<-as.numeric(names(sort(pri_ls,decreasing=TRUE)[1:d_srv]))
      maxwt<-max(d-res$time_arr[srv_ls])
      meanwt<-mean(d-res$time_arr[srv_ls])
      medianwt<-median(d-res$time_arr[srv_ls])
      res<-res[-srv_ls,]
    } else {
      maxwt<-NA
      meanwt<-NA
      medianwt<-NA
    }
    #output measures
    waits<-d-res$time_arr
    outp<-rbind(outp,data.frame(day=d,nwl18=length(which(waits<=126)),
                                nwm18=length(which(waits>126)),act=d_srv,maxwt=maxwt,meanwt=meanwt,medianwt=medianwt))
  }
  #compile output measures over each simulation run
  outp$nwl18<-outp$nwl18*scaler
  outp$nwm18<-outp$nwm18*scaler
  outp$act<-outp$act*scaler
  outp$wl_size<-outp$nwl18+outp$nwm18
  outp$perf<-outp$nwl18/outp$wl_size
  outp$ref<-SEED
  return(outp)
}

cl<-makeCluster(detectCores()-1)
clusterExport(cl=cl,
              varlist=c("clock.starts","clock.stops","priority.prob","priority.points","initial.waiting.list","initial.performance","scaler"),
              envir=environment())
clusterEvalQ(cl=cl,c(library(truncdist)))
res<-parLapply(cl,1:n.runs,simfn)
stopCluster(cl)
res<-do.call("rbind",res)

######################################################################################################################################################
### OUTPUTS ###

res.sum<-res %>%
  pivot_longer(cols=-c(day,ref),names_to="metric",values_to="value") %>%
  group_by(day,metric) %>%
  summarise(mean=mean(value),q025=quantile(value,0.025,na.rm=TRUE),q975=quantile(value,0.975,na.rm=TRUE))

plot.res<-res.sum %>%
  ggplot(aes(x=day)) +
  geom_vline(xintercept=warm.up.period,linetype="dashed",colour="darkgrey") +
  geom_ribbon(aes(ymin=q025,ymax=q975),fill="grey") +
  geom_line(aes(y=mean)) +
  facet_wrap(~metric,scales="free") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

plot.res


