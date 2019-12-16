# name: discrete-time simulation for understanding rtt dynamics 
# version: v1
# date: june 2019
# contact: richard.wood16@nhs.net
# note: dummy data included for illustration

rm(list=ls())
require(truncdist)
require(foreach)
require(doSNOW)

######################################################################################################################################################
### INPUTS ###

clock.starts<-rep(100,200)       # vector containing num clock starts for each day in considered period
clock.stops<-rep(100,200)        # vector containing max num clock stops (capacity) for each day in considered period
priority.prob<-c(0.5,0.5)        # probability of being in considered priority classes (sampled on referral)
priority.points<-c(0,10)         # initial points awarded to referrals belonging to each priority class
initial.waiting.list.size<-1000  # initial number of incomplete referrals (i.e. with no clock stop)
initial.performance<-0.9         # initial performance (to target)

n.runs<-10
SEED<-1

######################################################################################################################################################
### SIMULATION ###

cl<-makeCluster(7)
registerDoSNOW(cl)
RESULTS<-foreach(runs=1:n.runs,.combine="rbind",.packages=c("truncdist")) %dopar% {
  set.seed(SEED)
  #initial conditions
  nwl18<-round(initial.waiting.list.size*initial.performance)
  nwm18<-initial.waiting.list.size-nwl18
  #simulate waiting list times using exp dist (note 126 is num of days in 18wks)
  lambda<- -(1/126)*log(1-nwl18/(nwl18+nwm18))
  waits<-c(round(rtrunc(nwl18,"exp",0,126,rate=lambda)),round(rtrunc(nwm18,"exp",126,Inf,rate=lambda)))
  waits<-sort(waits,decreasing=TRUE)
  #add to active list
  rates<-data.frame(arr=clock.starts,srv=clock.stops)
  res<-data.frame(id=1:initial.waiting.list.size,time_arr=as.numeric(rownames(rates)[1])-1-waits
                  ,priority=sample(1:length(priority.prob),
               initial.waiting.list.size,replace=TRUE,priority.prob),stringsAsFactors=FALSE)
  ID<-nwl18+nwm18+1
  outp<-data.frame(day=as.numeric(rownames(rates)[1])-1,nwl18=length(which(waits<=126)),
                   nwm18=length(which(waits>126)),act=NA,maxwt=NA,meanwt=NA,medianwt=NA)
  #step through and simulate each day in considered period
  for (d in as.numeric(rownames(rates))) {
    rates.ind<-which(rownames(rates)==d)
    #arrivals
    rate.arr<-rates$arr[rates.ind]
    d_arr<-rpois(1,lambda=rate.arr)
    if (d_arr>0) {
      tres<-data.frame(id=ID:(ID+d_arr-1),time_arr=d,priority=sample(1:length(priority.prob),
                        d_arr,replace=TRUE,priority.prob),stringsAsFactors=FALSE)
      res<-rbind(res,tres)
    }
    ID<-ID+1
    #services
    rate.srv<-rates$srv[rates.ind]
    d_srv<-floor(rate.srv)+rbinom(1,size=1,prob=(rate.srv-floor(rate.srv)))
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
  outp$wl_size<-outp$nwl18+outp$nwm18
  outp$perf<-outp$nwl18/outp$wl_size
  RES<-matrix(c(outp$wl_size,outp$perf,outp$act),ncol=length(outp$wl_size),nrow=3,byrow=TRUE)
  RES<-cbind(c(1,2,3),RES)
  return(RES)
}
stopCluster(cl)

######################################################################################################################################################
### OUTPUTS ###

#aggregate output measures for waiting list size, performance and activity (dependent variables)
RES_wlsize<-RESULTS[which(RESULTS[,1]==1),2:ncol(RESULTS),drop=FALSE]
RES_perf<-RESULTS[which(RESULTS[,1]==2),2:ncol(RESULTS),drop=FALSE]
RES_act<-RESULTS[which(RESULTS[,1]==3),2:ncol(RESULTS),drop=FALSE]

#provides a table for average waiting list size, performance and activity at each time period considered
RES_avg<-data.frame(wlsize=colSums(RES_wlsize)/nrow(RES_wlsize),perf=colSums(RES_perf)/nrow(RES_perf),act=colSums(RES_act)/nrow(RES_act))

#provides a table for lower 95% waiting list size and performance at each time period considered
RES_95_lower<-data.frame(wlsize_lower=sapply(1:ncol(RES_wlsize),function(x) quantile(RES_wlsize[,x],0.025)),
                         perf_lower=sapply(1:ncol(RES_perf),function(x) quantile(RES_perf[,x],0.025)))

#provides a table for upper 95% waiting list size and performance at each time period considered
RES_95_upper<-data.frame(wlsize_upper=sapply(1:ncol(RES_wlsize),function(x) quantile(RES_wlsize[,x],0.975)),
                         perf_upper=sapply(1:ncol(RES_perf),function(x) quantile(RES_perf[,x],0.975)))





