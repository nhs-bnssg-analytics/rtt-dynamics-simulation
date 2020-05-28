# scenario 1
rates<-data.frame(arr=c(rep(54784,200),rep(54784,365*3)),
                    srv=c(rep(54784,200),rep(0,91),rep(5/4*54784,365),rep(54784,-91+365*2)))

# scenario 2
rates<-data.frame(arr=c(rep(54784,200),rep(0.75*54784,91),rep(54784,365-91+365*2)),
                    srv=c(rep(54784,200),rep(0,91),rep(19/16*54784,365),rep(54784,-91+365*2)))

# scenario 3
rates<-data.frame(arr=c(rep(54784,200),rep(0.75*54784,91),rep(54784,-91+365*3)),
                    srv=c(rep(54784,200),rep(0.5*54784,91),rep(17/16*54784,365),rep(54784,-91+365*2)))

# scenario 4
rates<-data.frame(arr=c(rep(54784,200),rep(0.75*54784,182),rep(54784,365*3-182)),
                    srv=c(rep(54784,200),rep(0.5*54784,182),rep(17/16*54784,365*2),rep(54784,365-182)))

# scenario 5
rates<-data.frame(arr=c(rep(54784,200),rep(0.75*54784,273),rep(54784,365*3-273)),
                    srv=c(rep(54784,200),rep(0.5*54784,273),rep(35/32*54784,365*2),rep(54784,365-273)))

# scenario 6
rates<-data.frame(arr=c(rep(54784,200),rep(0.75*54784,365),rep(54784,365*2)),
                    srv=c(rep(54784,200),rep(0.5*54784,365),rep(1.125*54784,365*2)))
  
rownames(rates)<-(-200+1):(365*3)

