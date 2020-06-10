## MCMC in R(Cracker Market)
```
### MACS 30250 Final Paper
### Estimate Loss aversion in Cracker Market
### Haihao Guo
### June 10, 2020

library(bayesm)
library(mlogit)
library(rjags)
library(coda)
library(numDeriv)
library(bayesplot)
library(ggplot2)
library(rstanarm)
library(data.table)
#load data from mlogit package
data(Cracker)
crack <- Cracker
crack$price.sunshine <- crack$price.sunshine / 100
crack$price.keebler <- crack$price.keebler / 100
crack$price.nabisco <- crack$price.nabisco / 100
crack$price.private <- crack$price.private / 100

for (i in 1:3292){
  for (j in 10:13){
    if (length(crack[,j][i]) == 0){
      crack[,j][i] <- crack[,j][i-1]
    }
  }
}
#get memory-based reference price
a = c('sunshine', 'keebler', 'nabisco', 'private')
rpm = c(0)
for (i in 2:3292){
  if (crack$choice[i-1] == 'sunshine'){
    if (length(crack$price.sunshine[i-1]) != 0){
    rpm[i] <- crack$price.sunshine[i-1]
    }else{
      rpm[i] <- crack$price.sunshine[i-2]
    }
  }
  if (crack$choice[i-1] == 'keebler'){
    if (length(crack$price.keebler[i-1]) != 0){
      rpm[i] <- crack$price.keebler[i-1]
    }else{
      rpm[i] <- crack$price.keebler[i-2]
    }
  }
  if (crack$choice[i-1] == 'nabisco'){
    if (length(crack$price.nabisco[i-1]) != 0){
      rpm[i] <- crack$price.nabisco[i-1]
    }else{
      rpm[i] <- crack$price.nabisco[i-2]
    }
  }
  if (crack$choice[i-1] == 'private'){
    if (length(crack$price.private[i-1]) != 0){
      rpm[i] <- crack$price.private[i-1]
    }else{
      rpm[i] <- crack$price.private[i-2]
    }
  }
}

#get stimulus-based reference price
rps = c(0)
for (i in 2:3292){
  if (crack$choice[i-1] == 'sunshine'){
    if (length(crack$price.sunshine[i]) != 0){
      rps[i] <- crack$price.sunshine[i]
    }else{
      rps[i] <- crack$price.sunshine[i-1]
    }
  }
  if (crack$choice[i-1] == 'keebler'){
    if (length(crack$price.keebler[i]) != 0){
      rps[i] <- crack$price.keebler[i]
    }else{
      rps[i] <- crack$price.keebler[i-1]
    }
  }
  if (crack$choice[i-1] == 'nabisco'){
    if (length(crack$price.nabisco[i]) != 0){
      rps[i] <- crack$price.nabisco[i]
    }else{
      rps[i] <- crack$price.nabisco[i-1]
    }
  }
  if (crack$choice[i-1] == 'private'){
    if (length(crack$price.private[i]) != 0){
      rps[i] <- crack$price.private[i]
    }else{
      rps[i] <- crack$price.private[i-1]
    }
  }
}

# specify gain and loss in both situations

#memory-based
crack$gainm.sunshine <- rpm - crack$price.sunshine
crack$gainm.keebler <- rpm - crack$price.keebler
crack$gainm.nabisco <- rpm - crack$price.nabisco
crack$gainm.private <- rpm - crack$price.private
crack$lossm.sunshine <- crack$price.sunshine - rpm
crack$lossm.keebler <- crack$price.keebler - rpm
crack$lossm.nabisco <- crack$price.nabisco - rpm
crack$lossm.private <- crack$price.private - rpm

#stimulus-based
crack$gains.sunshine <- rps - crack$price.sunshine
crack$gains.keebler <- rps - crack$price.keebler
crack$gains.nabisco <- rps - crack$price.nabisco
crack$gains.private <- rps - crack$price.private
crack$losss.sunshine <- crack$price.sunshine - rps
crack$losss.keebler <- crack$price.keebler - rps
crack$losss.nabisco <- crack$price.nabisco - rps
crack$losss.private <- crack$price.private - rps


# make the first gain/loss for every individual's first purchase equals to 0
index_start = c()
for (i in 1:136){
  index_start = c(index_start, which(crack$id == i)[1])
}

for (i in index_start){
  crack$gains.keebler[i] = 0
  crack$gains.sunshine[i] = 0
  crack$losss.sunshine[i] = 0
  crack$losss.keebler[i] = 0
  crack$gains.private[i] = 0
  crack$losss.private[i] = 0
  crack$gains.nabisco[i] = 0
  crack$losss.nabisco[i] = 0
  
  crack$gainm.keebler[i] = 0
  crack$gainm.sunshine[i] = 0
  crack$lossm.sunshine[i] = 0
  crack$lossm.keebler[i] = 0
  crack$gainm.private[i] = 0
  crack$lossm.private[i] = 0
  crack$gainm.nabisco[i] = 0
  crack$lossm.nabisco[i] = 0
}

# specify loss and gain
for (i in 1:3292){
  for (j in 15:30){
    if (crack[,j][i] < 0){
      crack[,j][i] <- 0
    }
  }
}


dt <- data.table(crack)
rm(crack)

#Data cleaning
#No.Trips
dt[, trip := seq(.N), by = id]
#Reshape to trips*ID format
dat <- melt(dt, measure = patterns("^disp", "^feat", "^gainm", "^lossm", "^gains", "^losss"), 
            value.name=c("display", "feature", "gainm", "lossm", "gains", "losss"))
setnames(dat, "variable", "product")
alt <- data.table(altern = c("sunshine", "keebler", "nabisco", "private"), product = factor(1:4))
dat <- merge(dat, alt, by = "product", all.x = TRUE)
dat <- dat[order(id, trip)]
#Create choice indicators
dat[, bought := (choice == altern)*1]
dat[, bought_prod:= (which(choice == altern)), by = .(id, trip)]
setnames(dat,"choice","chosen_prod")
setnames(dat,"bought_prod","choice")
#Reorder variables
setcolorder(dat,c("id","trip","choice","gainm", "lossm", "gains", "losss","feature","display",
                  "product","bought","altern","chosen_prod"))
rm(dt)

# Estimate with heterogeneity - 1 components
dat.split = split(dat,dat[,id])
dat1 = lapply(dat.split,function(x) list(y=unique(x[, .(id,trip,choice)])$choice,
                                         X=as.matrix(x[, .(feature,display,gainm,lossm)])))
spec = rhierMnlRwMixture(Data = list(lgtdata = dat1 , p = 4),
                         Prior = list(ncomp = 1),
                         Mcmc = list(R = 1e4, nprint = 1e3))

#Posterior distribution
summary(spec$nmix)
plot(spec$nmix)
plot(spec$betadraw)
plot(spec$loglike)
apply(spec$betadraw,2,mean)
apply(spec$betadraw,2,FUN = function(x) quantile(x, probs = c(0.025, 0.5, 0.975)))

install.packages("stargazer") 
library(stargazer)
stargazer(spec)





library(data.table)
crack <- crack[ ,c(1:13 ,15:30 ,14)]
setDT(crack)[,choice.sunshine:= (choice == 'sunshine') + 0L, by= id]
setDT(crack)[,choice.keebler:= (choice == 'keebler') + 0L, by= id]
setDT(crack)[,choice.nabisco:= (choice == 'nabisco') + 0L, by= id]
setDT(crack)[,choice.private:= (choice == 'private') + 0L, by= id]


cr <- mlogit.data(crack, shape = "wide", choice = "choice",
                  varying = 2:29)

cr.ml <- mlogit(choice ~ price + disp + feat, data = cr)
cr.mlm <- mlogit(choice ~ gainm + lossm + disp + feat, data = cr)
cr.mls <- mlogit(choice ~ gains + losss + disp + feat, data = cr)
summary(cr.mlm)
summary(cr.mls)

library("texreg")
texreg(list('MNL(Null)' = cr.ml, 'MNL(loss aversion-m)' = cr.mlm, 'MNL(loss aversion-s)' = cr.mls),
       digits = 3, float.pos = "hbt", label = "tab:risktr", single.row = TRUE,
       caption = "Cracker Market")

yr <- mlogit.data(crack, shape = "wide", choice = "choice",
                  varying = c(c(2:9), c(15:22)))


yr.ml <- mlogit(choice ~ gain^2 + loss^2 + disp + feat, data = cr)
yr.ml1 <- mlogit(choice ~ gain + loss + disp + feat |0, data = cr)
summary(yr.ml1)
summary(yr.ml)

library("texreg")
texreg(list('MNL(Null)' = Cracker.ml, 'MNL(loss aversion)' = yr.ml1),
       digits = 3, float.pos = "hbt", label = "tab:risktr", single.row = TRUE,
       caption = "Transportation choices.")

model = "
model 
{
  
  # Loop over Total observations
  for(i in 1:3292)
  {
  
  zh[i,1:4] ~ dmulti( pr[i,1:4] , 1);
  
  # Construct Utilities
  u[i,1]<-a[id[i],1]+a[id[i],4]*ft[i,1]+a[id[i],5]*dp[i,1]+a[id[i],6]*gain[i,1]+a[id[i],7]*loss[i,1];
  u[i,4]<-a[id[i],2]+a[id[i],4]*ft[i,4]+a[id[i],5]*dp[i,4]+a[id[i],6]*gain[i,4]+a[id[i],7]*loss[i,4];
  u[i,3]<-a[id[i],3]+a[id[i],4]*ft[i,3]+a[id[i],5]*dp[i,3]+a[id[i],6]*gain[i,3]+a[id[i],7]*loss[i,3];
  u[i,2]<-               a[id[i],4]*ft[i,2]+a[id[i],5]**dp[i,2]+a[id[i],6]*gain[i,2]+a[id[i],7]*loss[i,2];
  
  # Sum(exp(U))
  vbot[i]<-exp(u[i,1])+exp(u[i,2])+exp(u[i,3])+exp(u[i,4]);
  
  # Choice Probabilities
  pr[i,1]<-exp(u[i,1])/vbot[i];
  pr[i,2]<-exp(u[i,2])/vbot[i];
  pr[i,3]<-exp(u[i,3])/vbot[i];
  pr[i,4]<-exp(u[i,4])/vbot[i];
  
  }
  
  # Loop over Individual Households
  for(k in 1:136) {
  
  # parameters have joint covariance (actually precision)
  a[k,1:7] ~ dmnorm(a.mu[1:7], a.tau[1:7, 1:7]);
  
  }
  
  # Prior parameters
  # MVN on Means
  a.mu[1:7] ~ dmnorm(mean[1:7], prec[1:7, 1:7])
  
  # Wishart on precision
  a.tau[1:7, 1:7] ~ dwish(R[1:7, 1:7], 7)
  
  # Invert Precision Matrix to get Covariance Matrix
  a.sig[1:7, 1:7] <- inverse(a.tau[,])
  
  } 
  "


dat1=crack
dat_m = list(
  id=dat1[,1],
  zh = dat1[,31:34],
  dp = dat1[,2:5],
  ft = dat1[,6:9],
  gain = dat1[,14:17],
  loss = dat1[,18:21],
  mean = c(0, 0, 0, 0, 0 ,0, 0),
  R = structure(.Data = c(0.1,0,0,0,0,0,0,
                          0,0.1,0,0,0,0,0,
                          0,0,0.1,0,0,0,0,
                          0,0,0,0.1,0,0,0,
                          0,0,0,0,0.1,0,0,
                          0,0,0,0,0,0.1,0,
                          0,0,0,0,0,0,0.1), .Dim = c(7, 7)),
  prec = structure(.Data = c(1.0E-6,0,0,0,0,0,0,
                             0,1.0E-6,0,0,0,0,0,
                             0,0,1.0E-6,0,0,0,0,
                             0,0,0,1.0E-6,0,0,0,
                             0,0,0,0,1.0E-6,0,0,
                             0,0,0,0,0,1.0E-6,0,
                             0,0,0,0,0,0,1.0E-6), .Dim = c(7, 7)));

datm = list(
  id=dat1[,1],
  zh = dat1[,31:34],
  dp = dat1[,2:5],
  ft = dat1[,6:9],
  gain = dat1[,14:17],
  loss = dat1[,18:21],
  mean = c(0, 0, 0, 0, 0 ,0, 0),
  R = structure(.Data = c(0.1,0,0,0,0,0,0,
                          0,0.1,0,0,0,0,0,
                          0,0,0.1,0,0,0,0,
                          0,0,0,0.1,0,0,0,
                          0,0,0,0,0.1,0,0,
                          0,0,0,0,0,0.1,0,
                          0,0,0,0,0,0,0.1), .Dim = c(7, 7)),
  prec = structure(.Data = c(1.0E-6,0,0,0,0,0,0,
                             0,1.0E-6,0,0,0,0,0,
                             0,0,1.0E-6,0,0,0,0,
                             0,0,0,1.0E-6,0,0,0,
                             0,0,0,0,1.0E-6,0,0,
                             0,0,0,0,0,1.0E-6,0,
                             0,0,0,0,0,0,1.0E-6), .Dim = c(7, 7)));


datm$id <- datm$id$id
datm$zh <- as.data.frame(datm$zh[,1:4])
datm$ft <- as.data.frame(datm$ft[,1:4])
datm$gain <- as.data.frame(datm$gain[,1:4])
datm$loss <- as.data.frame(datm$loss[,1:4])

inits = list(a.mu = c(0,0,0,0,1,-2,-2), 
             a.tau = structure(.Data = c(0.1,0,0,0,0,0,0,
                                         0,0.1,0,0,0,0,0,
                                         0,0,0.1,0,0,0,0,
                                         0,0,0,0.1,0,0,0,
                                         0,0,0,0,0.1,0,0,
                                         0,0,0,0,0,0.1,0,
                                         0,0,0,0,0,0,0.1), .Dim = c(7, 7)))

jmod = jags.model(textConnection(model),data=datm,inits=inits,n.adapt = 10000)

# Update without storing
update(jmod, 20000)

# Get Samples
jmod.samp = jags.samples(jmod,c("a.mu","a.tau"),5000)

# Get Samples
jmod.coda = coda.samples(jmod,c("a.mu"),1000)
jmod.samp
plot(jmod.coda)
library(coda)


dats = list(
  id=dat1[,1],
  zh = dat1[,31:34],
  dp = dat1[,2:5],
  ft = dat1[,6:9],
  gain = dat1[,14:17],
  loss = dat1[,18:21],
  mean = c(0, 0, 0, 0, 0 ,0, 0),
  R = structure(.Data = c(0.1,0,0,0,0,0,0,
                          0,0.1,0,0,0,0,0,
                          0,0,0.1,0,0,0,0,
                          0,0,0,0.1,0,0,0,
                          0,0,0,0,0.1,0,0,
                          0,0,0,0,0,0.1,0,
                          0,0,0,0,0,0,0.1), .Dim = c(7, 7)),
  prec = structure(.Data = c(1.0E-6,0,0,0,0,0,0,
                             0,1.0E-6,0,0,0,0,0,
                             0,0,1.0E-6,0,0,0,0,
                             0,0,0,1.0E-6,0,0,0,
                             0,0,0,0,1.0E-6,0,0,
                             0,0,0,0,0,1.0E-6,0,
                             0,0,0,0,0,0,1.0E-6), .Dim = c(7, 7)));


dats$id <- dats$id$id
dats$zh <- as.data.frame(dats$zh[,1:4])
dats$ft <- as.data.frame(dats$ft[,1:4])
dats$gain <- as.data.frame(dats$gain[,1:4])
dats$loss <- as.data.frame(dats$loss[,1:4])

inits = list(a.mu = c(0,0,0,0,0,0,-1), 
             a.tau = structure(.Data = c(0.1,0,0,0,0,0,0,
                                         0,0.1,0,0,0,0,0,
                                         0,0,0.1,0,0,0,0,
                                         0,0,0,0.1,0,0,0,
                                         0,0,0,0,0.1,0,0,
                                         0,0,0,0,0,0.1,0,
                                         0,0,0,0,0,0,0.1), .Dim = c(7, 7)))

jmods = jags.model(textConnection(model),data=dats,inits=inits,n.adapt = 10000)

# Update without storing
update(jmods, 20000)

# Get Samples
jmods.samp = jags.samples(jmods,c("a.mu","a.tau"),5000)

# Get Samples
jmods.coda = coda.samples(jmods,c("a.mu"),1000)
jmods.samp
plot(jmods.coda)




Cr <- mlogit.data(Cracker, shape = "wide", choice = "choice",
                  varying = c(2:13))

Cracker.ml <- mlogit(choice ~ price + disp + feat, data = Cr)
```

