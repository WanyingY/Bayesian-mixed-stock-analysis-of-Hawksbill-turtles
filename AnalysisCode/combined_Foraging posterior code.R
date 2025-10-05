# 
mydata <- read.csv("E:/daimao/Foraging _area_1.csv", row.names=1, header=TRUE)
head(mydata)
# 
mydata  <- as.mixstock.data(mydata)
mydata = markfreq.condense(mydata)
# 
mydata.cml = cml(mydata)
mydata.cml
plot(mydata.cml)
# 
mydata.uml = uml(mydata)
mydata.uml
plot(mydata.uml,plot.freqs=TRUE)
par(ask=FALSE)
# 
mydata.umlboot = genboot(mydata,"uml")
confint(mydata.umlboot)
plot(mydata.umlboot)
# 
mydata.mcmc = tmcmc(mydata)
mydata.mcmc
confint(mydata.mcmc) 
plot(mydata.mcmc) 
# 
diag1=calc.RL.0(mydata)
head(diag1$current)
diag1$history

diag2=calc.GR(mydata)
head(diag2$current)
diag2$history
# 
Z = simmixstock2(nsource=4,nmark=5,nmix=3,
                 sourcesize=c(4,2,1,1),
                 sourcesampsize=rep(25,4),
                 mixsampsize=rep(30,3),rseed=1001)
Z
plot(Z)
#
Zfit0 = mm.wbugs(Z,sourcesize=c(4,2,1,1),returntype="bugs")
plot(Zfit0)
# 
Plot CODA diagnostics (plot not shown):
  plot(as.mcmc.bugs(Zfit0))
Plot results:
  print(plot(Zfit))
print(plot(Zfit,sourcectr=TRUE))
summary(Zfit)
