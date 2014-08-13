#import case study data - Clustering of Infant HIV Clinics in Sub-Saharan Africa (fictional)
infant=read.csv("infantHIVClustering.csv")
summary(infant)
#explore individual variables
par(mfrow=c(2,3)); for (i in 2:9) hist(infant[[i]],xlab=names(infant)[i])
#identify correlations between operational metrics and medical outcomes
pairs(infant[,2:9])
#scale the data to prepare for cluster analysis
Zinfant=data.frame(lapply(infant[,2:9], scale, scale=T))

#import default clustering package
library(lattice)
#custom K-means summary function
summary.kmeans = function(fit)
{
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  n = sum(fit$size)
  sse = sum(fit$withinss)
  xbar = t(fit$centers)%*%fit$size/n
  ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2)
  print(data.frame(
    n=c(fit$size, n),
    Pct=(round(c(fit$size, n)/n,2)),
    round(rbind(fit$centers, t(xbar)), 2),
    RMSE = round(sqrt(c(fit$withinss/(p*fit$size-1), sse/(p*(n-k)))), 4)
  ))
  cat("SSE = ", sse, "; SSB = ", ssb, "\n")
  cat("R-Squared = ", ssb/(ssb+sse), "\n")
  cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
  invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
}

#custom K-means plot function
plot.kmeans = function(fit,boxplot=F)
{
  require(lattice)
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  plotdat = data.frame(
    mu=as.vector(fit$centers),
    clus=factor(rep(1:k, p)),
    var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers))
  )
  print(dotplot(var~mu|clus, data=plotdat,
                panel=function(...){
                  panel.dotplot(...)
                  panel.abline(v=0, lwd=.1)
                },
                layout=c(k,1),
                xlab="Cluster Mean"
  ))
  invisible(plotdat)
}

#4, 5 and 6 cluster solutions
#identified that 3-4 variables capture operational challenges
#with larger amounts of predictors, use principal component analysis
fit.infant4a = kmeans(Zinfant[,1:3],4,nstart=100)
fit.infant5a = kmeans(Zinfant[,1:3],5,nstart=100)
fit.infant6a = kmeans(Zinfant[,1:3],6,nstart=100)
fit.infant4b = kmeans(Zinfant[,c(1:3,8)],4,nstart=100)
fit.infant5b = kmeans(Zinfant[,c(1:3,8)],5,nstart=100)
fit.infant6b = kmeans(Zinfant[,c(1:3,8)],6,nstart=100)
summary(fit.infant4a)
summary(fit.infant5a)
summary(fit.infant6a)
summary(fit.infant4b)
summary(fit.infant5b)
summary(fit.infant6b)
#the 6-cluster solution with 4 predictor variables is optimal
plot(fit.infant6b)
#write the cluster results to an output file for further analysis
results=data.frame(infant,fit.infant6b$cluster)
write.csv(results,"results.csv")
