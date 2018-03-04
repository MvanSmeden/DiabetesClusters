###############################################
# Author: M van Smeden                        #
# Optimal no clusters (5-cluster Diabetes)    #
# Date: March 4 2018                          #
###############################################

# dependencies #
require(MASS)
require(mclust)
require(ggplot2)
# ---------- #

# simulations -------------
n <- 8980
S <- matrix(.3,ncol=6,nrow=6)
diag(S)<- 1
clust <- c()

set.seed(2018)

for(i in 1:100){
  df <- rbind(mvrnorm(n/2,mu=seq(0,1,by=.2),Sigma=S),mvrnorm(n/2,mu=seq(0.2,1.2,by=.2),Sigma=S))
  bic <- mclustBIC(df)
  out <- Mclust(df, x = bic, modelNames = ("EII"))
  clust[i] <- out$G
  print(i)
}
median(clust)


# end simulations ---------
results <- data.frame(n=clust)  
ggplot(results,aes(x=as.factor(n)))+geom_bar() + xlab("number of clusters") + ggtitle("How many clusters are optimal?") + theme_bw()
ggsave("optimal_clusters.png",width=4,height=4)

