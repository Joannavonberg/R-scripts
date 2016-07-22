library("TeachingDemos")

#PC1 <- read.table("proj_200ns2.xvg", skip = 24)
#PC2 <- read.table("proj_200ns_PC2.xvg", skip = 22)
#uc <- data.frame(cbind(PC1$V2, PC2$V2))

uc <- read.table("proj_200ns.xvg", skip = 12)

kleurtjes <- colorRampPalette(c("red", "blue"))
plot(uc, cex = 0.01, col = kleurtjes(length(uc[,1])))

#com <- read.table("proj_test.xvg", skip = 12)

# Determine number of clusters by within-groups-sum-of-squares (WSS)
wss <- (nrow(uc)-1)*sum(apply(uc,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(uc,
   centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")

n <- 4
# K-Means Cluster Analysis
fit <- kmeans(uc, n)
# get cluster means
aggregate(uc, by=list(fit$cluster), FUN=mean)
# append cluster assignment
uc_c <- data.frame(uc, fit$cluster)

col <- sample(colors(), size = n)

png("proj_cluster.png", width = 800, height = 800)

plot(uc, main = "projection of first two principal components", xlab = "PC1 (nm)", ylab = "PC2 (nm)")

for(i in 1:n){
    ind <- uc_c$fit.cluster == i
    lines(uc_c[ind,1], uc_c[ind,2], col = col[i], type = "p")
}

dev.off()

# rolling average for one PC

plot(uc[,2])

timeperiod <- 100
cra_PC1 <- matrix(ncol = 2, nrow = length(uc[,1]) - (timeperiod-1))
begin <- ceiling(timeperiod/2)
end <- length(uc[,1])- floor(timeperiod/2)
cra_PC1[,1] <- begin:end

for(i in cra_PC1[,1]){
     cra_PC1[i-floor(timeperiod/2),2] <- mean(uc[,2][(i-floor(timeperiod/2)):(i+floor(timeperiod/2))])
}

lines(cra_PC1[,2], col = "blue", lwd = 2)

# rolling average for two PC's

# timeperiod defines the 'steps'
plot(uc, cex = 0.01, col = kleurtjes(length(uc[,1])))

timeperiod <- 500
fl <- floor(timeperiod/2)
cra <- matrix(ncol = 3, nrow = length(uc[,1]) - (timeperiod-1))
begin <- ceiling(timeperiod/2)
end <- length(uc[,1])- fl
cra[,1] <- begin:end

cra <- data.frame(cra)
colnames(cra) <- c("Time", "PC1", "PC2")
cra$cluster <- uc_c$fit.cluster[begin:end]

for(i in cra[,1]){
     range <- (i-fl):(i+fl)
     #range <- (i-1):(i+1)
     cra$PC1[i-fl] <- mean(uc[,1][range])
     cra$PC2[i-fl] <- mean(uc[,2][range])
     cra$cluster[i-fl] <- mode(uc_c$fit.cluster[range])
}

cra$colour <- col[cra$cluster]

lines(cra$PC1, cra$PC2, lwd = 2)

# combining centered rolling average with clusters

png("proj_clusters_transitions+timepoints.png", width = 800, height = 800)

#plot(uc, type = "n", main = "Projection of 60 ns trajectory along the first two combined principal components", xlab = "Principal component 1 (nm)", ylab = "Principal component 2 (nm)")

plot(uc, cex = 0.01, col = kleurtjes(length(uc[,1])))
lines(cra$PC1, cra$PC2, lwd = 2)

for(i in 1:n){
    ind <- uc_c$fit.cluster == i
    lines(uc_c$V1[ind], uc_c$V2[ind], col = col[i], type = "p")
    ind2 <- cra$cluster == i
    lines(cra[ind2,2], cra[ind2,3], lwd = 3, col = col[i])
}

# prints lines with times for every 10 ns
for(i in seq(1000, 20000, 1000)){
    offset <- 0.1
    lines(c(cra$PC1[i-fl] - offset, cra$PC1[i-fl]+offset), c(cra$PC2[i-fl], cra$PC2[i-fl]), lwd = 3)
    shadowtext(cra$PC1[i-fl]+offset, cra$PC2[i-fl], pos = 3, labels = sprintf("%.0f ns", i/100), col = "white", bg = "black")
}

i = 19996
text(cra$PC1[i-fl]+offset, cra$PC2[i-fl], pos = 3, labels = sprintf("%.0f ns", i/100), col = "red")

# prints lines with times for cluster transitions
offset <- 0.5
for(i in 1:n){
    ndx <- match(i, cra$cluster)
    if(!is.na(ndx) && ndx != 1){
         pc1 <- cra$PC1[ndx]
	 pc2 <- cra$PC2[ndx]
         lines(c(pc1 - offset, pc1 + offset), c(pc2, pc2), lwd = 3)
         text(pc1 + offset, pc2, pos = 3, labels = sprintf("%.0f ns", (ndx+fl)/10))
    }
}

dev.off()

# col = rgb(t(c(col2rgb(col[i])))
lines(cra[,2], cra[,3], lwd = 3, col = cra$colour, type = "p", pch = 19, cex = 0.5)

rgb( t( c( col2rgb( col[i] ), 0.5 ), maxColorValue = 255))

mode <- function(x){
   temp <- table(as.vector(x))
   names(temp)[temp == max(temp)]
}


png("200ns_proj_timesteps_lesssteps.png", width = 800, height = 800)

plot(uc, main = "Projection of simulation on first 2 principal components", xlab = "PC1", ylab = "PC2", cex = 0.01, col = kleurtjes(length(uc[,1])))
lines(cra$PC1, cra$PC2, lwd = 2)
for(i in seq(1000, 20000, 2000)){
    offset <- 0.1
    lines(c(cra$PC1[i-fl] - offset, cra$PC1[i-fl]+offset), c(cra$PC2[i-fl], cra$PC2[i-fl]), lwd = 3)
    shadowtext(cra$PC1[i-fl]+offset, cra$PC2[i-fl], pos = 3, labels = sprintf("%.0f ns", i/100), col = "white", bg = "black")
}

dev.off()
