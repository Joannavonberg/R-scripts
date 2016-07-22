options(stringsasFactors=FALSE)

tmp <- scan("proj_PC1.xvg", what = "numeric", comment.char = "@")
tmp <- as.numeric(tmp)
tmp <- tmp[!is.na(tmp)]

mat <- matrix(tmp, nrow = 2)
#mat <- as.numeric(mat)
#vec <- c(mat[2,])
#vec <- as.numeric(vec)

#mat3 <- matrix(tmp, nrow = 2)

# Colour blind friendly palette (http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# I want to give each monomer a different color
RandomColours <- function(){
	      cols <- colors()
	      cols <- col2rgb(cols)
	      ind <-  cols[1,] < 150 | cols[2,] < 150 | cols[3,] < 150
	      darker <- cols[,ind]
	      darker <- darker[,seq(1, length(darker[1,]), 30)]

	      numbers <- sample(1:length(darker[1,]), size = 8)
	      colours <- darker[,numbers]
	      colours
}

ColourGradient <- function(rgb1, rgb2=NULL){
     tc2 <- matrix(c(rep(rgb1[1], 8), rep(rgb1[2], 8), rep(rgb1[3], 8), seq(100, 255, length.out=8)), ncol=8, byrow=TRUE)
     if(!is.null(rgb2)){
         for(n in 1:3){
		tc2[n,] <- seq(rgb1[n], rgb2[n], length.out=8)
	 }
     }
     tc2
}

colours <- ColourGradient(col2rgb("magenta"), col2rgb("yellow"))

OneColourGradient <- function(col="orange", n=8){
	  c <- col2rgb(col)
	  colours <- matrix(c(rep(c[1], 8), seq(c[2], 255, length.out=8), rep(c[3], 8), seq(100, 255, length.out=8)), nrow=4, byrow=TRUE)
	  colours
}	  

#colours <- 

png("proj_PC1-3.png", width=800, height=800)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(mat[1,1:51], mat[2,1:51], main="1D projection along combined PC", xlab="time (ps)", ylab="displacement(nm)", col=rgb(t(colours[,1]), alpha=colours[4,1], maxColorValue = 255), type="l", pch=19, cex=2, ylim=c(-2,2), lwd=2)
for (n in 2:8){
    begin <- 50*(n-1)+n
    end <- begin + 50
    lines(mat[1,1:51], mat[2,begin:end], col=rgb(t(colours[,n]), alpha=colours[4,n], maxColorValue = 255), pch=19, cex=2, lwd=2)
}
legend("topright", inset=c(-0.15,0), paste("chain ", LETTERS[1:8]), col= rgb(t(colours[]), alpha = colours[4,], maxColorValue = 255), lwd = 2)

dev.off()

plot(mat[1,], mat[2,], main="1D projection along first combined principal component", xlab="timestep", ylab="displacement(nm)", col=rgb(t(colours[,1]), maxColorValue = 255), type="l", pch=19, cex=2, ylim=c(-1,1), lwd=2)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
boxplot(mat[2,1:101]~ mat2[2,1:101], main="projection along combined PC 1 to 3", xlab="first PC", ylab="second PC", col=rgb(t(colours[,1]), maxColorValue = 255), type="l", pch=19, cex=2, lwd=2)
for (n in 2:8){
    begin <- 100*(n-1)+n
    end <- begin + 100
    lines(mat[2,begin:end], mat3[2,begin:end], col=rgb(t(colours[,n]), maxColorValue = 255), pch=19, cex=2, lwd=2)
}
legend("topright", inset=c(-0.15,0), paste("chain ", LETTERS[1:8]), col= rgb(t(colours[]), maxColorValue = 255), lwd = 2)

png("3dplot_PC1-3.png")

PCs <-scatterplot3d(mat[2,102:202], mat2[2,102:202], mat3[2,102:202], xlab="PC1", ylab="PC2", zlab="PC3", pch=16, highlight.3d=TRUE, type="l", main="scatterplot of displacement in first 3 PC's")
fit <- lm(mat3[2,1:101] ~ mat[2,1:101]+mat2[2,1:101])
PCs$plane3d(fit)

dev.off()

plot3d(mat[2,102:202], mat2[2,102:202], mat3[2,102:202], col="red", size=3, type="l", xlab="PC1", ylab="PC2", zlab="PC3")

# boxplots

png("boxplot_PC3.png")
PC2 <- matrix(mat2[2,], ncol=8)
rownames(PC1) <- seq(0, 10000, 100)
boxplot(PC1, main="boxplots for PC 1", names=paste("chain", LETTERS[1:8]), ylab="displacement (nm)")
dev.off()

plot(mat[1,], mat[2,], main="1D projection along first combined principal component", xlab="timestep", ylab="displacement(nm)", col=colours[1], maxColorValue = 255), type="l", pch=19, cex=2, ylim=c(-1,1), lwd=2)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
boxplot(mat[2,1:101]~ mat2[2,1:101], main="projection along combined PC 1 to 3", xlab="first PC", ylab="second PC", col=colours[1], maxColorValue = 255), type="l", pch=19, cex=2, lwd=2)
for (n in 2:8){
    begin <- 100*(n-1)+n
    end <- begin + 100
    lines(mat[2,begin:end], mat3[2,begin:end], col=rgb(t(colours[,n]), maxColorValue = 255), pch=19, cex=2, lwd=2)
}
legend("topright", inset=c(-0.15,0), paste("chain ", LETTERS[1:8]), col= colours[], maxColorValue = 255), lwd = 2)

png("proj_PC1.png")

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(mat[1,1:51], mat[2,1:51], main="1D projection along combined PC 1", xlab="time (ps)", ylab="displacement(nm)", col=colours[1], type="l", pch=19, cex=2, ylim=c(-2,2), lwd=2)
for (n in 2:8){
    begin <- 50*(n-1)+n
    end <- begin + 50
    lines(mat[1,1:51], mat[2,begin:end], col=colours[n], pch=19, cex=2, lwd=2)
}
legend("topright", inset=c(-0.15,0), paste("chain ", LETTERS[1:8]), col= colours[], lwd = 2)

dev.off()