options(stringsasFactors=FALSE)

d <- AllInOneFile()
d <- RefAndMean(d, TRUE)
d <- data.frame(scale(d))
#c <- ColourByChain()
c <- ColourGradient()
PlotB(dat, c)

ColourGradient <- function(rgb1, rgb2=NULL){
     tc2 <- matrix(c(rep(rgb1[1], 8), rep(rgb1[2], 8), rep(rgb1[3], 8), seq(100, 255, length.out=8)), ncol=8, byrow=TRUE)
     if(!is.null(rgb2)){
         for(n in 1:3){
		tc2[n,] <- seq(rgb1[n], rgb2[n], length.out=8)
	 }
     }
     tc2
}

AllInOneFile <- function(){
	     tmp <- scan("lessframes_filt_tot_PC1-5_bfac_CA_1.txt")
	     mat <- matrix(tmp, nrow=2)
	     mat2 <- matrix(mat[2,], ncol=8)
	     dat <- data.frame(mat2)
	     rownames(dat) <- 1:129
	     colnames(dat) <- LETTERS[1:8]
	     dat
}	     

SeveralFiles <- function(){
     tmp <- scan("A_PC1_bfac_CA.txt")
     mat <- matrix(tmp, nrow=2)
     mat2 <- matrix(mat[2,], ncol=1)
     dat <- data.frame(mat2)
     rownames(dat) <- 1:129
     n <- 2
     for (l in LETTERS[2:8]){
     	 tmp <- scan(paste(l, "_PC1_bfac_CA.txt", sep=""))
	 mat <- matrix(tmp, nrow=2)
	 dat[,n] <- as.vector(mat[2,])
	 n <- n+1
     }
     colnames(dat) <- LETTERS[1:8]
     dat
}     

# for reference, load in crystal B-factors
RefAndMean <- function(dat, cryo){
	if(cryo){
		tmp <- scan("/work2/berg/Simulations/2CGI/Bfacs/2CGI_CA.txt", what = "numeric")
 	}
	else{
		tmp <- scan("/work2/berg/Simulations/Unit_Cells/Ref_bfacs/4O34_RT_CA.txt", what = "numeric")
	}
	mat <- matrix(tmp, nrow = 2)
	vec <- c(mat[2,])
	vec <- as.numeric(vec)
	dat$mean <- apply(dat, 1, mean)
	dat$ref <- vec
	dat
}

# I want to give each monomer a different color
ColourByChain <- function(){
	     cols <- colors()
	     cols <- col2rgb(cols)
	     ind <-  cols[1,] < 150 | cols[2,] < 150 | cols[3,] < 150
	     darker <- cols[,ind]
	     darker <- darker[,seq(1, length(darker[1,]), 40)]
	     numbers <- sample(1:length(darker[1,]), size = 8)
	     colours <- darker[,numbers]
	     colours
}

PlotB <- function(dat, colours, main="test", save=FALSE, normalized=TRUE){
      if (save){
      	 png("norm_bfactors_300K_NVT_cryodim.png", width=480*2.4, height=480*2)
      }
      if(normalized){
	yl=c(-4, 6)
      }
      else{
	yl=c(0,70)
      }
      lw=2
      par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
      
      plot(dat$A, main=main, xlab="residues", ylab="B-factor (A^2)", col=rgb(t(colours[,1]), alpha=colours[4,], maxColorValue = 255), type="l", pch=19, cex=2, lwd=lw, ylim=yl)
#      for (n in 2:8){
#          lines(dat[,n], col=rgb(t(colours[,n]), alpha=colours[4,], maxColorValue = 255), pch=19, cex=2, lwd=lw)
#      }
      lines(dat$ref, col="red", lwd=lw+1)
      lines(dat$mean, col="blue", lwd=lw+1)
      legend("topright", inset=c(-0.1,0), c(paste("chain ", LETTERS[1]), "reference", "mean"), col= c(rgb(t(colours[]), alpha=colours[4,], maxColorValue = 255), "red", "blue"), lwd = c(rep(lw, 8), rep(lw+1,2)))
      if (save){
      	 dev.off()
      }
}

png("bfactors_300K_NVT_cryodim.png", width=480*2.4, height=480*2)

par(mar=c(5.1, 8.1, 4.1, 8.1), xpd=TRUE)
plot(dat$A, main="C-alpha B-factors of PCA results \n combined PCA on 8*50 ns of 300 K NVT simulation (cryo-dimensions)", xlab="residues", ylab="B-factor (A^2)", type="l", pch=19, cex=2, lwd=2, ylim=c(0, 90), col=rgb(t(c[,4]), alpha=c[4,4], maxColorValue = 255))
lines(dat$ref, col="red", lwd=lw+1)
legend("topright", inset=c(-0.1,0), c("PC 1 to 5", "reference"), col= c(rgb(t(c[,4]), alpha=colours[4,4], maxColorValue = 255), "red"), lwd = c(rep(lw, 8), rep(lw+1,2)))

dev.off()
