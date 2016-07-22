options(stringsasFactors = FALSE)

tmp <- scan("/gamelan/berg/Simulations/Unit_Cells/CorrectBox/300K_NVT_cryodim//Simulation/bfac_CA.txt", what = "numeric")

mat <- matrix(tmp, nrow = 2)
vec <- c(mat[2,])
vec <- as.numeric(vec)

bfac <- data.frame(matrix(vec, ncol = 8))

bfac$mean <- apply(bfac, 1, mean)

# for reference, load in crystal B-factors

tmp <- scan("/work2/berg/Simulations/2CGI/Bfacs/2CGI_CA.txt", what = "numeric")

mat <- matrix(tmp, nrow = 2)
vec <- c(mat[2,])
vec <- as.numeric(vec)

png("bfacs2.png")
op <- par(mar = c(5,4.5,4,2) + 0.1)
plot(bfac$mean, main= "B-factors for 300 K NVT crystal simulation (cryo-dimensions)\n vs. crystal structure per C-\U03B1",
		pch = 19, cex = 0, xlim=c(1,130) , ylim=c(0, 40),
		xlab = "Residues", ylab = expression(paste("Bfactor (", "\uc5"^"2", ")", sep = "")))
lines(bfac$mean)
lines(vec, col="red")
legend("topright", c("Simulation values", "Experimental values"), col = c("black", "red") , lwd = 1)
par(op)
dev.off()
