options(stringsasFactors=FALSE)

tmp <- scan("2dproj.xvg", what = "numeric", skip = 16)
mat <- matrix(tmp, nrow = 2)
vec <- c(mat[2,])
vec <- as.numeric(vec)

#colfunc <- colorRampPalette(c("black", "white"))
#plot(mat[1,], mat[2,], col=colfunc(10),pch=19,cex=3)

# I want to give each monomer a different color

cols <- colors()
cols <- col2rgb(cols)
ind <- cols[1,] < 150 | cols[2,] < 150 | cols[3,] < 150
darker <- cols[,ind]

numbers <- sample(1:length(darker[1,]), size = 8)
colours <- darker[,numbers]

png("2dproj_8monomers.png", dpi = 300)
plot(mat[1,1:101], mat[2,1:101], xlab="projection on first PC", ylab="projection on second PC", col=rgb(t(colours[,1]), maxColorValue = 255), type="l", pch=19, cex=2, xlim=c(-1,1), ylim=c(-1,1))
for(n in 2:8){
    begin <- 100*(n-1)+n
    end <- begin + 101
    lines(mat[1,begin:end], mat[2,begin:end], col=rgb(t(colours[,n]), maxColorValue = 255), pch=19, cex=2)
}
legend("topright", paste("chain ", LETTERS[1:8]), col= rgb(t(colours[]), maxColorValue = 255), lwd = 1)
dev.off()
