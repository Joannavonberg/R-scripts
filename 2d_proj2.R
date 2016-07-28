#install.packages("corrplot")
library("corrplot")

options(stringsAsFactors = FALSE)

for(let in LETTERS[1:8]){
  assign(let, read.table(sprintf("proj_%s.xvg", let), skip = 24))
}

png("2D_proj.png", width = 1000, height = 1000)

#col <- sample(colors(), 8)
plot(A$V1, A$V2, type = "p", xlim = c(-3,5), ylim = c(-3,3), col = col[1], lwd = 2, main = "projection along the first two principal components", xlab = "PC1 (nm)", ylab = "PC2 (nm)")
lines(A$V1, A$V2, type = "l", xlim = c(-3,5), ylim = c(-3,3), col = col[1], lwd = 2)

for(i in 2:8){
  obj <- get(LETTERS[i])
  lines(obj$V1, obj$V2, type = "l", col = col[i], lwd = 2)
  lines(obj$V1, obj$V2, type = "p", col = col[i], lwd = 2)
}

legend("topright", legend = LETTERS[1:8], col = col, lty = 1, lwd = 2)

dev.off()

MakePC <- function(ind){
    PC <- matrix(ncol = 8, nrow = 601)
    for(i in 1:8){
        let <- LETTERS[i]
        obj <- get(let)
        PC[,i] <- obj[,ind]
    }
    PC <- data.frame(PC)
    colnames(PC) <- LETTERS[1:8]
    PC
}

PC1 <- MakePC(1)
PC2 <- MakePC(2)

png("cor_PC1.png", width = 1000, height = 1000)

corrplot.mixed(cor(PC1), title = "correlation coefficients for the first principal component")

dev.off()

png("cor_PC2.png", width = 1000, height = 1000)

corrplot.mixed(cor(PC2), title = "correlation coefficients for the second principal component")

dev.off()

corrplot.mixed(cov(PC2), title = "covariance matrix for the second principal component", is.corr = FALSE)

# lose the dime-domain
plot(1, A$V1[1], xlim = c(1,8), ylim = c(-4,4))
for(n in 1:8){
    obj <- get(LETTERS[n])
    for(i in 1:601){lines(n, obj[,1][i], type = "p")}
}


make_boxplot <- function(pc, save = FALSE, main = "test", names = LETTERS[1:8]){
    if(save){ png(sprintf("boxplots_PC%.0f.png", pc)) }	     
    vec <- c()
    for(let in LETTERS[1:8]){
        vec <- c(vec, get(let)[,pc])
    }
    mat <- matrix(vec, ncol = 8)
    boxplot(mat, ylim = c(-4,5), main = main, names = names, ylab = "value (nm)")
    if(save){dev.off()}
}    
