library(png)
library(waveslim)

wave.names <- c("haar", "d4", "mb4", "w4", "bs3.1", "fk4", "d6", "fk6", "d8", "fk8", "la8", "mb8", "bl14", "fk14", "d16", "la16", "mb16", "la20", "bl20", "fk22", "mb24")

path = "Data/Lena.png"

img <- readPNG(path)
# plot.new()

Display <- function(img, bottom.left.x, bottom.left.y, top.right.x, top.right.y) {
  rasterImage(img, bottom.left.x, bottom.left.y, top.right.x, top.right.y, interpolate=FALSE)
}

RemovePoints <- function(img, percent) {
  ordered <- order(abs(img))
  threshold <- img[ordered[ceiling(length(ordered)*percent/100)]]
  img[img < threshold] <- 0
  img  
}

Threshold <- function(img.dwt, level, percent) {
  for(i in 1:(3 * level))
    img.dwt[[i]] <- RemovePoints(img.dwt[[i]], percent)
  img.dwt
}

DoWaveletTransform <- function(img, wave.name, level, percent) {
  img.dwt <- dwt.2d(img, wave.name, level)
  img.dwt <- Threshold(img.dwt, level, percent)
  reconstruction <- idwt.2d(img.dwt)
  difference <- img - reconstruction
  PlotImages(img, reconstruction, difference, img.dwt, wave.name)
  error <- sum((img-reconstruction)^2)
  error/(dim(img)[1]*dim(img)[2])
}

PlotImages <- function(original, reconstruction, difference, wavelets, wave.name){
  par(mfrow=c(2,2), pty="s")
  png(filename=paste(paste("Plots", wave.name,sep="/"),"original",sep="/"))
  plot.new()
  image(1:dim(original)[1], 1:dim(original)[2], original, xlab="", ylab="", main="Original Image",
        col=grey(seq(0, 1, length = 256)))
  dev.off()
  png(filename=paste(paste("Plots", wave.name,sep="/"),"reconstruction",sep="/"))
  plot.new()
  image(1:dim(original)[1], 1:dim(original)[2], reconstruction, xlab="", ylab="",main="Wavelet Reconstruction",
        col=grey(seq(0, 1, length = 256)))
  dev.off()
  png(filename=paste(paste("Plots", wave.name,sep="/"),"difference",sep="/"))
  plot.new()
  image(1:dim(original)[1], 1:dim(original)[2], difference, xlab="", ylab="", main="Difference",
        col=grey(seq(0, 1, length = 256)))
  dev.off()
  png(filename=paste(paste("Plots", wave.name,sep="/"),"wavelets",sep="/"))
  plot.new()
  interesting_plot <- plot.dwt.2d(wavelets, main="Wavelet Representation", plot=FALSE)
  
  image(1:dim(original)[1], 1:dim(original)[2], interesting_plot, xlab="", ylab="", main="Wavelet Representation",
        col=grey(seq(0, 1, length = 256))) 
  dev.off()
}

ShowErrors = function(errors){
  png(filename="errors.png")
  plot.new()
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  
  plot(errors, ylim=c(0,0.1))
  
  
  legend_wave_names <- c()
  for(i in seq(1, length(wave.names))){
    legend_wave_names[i] <- paste(paste(i, "-"), wave.names[i])
  }
  print(legend_wave_names)
  
  legend("topright" , inset=c(-0.35,0),title="Wavelets Errors", legend_wave_names)
  dev.off()
}

scope <- 1:length(wave.names)
errors <- scope

for( i in scope) {
  errors[i] <- DoWaveletTransform(img, wave.names[i], 3, 90)
}

best.wavelet <- which.min(errors)
worst.wavelet <- which.max(errors)
print(wave.names[best.wavelet])
ShowErrors(errors)