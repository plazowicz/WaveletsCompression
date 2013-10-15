library(png)
library(waveslim)

wave.names <- c("haar", "d4", "mb4",
               "w4", "bs3.1", "fk4",
               "d6", "fk6", "d8",
               "fk8", "la8", "mb8",
               "bl14", "fk14", "d16",
               "la16", "mb16", "la20",
               "bl20", "fk22", "mb24")

path = "Data/Durer.png"

img <- readPNG(path)
plot(0:2, 0:2, type='n')

Display <- function(img, bottom.left.x, bottom.left.y, top.right.x, top.right.y) {
  rasterImage(img, bottom.left.x, bottom.left.y, top.right.x, top.right.y, interpolate=FALSE)
}

Treshold <- function(img, percent) {
  ordered <- order(abs(img))
}

RemovePoints <- function(img, percent) {
  
}

DoWaveletTransform <- function(img, wave.name, level, percent) {
  img.dwt <- dwt.2d(img, wave.name, level)
    
}


Display(img, 0, 1, 1, 2)