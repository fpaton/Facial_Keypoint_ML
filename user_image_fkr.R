# User image
image_name <- "me3.jpg"

ImageProc <- function(x){

myjpg <- readJPEG(paste0(data.dir, image_name ), native = F)

RBGtoG <- function(x){
  # Converts an RBG array into a weighted grayscale image (Luminosity)
  # X= array
  z <- x[1]*.21 +.72*x[2]+ .07*x[3]
  }

df.image <- adply(myjpg, c(1,2), RBGtoG)

myimage <- df.image$V1 %>% as.vector() %>% as.numeric() %>% matrix(., nrow=96, ncol=96)

rotate <- function(x) t(apply(x, 2, rev))

myimage_ud <- myimage %>% rotate %>% rotate %>% rotate

return(myimage_ud)
}
######
ud.image <- ImageProc(image_name)


image(1:96, 1:96, ud.image, col=gray((0:255)/255))

# gjp is now upside down grayscale image

ru.image <- ud.image %>% rotate %>% rotate

image(1:96, 1:96, ru.image, col=gray((0:255)/255))

points(96-best.re$x, 96-best.re$y, col= "green")

points(96-best.le$x, 96-best.le$y, col= "green")

user.photo <- gjp