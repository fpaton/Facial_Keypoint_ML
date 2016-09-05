# Facial rec

findbest <- function(xpass){

coord <- "right_eye_center"
coord <- xpass
patch_size  <-  10
search_size <- 10

coord_x <- paste(coord, "x", sep= "_")
coord_y <- paste(coord, "y", sep= "_")

patches <- foreach (i = 1:nrow(d.train), .combine=rbind) %do% {
  im  <- matrix(data = im.train[i,], nrow=96, ncol=96) # image in pixels
  x   <- d.train[i, coord_x] # 
  y   <- d.train[i, coord_y]
  x1  <- (x-patch_size)
  x2  <- (x+patch_size)
  y1  <- (y-patch_size)
  y2  <- (y+patch_size)
  if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
  {
    as.vector(im[x1:x2, y1:y2])
  }
  else
  {
    NULL
  }
}

# mean.patch <- matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)

# image(1:21, 1:21, mean.patch[21:1,21:1], col=gray((0:255)/255))

# Use patch to search 
# search_size <- 10

mean_x <- mean(d.train[, coord_x], na.rm=T)
mean_y <- mean(d.train[, coord_y], na.rm=T)
x1     <- as.integer(mean_x)-search_size
x2     <- as.integer(mean_x)+search_size
y1     <- as.integer(mean_y)-search_size
y2     <- as.integer(mean_y)+search_size

params <- expand.grid(x = x1:x2, y = y1:y2)

r  <- foreach(j = 1:nrow(params), .combine=rbind) %dopar% {
  x     <- params$x[j]
  y     <- params$y[j]
  p     <- ud.image[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)] #kConstant for photo
  score <- cor(as.vector(p), as.vector(mean.patch))
  score <- ifelse(is.na(score), 0, score)
  data.frame(x, y, score)
  }

best <- r[which.max(r$score), c("x", "y")]

return(best)

}


