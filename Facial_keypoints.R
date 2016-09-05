setwd("~/Documents/Kaggle/Facial Keypoints Detection")
library(doMC)
registerDoMC()

data.dir   <- "~/Documents/Kaggle/Facial Keypoints Detection/"
train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')

# Train Data
d.train <- read.csv(train.file, stringsAsFactors=F)
im.train      <- d.train$Image
d.train$Image <- NULL

# im.train[1] %>% strsplit(., " ") %>% unlist() %>% as.integer()

im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

# Test data
d.test  <- read.csv(test.file, stringsAsFactors=F)
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
d.test$Image <- NULL
# ------------------------------
save(d.train, im.train, d.test, im.test, file='data.Rd')
load("data.Rd")

im <- matrix(data= rev(im.train[1, ]), nrow= 96, ncol= 96)

image(1:96, 1:96, im, col=gray((0:255)/255))

points(96-d.train$nose_tip_x[1],       96-d.train$nose_tip_y[1],         col="red")
points(96-d.train$left_eye_center_x[1],  96-d.train$left_eye_center_y[1],  col="blue")
points(96-d.train$right_eye_center_x[1], 96-d.train$right_eye_center_y[1], col="green")

colMeans(d.train, na.rm=T)

p           <- matrix(data=colMeans(d.train, na.rm=T), nrow=nrow(d.test), ncol=ncol(d.train), byrow=T)
colnames(p) <- names(d.train)
predictions <- data.frame(ImageId = 1:nrow(d.test), p)
head(predictions)
str(predictions)

library(reshape2)
submission <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")

head(submission)

example.submission <- read.csv(paste0(data.dir, 'SampleSubmission.csv'))
sub.col.names      <- names(example.submission)

example.submission$Location <- NULL
submission <- merge(example.submission, submission, all.x=T, sort=F)
submission <- submission[, sub.col.names]
write.csv(submission, file="submission_means.csv", quote=F, row.names=F)




#First learning algorithm patch left eye

coord      <- "left_eye_center"
patch_size <- 20

coord_x <- paste(coord, "x", sep="_")
coord_y <- paste(coord, "y", sep="_")
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
mean.patch <- matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)

image(1:41, 1:41, mean.patch, col=gray((0:255)/255))

image(1:41, 1:41, mean.patch[41:1,41:1], col=gray((0:255)/255))

# Use patch to search 

search_size <- 10

mean_x <- mean(d.train[, coord_x], na.rm=T)
mean_y <- mean(d.train[, coord_y], na.rm=T)
x1     <- as.integer(mean_x)-search_size
x2     <- as.integer(mean_x)+search_size
y1     <- as.integer(mean_y)-search_size
y2     <- as.integer(mean_y)+search_size

params <- expand.grid(x = x1:x2, y = y1:y2)

#

str(im)
im <- matrix(data = im.test[4,], nrow=96, ncol=96)

r  <- foreach(j = 1:nrow(params), .combine=rbind) %dopar% {
  x     <- params$x[j]
  y     <- params$y[j]
  p     <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
  score <- cor(as.vector(p), as.vector(mean.patch))
  score <- ifelse(is.na(score), 0, score)
  data.frame(x, y, score)
}

best <- r[which.max(r$score), c("x", "y")]
best

im.1 <- matrix(data= rev(im.test[4, ]), nrow= 96, ncol= 96)

image(1:96, 1:96, im.1, col=gray((0:255)/255))

points(96-best$x, 96-best$y, col="red")
points(96-best$x, 96-best$y, col="red")

points(96-d.train$left_eye_center_x[1],  96-d.train$left_eye_center_y[1],  col="blue")
points(96-d.train$right_eye_center_x[1], 96-d.train$right_eye_center_y[1], col="green")

library(imageData)
library('png')
library("colorspace")
str(myjpg)
myjpg <- readJPEG(paste0(data.dir, "me.96.jpg"), native = F)
temp <- myjpg[1:96]
temp2 <- myjpg[1:96]
?readJPEG
image(1:96, 1:96, myjpg, col=gray((0:255)/255))
plot(myjpg)
mypng <- readPNG(paste0(data.dir, "me2.png"))
myjpg1 <- myjpg[1:96, , ]
head(myjpg)



RBGtoG <- function(x){
      z <- x[1]*.21 +.72*x[2]+ .07*x[3]
}
library(magrittr)
x <- x %>% RBGtoG

func <- function(x) { 
  return(x[1]+1)}

func(1)

library(plyr)
x <- matrix(c(1,2,3,1,2,3,1,2,3), nrow=3, ncol=3)
x <- array(c(1,2,3), dim=3)
x <- array(x, dim=3)

df.jpg <- adply(myjpg, c(1,2), RBGtoG)

gjp <- df.jpg$V1 %>% as.vector() %>% as.numeric() %>% matrix(., nrow=96, ncol=96)

gjp <- rotate(gjp)

image(1:96, 1:96, gjp, col=gray((0:255)/255))

rotate <- function(x) t(apply(x, 2, rev))

im.1 <- gjp
search_size=10

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
  p     <- im.1[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
  score <- cor(as.vector(p), as.vector(mean.patch))
  score <- ifelse(is.na(score), 0, score)
  data.frame(x, y, score)
}

best <- r[which.max(r$score), c("x", "y")]
best

im.1 <- gjp

image(1:96, 1:96, im.1, col=gray((0:255)/255))

points(best$x, best$y, col="red")


