

# Calling functions and Updating parameters
ud.image <- ImageProc(image_name)

best.re <- findbest("right_eye_center")

best.le <- findbest("left_eye_center")

best.n  <- findbest("nose_tip")

str(ud.image)

# Graphics: 

image(1:96, 1:96, ud.image, col=gray((0:255)/255))

points(best.re$x, best.re$y, col= "green")
points(best.le$x, best.le$y, col= "green")
points(best.n$x, best.n$y, col='red')



points(96-best.re$x, 96-best.re$y, col= "green")

points(96-best.le$x, 96-best.le$y, col= "green")

points(96-best.n$x, 96-best.n$y, col= "red")

text(96-best.re$x, 96-best.re$y +10, labels = "left eye", col="green")
text(96-best.n$x, 96-best.n$y +10, labels = "right eye", col="green")

# Tests
best.re.test <- findbest(10, 2, "right_eye_center", ud.image)

best.le.test <- findbest(10, 2, "left_eye_center", myimage_ud )
best.n.test  <- findbest(10, 2, "nose_tip", myimage_ud)

im.1 <- matrix(data= rev(im.test[6, ]), nrow= 96, ncol= 96)

image(1:96, 1:96, ud.image, col=gray((0:255)/255))
points(68, 39)

r  <- foreach(j = 1:nrow(params), .combine=rbind) %dopar% {
  x     <- params$x[j]
  y     <- params$y[j]
  p     <- myimage_ud[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)] #kConstant for photo
  score <- cor(as.vector(p), as.vector(mean.patch))
  score <- ifelse(is.na(score), 0, score)
  data.frame(x, y, score)
}