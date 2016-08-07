library(biOps)

kXBase = 48
kYBase = 27
kXYVecMax = kXBase * kYBase *2

CreateDataset <- function(){
  gl_imgs   <- ReadImages("./pika/")
  none_imgs <- ReadImages("./no_pika/")
  gl_df     <- ConvertImagesToFeatureVectors(gl_imgs,   "pika")
  none_df   <- ConvertImagesToFeatureVectors(none_imgs, "none")
  df        <- rbind(gl_df, none_df)
  return(df)  
}

CreateSamples <- function(type){
  gl_imgs   <- ReadImages("./sample_pika/")
  none_imgs <- ReadImages("./sample_no_pika/")

  gl_df     <- ConvertImagesToFeatureVectors(gl_imgs, "pika")
  none_df   <- ConvertImagesToFeatureVectors(none_imgs, "none")
  df        <- rbind(gl_df, none_df)
  return(df)  
}

ReadImages <- function(dir){
  filenames <- list.files(dir, "*.jpg", full.names=T)
  images <- lapply(filenames, readJpeg)
  print(filenames)
  write.table(x = filenames, "trainfiles.csv", quote=F, row.names = FALSE, col.names = FALSE,append=T)
  return(images)
}

DoDownsising <- function(img){
  x_size <- ncol(img)
  y_size <- nrow(img)
  x_scale <- kXBase / x_size
  y_scale <- kYBase / y_size
  img <- imgAverageShrink(img, x=x_scale, y=y_scale)
  return(img)
}


ConvertImageToFeatureVector <- function(img){
  # black: 0, white: 255
  g_img <- imgRGB2Grey(img, coefs=c(0.30, 0.59, 0.11))
  ge_img <- imgCanny(g_img, sigma=0.4)
  ged_img <- DoDownsising(ge_img)
  vec <- as.vector(ged_img)

  ged_img2 <- DoDownsising(g_img)
  vec2 <- as.vector(ged_img2)
  
  vec <- as.vector(rbind(vec,vec2))
  normalized_vec <- vec / 255
  return(normalized_vec)
}

ConvertImagesToFeatureVectors <- function(imgs, label){
  vectors_list <- lapply(imgs, ConvertImageToFeatureVector)
  vectors_list <- lapply(vectors_list, (function(x) {x[1:kXYVecMax]}))
  df <- as.data.frame(do.call("rbind", vectors_list))
  df[is.na(df)] <- 1
  df$label <- as.factor(label)
  return(df)
}

# MAIN

write.table(x = "", "trainfiles.csv", quote=F, row.names = FALSE, col.names = FALSE)

dataset.train <- CreateDataset()
dataset.test  <- CreateSamples()

write.csv(x = dataset.train, file = "train.csv", row.names = FALSE)
write.csv(x = dataset.test,  file = "test.csv" , row.names = FALSE)

