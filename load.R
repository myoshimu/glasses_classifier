library(biOps)

# 縮小サイズ
kXBase = 48
kYBase = 27
kXYVecMax = kXBase * kYBase

CreateDataset <- function(){
  # 画像オブジェクト生成"
  gl_imgs   <- ReadImages("./pika/")
  none_imgs <- ReadImages("./no_pika/")
  # 素性ベクトル作成"
  gl_df     <- ConvertImagesToFeatureVectors(gl_imgs,   "pika")
  none_df   <- ConvertImagesToFeatureVectors(none_imgs, "none")
  # データセット作成"
  df        <- rbind(gl_df, none_df)
  return(df)  
}

CreateSamples <- function(type){
  gl_imgs   <- ReadImages("./sample_pika/")
  none_imgs <- ReadImages("./sample_no_pika/")

  gl_df     <- ConvertImagesToFeatureVectors(gl_imgs, "pika")
  none_df   <- ConvertImagesToFeatureVectors(none_imgs, "none")
  # データセット作成"
  df        <- rbind(gl_df, none_df)
  return(df)  
}

ReadImages <- function(dir){
  print(dir)
  filenames <- list.files(dir, "*.jpg", full.names=T)
  images <- lapply(filenames, readJpeg)
  print(filenames)
  return(images)
}

# 読み込んだ jpg を kXBase x kYBase にダウンサイジング
DoDownsising <- function(img){
  x_size <- ncol(img)
  y_size <- nrow(img)
  x_scale <- kXBase / x_size
  y_scale <- kYBase / y_size
  img <- imgAverageShrink(img, x=x_scale, y=y_scale)
  return(img)
}


# 読み込んだ jpg を グレースケール変換
ToGrayscale <- function(img){
  return(imgRGB2Grey(img, coefs=c(0.30, 0.59, 0.11)))
}

# 読み込んだ jpg を エッジ強調
EmphasizeEdge <- function(img){
  img <- imgCanny(img, sigma=0.4)
  return(img)
}

# 素性ベクトル作成
ConvertImageToFeatureVector <- function(img){
  # 1. 元画像
  # 2. グレースケール変換
  # black: 0, white: 255
  g_img <- ToGrayscale(img)
  # 3. エッジ強調
  ge_img <- EmphasizeEdge(g_img)
  # 4. ダウンサイジング
  ged_img <- DoDownsising(ge_img)
  vec <- as.vector(ged_img)
  # black: 0 to white: 1 
  normalized_vec <- vec / 255
  return(normalized_vec)
}

# 全イメージを素性ベクトルに変換
ConvertImagesToFeatureVectors <- function(imgs, label){
  vectors_list <- lapply(imgs, ConvertImageToFeatureVector)
  vectors_list <- lapply(vectors_list, (function(x) {x[1:kXYVecMax]}))
  df <- as.data.frame(do.call("rbind", vectors_list))
  df[is.na(df)] <- 1
  df$label <- as.factor(label)
  return(df)
}

# MAIN
dataset.train <- CreateDataset()
dataset.test  <- CreateSamples()

write.csv(x = dataset.train, file = "train.csv", row.names = FALSE)
write.csv(x = dataset.test,  file = "test.csv" , row.names = FALSE)