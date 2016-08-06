library(e1071)
library(biOps)

kBaseDir <-"./" 

kXBase = 48
kYBase = 27
kXYVecMax = kXBase * kYBase

# グリッドサーチ用変数
kRoughGammaRange <-c(2^(seq(-15,3,3)))
kRoughCostRange <-c(2^(seq(-5,15,3)))

CreateDataset <- function(){
  gl_imgs <- ReadImages("./pika/")
  none_imgs <- ReadImages("./no_pika/")
  gl_df <- ConvertImagesToFeatureVectors(gl_imgs, "pika")
  none_df <- ConvertImagesToFeatureVectors(none_imgs, "none")
  df <- rbind(gl_df, none_df)
  return(df)  
}

CreateSamples <- function(type){
  if(type == "pika"){
    gl_imgs <- ReadImages("./sample_pika/")
    df <- ConvertImagesToFeatureVectors(gl_imgs, "pika")
    
  } else {
    none_imgs <- ReadImages("./sample_no_pika/")
    df <- ConvertImagesToFeatureVectors(none_imgs, "none")
  }
  df$label <- NULL
  return(df)  
}

ReadImages <- function(dir){
  filenames <- list.files(dir, "*.jpg", full.names=T)
  images <- lapply(filenames, readJpeg)
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


ToGrayscale <- function(img){
  return(imgRGB2Grey(img, coefs=c(0.30, 0.59, 0.11)))
}

EmphasizeEdge <- function(img){
  img <- imgCanny(img, sigma=0.4)
  return(img)
}

ConvertImageToFeatureVector <- function(img){
  g_img <- ToGrayscale(img)
  ge_img <- EmphasizeEdge(g_img)
  ged_img <- DoDownsising(ge_img)
  vec <- as.vector(ged_img)
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

# パラメータチューニング
TuneSVM <- function(dataset, gammas, costs){
  t <- tune.svm(
    label ~ ., 
    data = dataset,
    gamma = gammas,
    cost = costs,
    tunecontrol = tune.control(sampling="cross", cross = 10)
  )
  cat("- best parameters:\n")
  cat("gamma =", t$best.parameters$gamma, "; cost =", t$best.parameters$cost, ";\n")
  cat("accuracy:", 100 - t$best.performance * 100, "%\n\n")
  # best.performance は 誤分類率 なので要注意
  plot(t, transform.x=log2, transform.y=log2)
}

############################################################################
setwd(paste0(kBaseDir,"./"))
dataset <- CreateDataset()

# デフォルトパラメータで確認
model <- svm(label ~ ., data = dataset, cross = 10)
summary(model)
# Total Accuracy: 64.42953

# おおまかにグリッドサーチ
TuneSVM(dataset, kRoughGammaRange, kRoughCostRange)
# - best parameters:
# gamma = 3.051758e-05 ; cost = 128 ;
# accuracy: 70.33333 %

# モデルの生成
model <- svm(
  label ~ .,
  data = dataset,
  gamma = 3.051758^(-5),
  cost = 16
  )

# 新しいデータを分類
gl_samples_df <- CreateSamples("pika")
result <- predict(model, gl_samples_df)
print(result)
# 1       2       3 
# pika pika    none 
# Levels: pika none

none_samples_df <- CreateSamples("none")
result <- predict(model, none_samples_df)
print(result)
# 1       2       3 
# none    none pika 
# Levels: pika none
Contact GitHub API Training Shop Blog About
