require(jpeg)
require(RCurl)

# saving image url
url <-"https://www.meissl.com/media/images/8f24db1f/schweiz.jpg"

# downloading the image to perform image compression
image <- tempfile()
download.file(url,image,mode="wb")

# reading the image
readImage <- readJPEG(image)

# getting a saving the dimensions of the image
dm <- dim(readImage)

# plotting the original rgb image
rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))
plot(y ~ x, data=rgbImage, main="Switzerland River",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".", axes=FALSE)

# getting the file size in KB
file.info(image)$size/1000

###################################################################################################################
# k-means using 5 as k
kColors <- 5  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

# plotting the k means image using 5 as k
plot(y ~ x, data=rgbImage, main="Switzerland River",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 5 colours")

# writing the compressed jpeg image
k5<-kMeans$centers[kMeans$cluster, ]
dim(k5) <- dm
writeJPEG(k5,"k5.jpg")

# getting the file size in KB
file.info('k5.jpg')$size/1000

###################################################################################################################
# k-means using 15 as k
kColors <- 15  
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

# plotting the k means image using 15 as k
plot(y ~ x, data=rgbImage, main="Switzerland River",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 15 colours")

# writing the compressed jpeg image
k15<-kMeans$centers[kMeans$cluster, ]
dim(k15) <- dm
writeJPEG(k15,"k15.jpg")

# getting the file size in KB
file.info('k15.jpg')$size/1000

###################################################################################################################
# k-means using 2 as k
kColors <- 2
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

# plotting the k means image using 2 as k
plot(y ~ x, data=rgbImage, main="Switzerland River",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 2 colours")

# writing the compressed jpeg image
k2<-kMeans$centers[kMeans$cluster, ]
dim(k2) <- dm
writeJPEG(k2,"k2.jpg")

# getting the file size in KB
file.info('k2.jpg')$size/1000

###################################################################################################################
# k-means using 3 as k
kColors <- 3
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

# plotting the k means image using 50 as k
plot(y ~ x, data=rgbImage, main="Switzerland River",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 3 colours")

# writing the compressed jpeg image
k3<-kMeans$centers[kMeans$cluster, ]
dim(k3) <- dm
writeJPEG(k3,"k3.jpg")

# getting the file size in KB
file.info('k3.jpg')$size/1000

###################################################################################################################
# k-means using 7 as k
kColors <- 7
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])

# plotting the k means image using 7 as k
plot(y ~ x, data=rgbImage, main="Switzerland River",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 7 colours")

# writing the compressed jpeg image
k7<-kMeans$centers[kMeans$cluster, ]
dim(k7) <- dm
writeJPEG(k7,"k7.jpg")

# getting the file size in KB
file.info('k7.jpg')$size/1000

###################################################################################################################
# preparing for PCA image compression
# extracting individual color value matrices
r <- readImage[,,1]
g <- readImage[,,2]
b <- readImage[,,3]

# principal component analysis performed on each color matrix
swiss.r.pca <- prcomp(r, center = FALSE)
swiss.g.pca <- prcomp(g, center = FALSE)
swiss.b.pca <- prcomp(b, center = FALSE)

# collect PCA objects into a list
rgb.pca <- list(swiss.r.pca, swiss.g.pca, swiss.b.pca)

###################################################################################################################
# pca with 3 components
pca.img <- sapply(rgb.pca, function(j) {compressed.img <- j$x[,1:3] %*% t(j$rotation[,1:3])}, simplify = 'array')

# writing compressed jpeg image
writeJPEG(pca.img, "pca3.jpg")

# getting the file size in KB
file.info('pca3.jpg')$size/1000

###################################################################################################################
# pca with 8 components
pca.img <- sapply(rgb.pca, function(j) {compressed.img <- j$x[,1:8] %*% t(j$rotation[,1:8])}, simplify = 'array')

# writing compressed jpeg image
writeJPEG(pca.img, "pca8.jpg")

# getting the file size in KB
file.info('pca8.jpg')$size/1000

###################################################################################################################
# pca with 15 components
pca.img <- sapply(rgb.pca, function(j) {compressed.img <- j$x[,1:15] %*% t(j$rotation[,1:15])}, simplify = 'array')

# writing compressed jpeg image
writeJPEG(pca.img, "pca15.jpg")

# getting the file size in KB
file.info('pca15.jpg')$size/1000

###################################################################################################################
# pca with 35 components
pca.img <- sapply(rgb.pca, function(j) {compressed.img <- j$x[,1:35] %*% t(j$rotation[,1:35])}, simplify = 'array')

# writing compressed jpeg image
writeJPEG(pca.img, "pca35.jpg")

# getting the file size in KB
file.info('pca35.jpg')$size/1000

###################################################################################################################
# pca with 300 components
pca.img <- sapply(rgb.pca, function(j) {compressed.img <- j$x[,1:300] %*% t(j$rotation[,1:300])}, simplify = 'array')

# writing compressed jpeg image
writeJPEG(pca.img, "pca300.jpg")

# getting the file size in KB
file.info('pca300.jpg')$size/1000