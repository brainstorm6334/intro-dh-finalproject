
# adapted from https://www.r-bloggers.com/flag-space-a-scatter-plot-of-raster-images/ | https://gist.github.com/dsparks/3886739#file-raster_scatter-r
# images should be all png, preferably no black and white

# installing needed packages
library(jsonlite)
install.packages("data.table", dependencies=TRUE)
require(data.table)
install.packages("dplyr") 
library(dplyr)

doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
toInstall <- c("png", "devtools", "MASS", "RCurl")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

source_gist("818983")
source_gist("818986")

# extracting the metadata from the json database to a data frame in R 
setwd("/Users/theo/Downloads/reddit-memes-dataset/")

json_file <- jsonlite::fromJSON('db.json')
json_df<- data.table::rbindlist(lapply(json_file, function(x) {
  x[] <- lapply(x, function(y) if(is.data.frame(y)) NA else y)
  x}))
json_df <- data.frame(json_df)

# selecting metadata to keep in the dataframe
json_df <- data.frame(json_df %>% t())
colnames(json_df) <-c('title','thumbnail','created_utc','author','id','ups','downs','media')

json_df$title <- as.character(json_df$title)
json_df$thumbnail <- as.list(json_df$thumbnail)
json_df$media <- as.character(json_df$media)
json_df$author <- as.character(json_df$author)
json_df$created_utc <- as.character(json_df$created_utc)
json_df$id <- as.character(json_df$id)
json_df$ups <- as.integer(json_df$ups)
json_df$downs <- as.integer(json_df$downs)
rownames(json_df)<-1:nrow(json_df)

#ordering the memes by upvotes on reddit and grabbing the top 99 (most upvotes)

top_memes <- arrange(json_df,desc(ups))
top_memes_list <- top_memes[1:99,5]

sink("top_memes_list.txt")
cat(top_memes_list)
sink()

#ordering the memes by upvotes on reddit and grabbing the bottom 99 (least upvotes)

top_memes <- arrange(json_df,ups)
worst_memes_list <- top_memes[1:99,5]

sink("worst_memes_list.txt")
cat(worst_memes_list)
sink()

# read in a list of the files. This can be made easily with cmd+C inside the folder

memefiles <- "/Users/theo/Downloads/reddit-memes-dataset/worst_memes_list.txt"
pngfiles <- scan(memefiles, what="", sep="\n")

# read in the png files

setwd("/Users/theo/Downloads/reddit-memes-dataset/pngfiles")
pngList <- list()
for(ii in 1:99){
  tempName <- paste("meme", ii)
  tempPNG <- readPNG(pngfiles[ii])  # Downloads & loads PNGs
  pngList[[tempName]] <- tempPNG  # And assigns them to a list.
}

# calculating the average red, green, and blue values for all pixels in each.

meanRGB <- t(sapply(pngList, function(ll){
  apply(ll[, , -4], 3, mean)
}))

# The dimensions of each item are equal to the pixel dimensions of the .PNG

meme_dimensions <- t(sapply(pngList, function(ll){
  dim(ll)[1:2]
}))

# Using non-metric multidimensional scaling (MDS) to put similarly-colored flags 
# in similar locations in a two-dimensional space
# https://en.wikipedia.org/wiki/Multidimensional_scaling

meme_distance <- dist(meanRGB)
meme_distance[meme_distance <= 0] <- 1e-10

MDS <- isoMDS(meme_distance)$points
plot(MDS, col = rgb(meanRGB), pch = 20, cex = 5)

# visualize the scatter plot with the raster images included in 2 dimensional space

boxParameter <- 15000   # To alter dimensions of raster image bounding box
par(bg = gray(8/9))
plot(MDS, type = "n", asp = 1)
for(ii in 1:length(pngList)){  # Go through each manga
  tempName <- rownames(MDS)[ii]
  Coords <- MDS[tempName, 1:2]  # Get coordinates from MDS
  Dims <- MangaDimensions[tempName, ]  # Get pixel dimensions
  rasterImage(pngList[[tempName]],  # Plot each manga with these boundaries:
              Coords[1]-Dims[2]/boxParameter, Coords[2]-Dims[1]/boxParameter,
              Coords[1]+Dims[2]/boxParameter, Coords[2]+Dims[1]/boxParameter)
}
