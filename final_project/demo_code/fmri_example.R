# This file loads and looks at some of the data sets for the
# STAT215A Fall 2015 final project.
library(corrplot)
library(ggplot2)
library(reshape2)
source('utils.R')

# Load the data.
load("fMRIdata.RData")
ls()

# Read in a raw image.
img1 <- ReadImage(1)
image(img1, col=gray((1:500) / 501))

# Load in a raw basis function.
wav1 <- ReadRealBasisFunction(150)
image(wav1)

# Take a look at the distribution of responses.
resp.dat <- data.frame(resp_dat)
names(resp.dat)  <- paste("voxel", 1:ncol(resp.dat), sep="")
rm(resp_dat)
resp.melt <- melt(resp.dat)
ggplot(resp.melt) +
  geom_density(aes(x=value)) +
  facet_grid(variable ~ .)
corrplot(cor(resp.dat))

# Look at the first image's feature distribution.
fit.feat <- data.frame(fit_feat)
rm(fit_feat)
qplot(x=as.numeric(fit.feat[1, ]), geom="density")


# Look at the validation set.
dim(val_feat)


