setwd("C:/Users/user/OneDrive/바탕 화면/leaRning")

scatter1 <- read.table("Datasets/scatter1.txt", header = T)
attach(scatter1)
head(scatter1)

# Basic Scatter plot
plot(xv, ys)

# Axes labes and Titles
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     main = "A plot of the resposne variable against the explanatory variable")

# Break the line
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     main = "A plot of the resposne variable\n against the explanatory variable")

library(ggplot2)
ggplot(scatter1,aes(xv, ys)) + 
  geom_point(aes(alpha = 0.5), show.legend = F) +
  stat_smooth(method = "lm") +
  labs(title = "Basic ggplot2 scatter plot!") +
  ylab("Outcome") +
  xlab("Covariate")

# Plotting symbols and colors
# default cex = 1
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     pch = 16, cex = 0.6)

# col argument
# col = "color name"
# col = "#RRGGBB"
plot(xv, ys, xlab = "Explanatory variable", ylab = "Response variable",
     pch = 16, cex = 0.6, col = "blue")

# bg = "color name" is a background color for "symbols"
plot(xv, ys, xlab = "Covariate", ylab = "Outcome",
     pch = 21, cex = 0.6, col = "blue", bg = "orange")
title(main = "Scatter plot\nwith the argument of col&bg")

# install.packages("scales")
library(scales)

# hue_pal() function in the "scales" package
plot(xv, ys, xlab = "Covariate", ylab = "Outcome",
     pch = 21, cex = 0.6, 
     col = hue_pal()(2)[1], bg = hue_pal()(2)[2])


# Saving graphics for publication-quality graphics
pdf("myplot.pdf", width = 7, height = 4)

plot(xv, ys, xlab = "Covariate", ylab = "Outcome",
     pch = 21, cex = 0.6, 
     col = hue_pal()(2)[1], bg = hue_pal()(2)[2])

dev.off()
detach(scatter1)

# bmp(), png(), jpeg(), tiff(), and postscript() can do similar things.

# Univariate plots
# Plot name    | Variable type | R function
# Histogram    | Numeric       | hist()
# Density plot | Numeric       | density()
# Boxplot      | Numeric       | box()
# Dotplot      | Numeric       | stripchart()
# Bar chart    | Categorical   | barplot()
# Pie chart    | Categorical   | pie()

# Histograms
daph <- read.table("Datasets/daphnia.txt", header = T)
attach(daph)

hist(Growth.rate, col = hue_pal()(4)[1], main = "")

# Manipulating bins by 'breaks' argument
hist(Growth.rate, breaks = seq(0, 8, 0.25),
     col = hue_pal()(4)[1], main = "")
hist(Growth.rate, breaks = seq(0, 8, 0.5),
     col = hue_pal()(4)[1], main = "")
hist(Growth.rate, breaks = seq(0, 8, 2),
     col = hue_pal()(4)[1], main = "")
hist(Growth.rate, breaks = c(0,1,3,4,8),
     col = hue_pal()(4)[1], main = "")

values <- rpois(1000, 1.7)
hist(values, breaks = (-0.5 : 8.5), col = hue_pal()(4)[1],
     main = "", xlab = "random numbers from a Poisson\nwith mean 1.7")

# Use density on y axis
hist(Growth.rate, col = hue_pal()(4)[1], main = "", freq= F)

# Density plots
plot(density(Growth.rate), col = hue_pal() (4) [1],
     xlab = "Daphnia growth rate", main = "")
polygon ( density (Growth.rate), col = hue_pal () (4) [1],
          xlab = "Daphnia growth rate", main = "")

# Vanilla density function
density(Growth.rate)

# Superimpose the estimated density on a histogram
hist(Growth.rate, col = hue_pal() (4)[1], main="",
     freq=F)
lines(density(Growth.rate))

# Boxplots
boxplot(Growth.rate, col = hue_pal()(4)[1], 
        ylab = "Daphnia growth rate")
boxplot(Growth.rate, col = hue_pal()(4)[1], 
        xlab = "Daphnia growth rate",
        horizontal = T)

new<-data.frame(Growth.rate=c(10,11))
daph.outlied <- rbind(as.data.frame(Growth.rate), new)
daph.outlied

boxplot(daph.outlied, col = "blue", 
        xlab = "Daphnia growth rate")

# Dotplots
caterpillar <- read.table("Datasets/caterpillar.txt", header = T)
stripchart(caterpillar$growth, xlab = "Caterpillar growth",
           col = hue_pal()(2)[1])
stripchart(caterpillar$growth, xlab = "Caterpillar growth",
           col = hue_pal()(2)[2],
           method = 'jitter', jitter = 1, pch = 16, cex = 2)
stripchart(caterpillar$growth, xlab = "Caterpillar growth",
           col = hue_pal()(2)[2],
           method = 'stack', pch = 16, cex = 2)

# Bar charts
hair_eye <- read.table("Datasets/hair_eye.txt", header= T)
table_hair <- table(hair_eye$Hair)
table_hair
barplot(table_hair, col = hue_pal()(4), ylab = "Frequency")

# Pie charts
piedata <- read.csv("Datasets/piedata.csv")
piedata
pie(piedata$amounts, labels = as.character(piedata$names),
    col = hue_pal()(5))

# Bivariate plots
# Cartesian : plot(x, y)
# formula : plot(y ~ x)
attach(scatter1)
names(scatter1)

plot(xv, ys, col = hue_pal()(3)[1], pch = 16, cex = 0.6)

# mapping lines on scatter plot
plot(xv, ys, col = hue_pal()(3)[1], cex=0.6,
     xlab="Covariate", ylab="Outcome")
abline(v=40, col=hue_pal()(3)[2], lwd=3)
plot(xv, ys, col = hue_pal()(3)[1], cex=0.6,
     xlab="Covariate", ylab="Outcome")
abline(lm(ys~xv), col=hue_pal()(3)[2], lwd=3)

scatter2 <- read.table("Datasets/scatter2.txt", header=T)
attach(scatter2)
names(scatter2)

plot(xv, ys, col = hue_pal()(3)[1], cex = 0.6,
     xlab = "Covariate", ylab = "Outcome")
abline(lm(ys ~ xv), lty = 1, lwd = 2)
points(xv2, ys2, col = hue_pal()(3)[2], pch = 16, cex =0.6)
abline(lm(ys2 ~ xv2), lty = 2, lwd = 2)

# Let all of the points from both datasets appear on the plot
plot(c(xv, xv2), c(ys, ys2), xlab = "Covariate", ylab = "Outcome",
     type = "n")
points(xv, ys, col = hue_pal()(3)[1], pch = 16, cex = 0.6)
points(xv2, ys2, col = hue_pal()(3)[2], pch = 16, cex = 0.6)
abline(lm(ys~xv), lty=1, lwd=2)
abline(lm(ys2~xv2), lty=2, lwd=2)

# Arbitrarily set the limits of axes
range(c(xv, xv2))# 0~100
range(c(ys, ys2))# 0~70

plot(xv, ys, xlim=c(0, 100), ylim=c(0, 70),
     xlab="Covariate", ylab="Outcome",
     col=hue_pal()(3)[1], pch=16, cex=0.6)
points(xv2, ys2, col=hue_pal()(3)[2], pch=16, cex=0.6)
abline(lm(ys ~ xv), lty=1, lwd=2)
abline(lm(ys2 ~ xv2), lty=2, lwd=2)

# Adding a legned!
# The locator(1) function can choose the location of legned
# by single mouse click!
legend(locator(1), c("Dataset 1", "Dataset 2"),
       pch=16, col=hue_pal()(3)[1:2])
# Or specify the number in the axes
legend(0, 70, c("Dataset 1", "Dataset 2"), pch = 16,
                col = hue_pal()(3)[1:2])
detach(scatter1)
detach(scatter2)

# Plots with duplicated observations
longdata <- read.table("Datasets/longdata.txt", header=T)
attach(longdata)
names(longdata)

# Simple scatter plot
plot(xlong, ylong, col=hue_pal()(3)[1], xlab="Input", ylab="Count")

# Jittered scatter plot
plot(jitter(xlong, amount=1), 
     jitter(ylong, amount=1),
     xlab="Input", ylab="Count", col=hue_pal()(4)[1])

# Sunflower plot
sunflowerplot(xlong, ylong, xlab="Input", ylab="Count",
              col=hue_pal()(4)[1], size = 0.05)

# Preparing data for the bubble plot
tab_longdata <- table(ylong, xlong)
tab_longdata_df <- as.data.frame(tab_longdata)
tab_longdata_df <- tab_longdata_df[!(tab_longdata_df$Freq == 0),]

# Setting the radius of circles 
# so that area of bubble represents frequency
radius_area <- sqrt(tab_longdata_df$Freq/pi)
symbols(tab_longdata_df$xlong, tab_longdata_df$ylong,
        circles=radius_area, xlab="Input", ylab="Count",
        inches=0.3, bg=hue_pal()(4)[1])

