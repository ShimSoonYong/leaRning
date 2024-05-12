setwd("C:/Users/user/OneDrive/바탕 화면/leaRning")
library(PerformanceAnalytics)
taxa <- read.table("datasets/taxonomy.txt", header = T, colClasses = list(Taxon = "factor"))

chart.Correlation(taxa[, 2:8], histogram = T, pch=20)
stars(taxa[, 2:8], locations = c(0, 0), key.loc = c(0, 0),
      radius = F, col.lines = rainbow(120))

#MANOVA, Multivariate ANalysis Of VAriance
plastic <- read.table("datasets/manova.txt", header = T)
head(plastic)
attach(plastic)

plastic_out <- cbind(tear, gloss, opacity)
colnames(plastic_out) <- colnames(plastic)[1:3]
plastic_mod1 <- manova(plastic_out ~ rate * additive)
summary(plastic_mod1)

plastic_mod2 <- manova(plastic_out ~ rate + additive)
summary.aov(plastic_mod2)

#PCA, Principal Component Analysis
pgdata <- read.table("datasets/pgfull.txt", header = T)
names(pgdata)

pgfull <- pgdata[,1:54]
pg_pca10 <- prcomp(pgfull, scale. = T, rank. = 10)
pg_pca10$rotation[,1]
biplot(pg_pca10)

summary(pg_pca10)
barplot(pg_pca10$sdev[1:10]^2, main = "", col = "red", cex.axis = 2, ylab = "")

yv <- predict(pg_pca10)[,1]
yv2 <- predict(pg_pca10)[,2]
plot(pgdata$hay, yv, xlab = "Hay biomass", ylab = "PC 1",
     col = "red", cex.lab = 2)
plot(pgdata$pH, yv2, xlab = "Soil pH", ylab = "PC 2",
     col = "blue", cex.lab = 2)

#Simple PC regression
pcr1 <- lm(yv ~ pgdata$hay)
pcr2 <- lm(yv2 ~ pgdata$pH)
summary(pcr1)
summary(pcr2)
summary(pgdata$hay, pgdata$pH)

plot("Covariates", "PCs", type = "l", xlim = c(0, 10), ylim = c(-10, 10),
     main = "PC Regression")
abline(pcr1)
abline(pcr2)

#Factor Analysis
pg_fact8 <- factanal(pgfull, 8)
loadings(pg_fact8)
barplot(colSums(loadings(pg_fact8)^2), cex.names = 0.5)

#Cluster Analysis
#K-means
kmd <- read.table("datasets/kmeansdata.txt", header=T)
head(kmd)
attach(kmd)
plot(x, y , col = c("red", "blue"))

model4 <- kmeans(kmd[, 1:2], 4)
plot(x, y, col = model4$cluster, xlab = "", ylab = "",
     main = "Clustered Toy data")
points(model4$centers, col = 1:3, pch = 10, cex=2)
legend("topleft", legend = "K=4")

model5 <- kmeans(kmd[, 1:2], 5)
plot(x, y, col = model5$cluster, xlab = "", ylab = "",
     main = "Clustered Toy data")
points(model5$centers, col = 1:3, pch = 10, cex=2)
legend("topleft", legend = "K=5")

model6 <- kmeans(kmd[, 1:2], 6)
plot(x, y, col = model6$cluster, xlab = "", ylab = "",
     main = "Clustered Toy data")
points(model6$centers, col = 1:3, pch = 10, cex=2)
legend("topleft", legend = "K=6")
detach(kmd)

attach(taxa)
head(taxa)

taxa_kn <- kmeans(taxa[, 2:8], 4)
taxa_kn$centers
taxa_kn$cluster

table(taxa$Taxon, taxa_kn$cluster)

#Hierarchical Cluster Analysis
pgdata$plot
pgdata$lime
letters[pgdata$lime]
labels <- paste(pgdata$plot, letters[pgdata$lime], sep = "")
labels

pgdist <- dist(pgdata[,1:54])
hpg <- hclust(pgdist)
plot(hpg, labels = labels, main = "", xlab = "", ylab = "", 
     axes = F, sub = "", cex = 0.6)

plot(hclust(dist(taxa)), main = "", xlab = "", ylab = "", axes = F, sub = "",
     cex = 0.6)

#Discriminant Analysis
library(MASS)
lda_model <- lda(Taxon ~ ., data=taxa)
lda_model

plot(lda_model, 
     col = rep(c("red", "green", "skyblue", "purple"), each = 40))

train <- sort( sample (1:120, 60))
table(taxa$Taxon[train])
attach(taxa)
lda_model2 <- lda (Taxon ~ ., data = taxa, subset = train)
untrained <- taxa[-train, ]
not_train <- predict(lda_model2, untrained)
not_train$class

lda_cm <- table (Taxon[-train], not_train$class)
lda_cm

#Neural Networks
library(nnet)
nn_model <- nnet(Taxon ~ ., data = taxa, subset = train, size = 4, 
                 decay = 1.0e-5, maxit = 200)
nn_cm <- table(Taxon[-train], predict(nn_model, untrained, type ="class"))
nn_cm
