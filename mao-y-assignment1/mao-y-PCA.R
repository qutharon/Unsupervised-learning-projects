pewwork <-read.delim("mao-y-data.txt", sep="\t")

# One-hot encoding
library(caret)
dummy <- dummyVars(" ~ .", data = pewwork)
pewdf <- data.frame(predict(dummy, newdata = pewwork))
pewdf <- pewdf[,-116] # substract the all 0 column

# standard PCA
std_pca <- prcomp(pewdf, scale = FALSE)

# View results
summary(std_pca)

# Principal component scores
scores <- std_pca$x
head(scores)

# Variance explained by each component
var_explained <- std_pca$sdev^2 / sum(std_pca$sdev^2)

# logistic PCA
library(logisticPCA)

# cross validation to determine k and m. This takes quite some time to run, so commented out.
# logpca_cv = cv.lpca(pewdf, ks = seq(2,10,2), ms = 1:10)
# plot(logpca_cv)
# m=4
explained_variance = c()
for (i in 1:30) {
  log_pca = logisticPCA(pewdf, k = i, m = 4)
  explained_variance <- c(explained_variance,log_pca$prop_deviance_expl-sum(explained_variance))
}
k = 1:30
jpeg('screeplot.jpg')
plot(k, var_explained[1:30], pch = 16, main = "PCA vs logistic PCA Screeplot", xlab = "components", ylab = "%variance", col = 'blue')
points(k, explained_variance, pch = 16, col = 'red')
legend("right",c("PCA","Logistic PCA"), pch = c(16,16), col = c("blue", "red"))
dev.off()

majority = c()
for (i in 1:ncol(pewdf)) {
  maj = mean(pewdf[,i]) >0.5
  majority <- c(majority, maj)
}
sum(abs(pewdf[409,] - majority))
sum(abs(pewdf[1050,] - majority))
sum(abs(pewdf[131,] - majority))
deviation = apply(pewdf, 1, function(col) sum(abs(col - majority)))
sorted <- sort(deviation, decreasing = TRUE, index.return = TRUE)

# plot PC1 and 2
pcaData <- as.data.frame(std_pca$x[, 1:2])
colnames(pcaData) <- c("PC1", "PC2")
std_biplot <- ggplot(pcaData) +
  aes(PC1, PC2) + 
  geom_point(size = 2) + 
  coord_fixed() +
  labs(title="Standard PCA") +
  annotate("point", x = pcaData[sorted$ix[1:3],1], 
           y = pcaData[sorted$ix[1:3],2], color = "blue") +
  annotate("point", x = pcaData[sorted$ix[4:30],1], 
           y = pcaData[sorted$ix[4:30],2], color = "red")
ggsave(std_biplot, file="standard_biplot.jpg")
lpcaData <- as.data.frame(log_pca$PCs[, 1:2])
colnames(lpcaData) <- c("PC1", "PC2")
log_biplot <- ggplot(lpcaData) +
  aes(PC1, PC2) + 
  geom_point(size = 2) + 
  coord_fixed() +
  labs(title="Logistic PCA") +
  annotate("point", x = lpcaData[sorted$ix[1:3],1], 
           y = lpcaData[sorted$ix[1:3],2], color = "blue") +
  annotate("point", x = lpcaData[sorted$ix[4:30],1], 
           y = lpcaData[sorted$ix[4:30],2], color = "red")
ggsave(log_biplot, file="logistic_biplot.jpg")

# plot PC loadings 
jpeg('standard_loadings.jpg')
heatmap(std_pca$rotation[,1:64], 
        col = gray.colors(256, start = 0, end = 1),
        Rowv = NA, Colv = NA, 
        main = "Standard PCA principal component loadings")
dev.off()

log_pca = logisticPCA(pewdf, k = 70, m = 4)
rownames(log_pca$U) <- rownames(std_pca$rotation)

jpeg('logistic_loadings.jpg')
heatmap(log_pca$U, 
        col = gray.colors(256, start = 0, end = 1),
        Rowv = NA, Colv = NA, 
        scale = "none",
        main = "Logistic PCA principal component loadings")
dev.off()