library("FactoMineR")
library("readxl")
library("ggpubr")
library('factoextra')
library(caret)
library(mclust)

data= read_excel(choose.files())
data


x=data[0:15]
x

y= as.matrix(data[16])


#PCA + Kmean

x.pca <- PCA(x, graph = TRUE)

pca_projection = x.pca$ind$coord

res.km <- kmeans(pca_projection, 2)
res.km$cluster

fviz_cluster(res.km, x.pca$ind$coord,
  palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
  geom = "point",
  ellipse.type = "convex", 
  ggtheme = theme_bw())

table(y,res.km$cluster)


################################mclust###########

res=Mclust(x)
res$classification

table(y,res$classification)

############################PCA + HCA#######

D=dist(x, method = "euclidean")
h = hclust(D^2, "ward.D2")
plot(h,hang=-1,label=FALSE)
plot(h,hang=-1)
cut = cutree(h,2)
plot(pca_projection,col= cut)
pca_hca_conf = table(cut,y)
pca_hca_conf

######################## PCA + Spherical Kmeans#############
install.packages("skmeans")
library(R.matlab)
library(skmeans)
skm.res <- skmeans(pca_projection, 2)
plot(pca_projection,col=skm.res$cluster,pch="+")
pca_skm_conf = table(skm.res$cluster,y)
pca_skm_conf


####################### Autoencoders ###############
install.packages("autoencoder")
install.packages("ANN2")
library("autoencoder")
library(ANN2)
mat = as.matrix(x)
AE <- autoencoder(mat, c(10,2,10), loss.type = 'pseudo-huber',
                  activ.functions = c('tanh','linear','tanh'),
                  batch.size = 8, optim.type = 'adam',
                  n.epochs = 1000, val.prop = 0)
plot(AE)
reconstruction_plot(AE, mat)


