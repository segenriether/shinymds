otu <- read.csv("otu_table_rarefied.csv", header=TRUE)
otu$X <- NULL
otu <- otu[1:100,]

library(vegan)

dist=vegdist(t(otu),method="bray", upper=TRUE)
tail(dist)

write.csv(as.matrix(dist), "dist.csv")

dist <- read.csv("dist", header=FALSE)

mds <- metaMDS(dist)
plot(mds)

data.scores <- as.data.frame(scores(mds))
data.scores$site <- rownames(data.scores)

write.csv(data.scores, "scores.csv")

withmap <- read.csv("scores.csv", header=TRUE)

library(ggplot2)

ggplot() +
  geom_text(data=withmap, aes(x=NMDS1,y=NMDS2,label=crop),alpha=0.5) +
  geom_point(data=withmap, aes(x=NMDS1,y=NMDS2,shape=trt_type,color=site_ID),size=3) +
  coord_equal() +
  theme_classic()
