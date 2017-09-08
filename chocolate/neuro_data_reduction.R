# Load libraries
library(psych)
library(FactoMineR)

# Helper functions for the scatterplot matrix
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Read in the data
neuro.dat <- foreign::read.spss("/Users/chrisd/Documents/chocolate_study/data/Chocolate_NeruoReponse.sav", to.data.frame = T)

## High Fat/High Sugar ----
# wave 1
hfhs.w1 <- data.frame(subset(neuro.dat, select = c(W1_HFHS_amyg:W1_HFHS_thal)))
hfhs.w1 <- scale(hfhs.w1)
hfhs.w1 <- na.omit(hfhs.w1)
pairs(hfhs.w1, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
scree(hfhs.w1, factors = F)

# pca
pca.w1 <- PCA(hfhs.w1)
summary(pca.w1)
# W1_HFHS_medOFC doesn't really load on component 1, try dropping it and see
# what we find?
pca.w1.sub <- hfhs.w1[,-c(4)]
par(mfrow = c(1,2))
scree(hfhs.w1, factors = F)
scree(pca.w1.sub, factors = F)
par(mfrow = c(1,1))
pca.w1 <- PCA(pca.w1.sub)
summary(pca.w1)
pca.w1$eig
hfhs.w1.scores <- pca.w1$ind$coord[,1]
hfhs.w1.id <- names(pca.w1$ind$coord[,1])
hfhs.scores <- data.frame(ID = hfhs.w1.id, wave1 = hfhs.w1.scores)
hfhs.scores <- data.frame(ID = hfhs.w1.id, wave1 = rowSums(pca.w1.sub))

# wave 2
hfhs.w2 <- data.frame(subset(neuro.dat, select = c(W2_HFHS_amyg:W2_HFHS_thal)))
hfhs.w2 <- scale(hfhs.w2)
hfhs.w2 <- na.omit(hfhs.w2)
pairs(hfhs.w2, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
scree(hfhs.w2, factors = F)

# pca
pca.w2 <- PCA(hfhs.w2)
summary(pca.w2)
# W2_HFHS_medOFC doesn't really load on component 1, try dropping it and see
# what we find?
pca.w2.sub <- hfhs.w2[,-c(4)]
par(mfrow = c(1,2))
scree(hfhs.w2, factors = F)
scree(pca.w2.sub, factors = F)
par(mfrow = c(1,1))
pca.w2 <- PCA(pca.w2.sub)
str(pca.w2)
pca.w2$ind$coord[,1]
summary(pca.w2)
pca.w2$eig
hfhs.w2.scores <- pca.w2$ind$coord[,1]
hfhs.w2.id <- names(pca.w2$ind$coord[,1])
hfhs.wave2 <- data.frame(ID = hfhs.w2.id, wave2 = hfhs.w2.scores)
hfhs.wave2 <- data.frame(ID = hfhs.w2.id, wave2 = rowSums(pca.w2.sub))
hfhs.scores <- merge(hfhs.scores, hfhs.wave2, by = "ID", all = T)
cor(hfhs.scores[,-1], use = "complete.obs")

# wave 3
hfhs.w3 <- data.frame(subset(neuro.dat, select = c(W3_HFHS_amyg:W3_HFHS_thal)))
hfhs.w3 <- scale(hfhs.w3)
hfhs.w3 <- na.omit(hfhs.w3)
pairs(hfhs.w3, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
scree(hfhs.w3, factors = F)

# pca
pca.w3 <- PCA(hfhs.w3)
summary(pca.w3)
# W3_HFHS_medOFC doesn't really load on component 1, try dropping it and see
# what we find?
pca.w3.sub <- hfhs.w3[,-c(4)]
par(mfrow = c(1,2))
scree(hfhs.w3, factors = F)
scree(pca.w3.sub, factors = F)
par(mfrow = c(1,1))
pca.w3 <- PCA(pca.w3.sub)
summary(pca.w3)
pca.w3$eig
hfhs.w3.scores <- pca.w3$ind$coord[,1]
hfhs.w3.id <- names(pca.w3$ind$coord[,1])
hfhs.wave3 <- data.frame(ID = hfhs.w3.id, wave3 = hfhs.w3.scores)
hfhs.wave3 <- data.frame(ID = hfhs.w3.id, wave3 = rowSums(pca.w3.sub))
hfhs.scores <- merge(hfhs.scores, hfhs.wave3, by = "ID", all = T)
cor(hfhs.scores[,-1], use = "complete.obs")

# wave 4
hfhs.w4 <- data.frame(subset(neuro.dat, select = c(W4_HFHS_amyg:W4_HFHS_thal)))
hfhs.w4 <- scale(hfhs.w4)
hfhs.w4 <- na.omit(hfhs.w4)
pairs(hfhs.w4, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
scree(hfhs.w4, factors = F)




# pca
pca.w4 <- PCA(hfhs.w4)
summary(pca.w4)
# W4_HFHS_medOFC doesn't really load on component 1, try dropping it and see
# what we find?
pca.w4.sub <- hfhs.w4[,-c(4)]
par(mfrow = c(1,2))
scree(hfhs.w4, factors = F)
scree(pca.w4.sub, factors = F)
par(mfrow = c(1,1))
pca.w4 <- PCA(pca.w4.sub)
summary(pca.w4)
pca.w4$eig
alpha(pca.w1.sub);alpha(pca.w2.sub); alpha(pca.w3.sub); alpha(pca.w4.sub)

## High Fat/Low Sugar ----
# wave 1
hfls.w1 <- data.frame(subset(neuro.dat, select = c(W1_HFLS_amyg:W1_HFLS_thal)))
hfls.w1 <- scale(hfls.w1)
hfls.w1 <- na.omit(hfls.w1)
pairs(hfls.w1, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
scree(hfls.w1, factors = F)

# pca
pca.w1 <- PCA(hfls.w1)
summary(pca.w1)
# W1_HFLS_medOFC doesn't really load on component 1, try dropping it and see
# what we find?
pca.w1.sub <- hfls.w1[,-c(4)]
par(mfrow = c(1,2))
scree(hfls.w1, factors = F)
scree(pca.w1.sub, factors = F)
par(mfrow = c(1,1))
pca.w1 <- PCA(pca.w1.sub)
summary(pca.w1)
pca.w1$eig

# wave 2
hfls.w2 <- data.frame(subset(neuro.dat, select = c(W2_HFLS_amyg:W2_HFLS_thal)))
hfls.w2 <- scale(hfls.w2)
hfls.w2 <- na.omit(hfls.w2)
pairs(hfls.w2, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
scree(hfls.w2, factors = F)

# pca
pca.w2 <- PCA(hfls.w2)
summary(pca.w2)
# W2_HFLS_medOFC doesn't really load on component 1, try dropping it and see
# what we find?
pca.w2.sub <- hfls.w2[,-c(4)]
par(mfrow = c(1,2))
scree(hfls.w2, factors = F)
scree(pca.w2.sub, factors = F)
par(mfrow = c(1,1))
pca.w2 <- PCA(pca.w2.sub)
summary(pca.w2)
pca.w2$eig

# wave 3
hfls.w3 <- data.frame(subset(neuro.dat, select = c(W3_HFLS_amyg:W3_HFLS_thal)))
hfls.w3 <- scale(hfls.w3)
hfls.w3 <- na.omit(hfls.w3)
pairs(hfls.w3, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
scree(hfls.w3, factors = F)

# pca
pca.w3 <- PCA(hfls.w3)
summary(pca.w3)
# W3_HFLS_medOFC doesn't really load on component 1, try dropping it and see
# what we find?
pca.w3.sub <- hfls.w3[,-c(4)]
par(mfrow = c(1,2))
scree(hfls.w3, factors = F)
scree(pca.w3.sub, factors = F)
par(mfrow = c(1,1))
pca.w3 <- PCA(pca.w3.sub)
summary(pca.w3)
pca.w3$eig

# wave 4
hfls.w4 <- data.frame(subset(neuro.dat, select = c(W4_HFLS_amyg:W4_HFLS_thal)))
hfls.w4 <- scale(hfls.w4)
hfls.w4 <- na.omit(hfls.w4)
pairs(hfls.w4, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)
scree(hfls.w4, factors = F)

# pca
pca.w4 <- PCA(hfls.w4)
summary(pca.w4)
# W4_HFLS_medOFC doesn't really load on component 1, try dropping it and see
# what we find?
pca.w4.sub <- hfls.w4[,-c(4)]
par(mfrow = c(1,2))
scree(hfls.w4, factors = F)
scree(pca.w4.sub, factors = F)
par(mfrow = c(1,1))
pca.w4 <- PCA(pca.w4.sub)
summary(pca.w4)
pca.w4$eig

alpha(pca.w1.sub);alpha(pca.w2.sub); alpha(pca.w3.sub); alpha(pca.w4.sub)
