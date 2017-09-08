neuro.dat <- foreign::read.spss("/Users/chrisd/Documents/chocolate_study/data/Chocolate_NeruoReponse.sav", to.data.frame = T)

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

# HFHS
# amygdala
tmp <- subset(neuro.dat, select = c(W1_HFHS_amyg, W2_HFHS_amyg, W3_HFHS_amyg, W4_HFHS_amyg))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# caudate
tmp <- subset(neuro.dat, select = c(W1_HFHS_caudate, W2_HFHS_caudate, W3_HFHS_caudate, W4_HFHS_caudate))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# anterior cingulate cortex
tmp <- subset(neuro.dat, select = c(W1_HFHS_ACC, W2_HFHS_ACC, W3_HFHS_ACC, W4_HFHS_ACC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# medial orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_HFHS_medOFC, W2_HFHS_medOFC, W3_HFHS_medOFC, W4_HFHS_medOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# middle orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_HFHS_midOFC, W2_HFHS_midOFC, W3_HFHS_midOFC, W4_HFHS_midOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# insula
tmp <- subset(neuro.dat, select = c(W1_HFHS_Insula, W2_HFHS_Insula, W3_HFHS_Insula, W4_HFHS_Insula))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# postcentral gyrus
tmp <- subset(neuro.dat, select = c(W1_HFHS_postcentral, W2_HFHS_postcentral, W3_HFHS_postcentral, W4_HFHS_postcentral))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# putamen
tmp <- subset(neuro.dat, select = c(W1_HFHS_put, W2_HFHS_put, W3_HFHS_put, W4_HFHS_put))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# rolandic operculum
tmp <- subset(neuro.dat, select = c(W1_HFHS_Rol, W2_HFHS_Rol, W3_HFHS_Rol, W4_HFHS_Rol))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# thalamus
tmp <- subset(neuro.dat, select = c(W1_HFHS_thal, W2_HFHS_thal, W3_HFHS_thal, W4_HFHS_thal))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# HFLS ----
# amygdala
tmp <- subset(neuro.dat, select = c(W1_HFLS_amyg, W2_HFLS_amyg, W3_HFLS_amyg, W4_HFLS_amyg))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# caudate
tmp <- subset(neuro.dat, select = c(W1_HFLS_caudate, W2_HFLS_caudate, W3_HFLS_caudate, W4_HFLS_caudate))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# anterior cingulate cortex
tmp <- subset(neuro.dat, select = c(W1_HFLS_ACC, W2_HFLS_ACC, W3_HFLS_ACC, W4_HFLS_ACC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# medial orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_HFLS_medOFC, W2_HFLS_medOFC, W3_HFLS_medOFC, W4_HFLS_medOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# middle orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_HFLS_midOFC, W2_HFLS_midOFC, W3_HFLS_midOFC, W4_HFLS_midOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# insula
tmp <- subset(neuro.dat, select = c(W1_HFLS_Insula, W2_HFLS_Insula, W3_HFLS_Insula, W4_HFLS_Insula))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# postcentral gyrus
tmp <- subset(neuro.dat, select = c(W1_HFLS_postcentral, W2_HFLS_postcentral, W3_HFLS_postcentral, W4_HFLS_postcentral))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# putamen
tmp <- subset(neuro.dat, select = c(W1_HFLS_put, W2_HFLS_put, W3_HFLS_put, W4_HFLS_put))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# rolandic operculum
tmp <- subset(neuro.dat, select = c(W1_HFLS_Rol, W2_HFLS_Rol, W3_HFLS_Rol, W4_HFLS_Rol))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# thalamus
tmp <- subset(neuro.dat, select = c(W1_HFLS_thal, W2_HFLS_thal, W3_HFLS_thal, W4_HFLS_thal))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# LFHS ----
# amygdala
tmp <- subset(neuro.dat, select = c(W1_LFHS_amyg, W2_LFHS_amyg, W3_LFHS_amyg, W4_LFHS_amyg))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# caudate
tmp <- subset(neuro.dat, select = c(W1_LFHS_caudate, W2_LFHS_caudate, W3_LFHS_caudate, W4_LFHS_caudate))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# anterior cingulate cortex
tmp <- subset(neuro.dat, select = c(W1_LFHS_ACC, W2_LFHS_ACC, W3_LFHS_ACC, W4_LFHS_ACC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# medial orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_LFHS_medOFC, W2_LFHS_medOFC, W3_LFHS_medOFC, W4_LFHS_medOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# middle orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_LFHS_midOFC, W2_LFHS_midOFC, W3_LFHS_midOFC, W4_LFHS_midOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# insula
tmp <- subset(neuro.dat, select = c(W1_LFHS_Insula, W2_LFHS_Insula, W3_LFHS_Insula, W4_LFHS_Insula))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# postcentral gyrus
tmp <- subset(neuro.dat, select = c(W1_LFHS_postcentral, W2_LFHS_postcentral, W3_LFHS_postcentral, W4_LFHS_postcentral))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# putamen
tmp <- subset(neuro.dat, select = c(W1_LFHS_put, W2_LFHS_put, W3_LFHS_put, W4_LFHS_put))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# rolandic operculum
tmp <- subset(neuro.dat, select = c(W1_LFHS_Rol, W2_LFHS_Rol, W3_LFHS_Rol, W4_LFHS_Rol))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# thalamus
tmp <- subset(neuro.dat, select = c(W1_LFHS_thal, W2_LFHS_thal, W3_LFHS_thal, W4_LFHS_thal))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# LFLS ----
# amygdala
tmp <- subset(neuro.dat, select = c(W1_LFLS_amyg, W2_LFLS_amyg, W3_LFLS_amyg, W4_LFLS_amyg))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# caudate
tmp <- subset(neuro.dat, select = c(W1_LFLS_caudate, W2_LFLS_caudate, W3_LFLS_caudate, W4_LFLS_caudate))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# anterior cingulate cortex
tmp <- subset(neuro.dat, select = c(W1_LFLS_ACC, W2_LFLS_ACC, W3_LFLS_ACC, W4_LFLS_ACC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# medial orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_LFLS_medOFC, W2_LFLS_medOFC, W3_LFLS_medOFC, W4_LFLS_medOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# middle orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_LFLS_midOFC, W2_LFLS_midOFC, W3_LFLS_midOFC, W4_LFLS_midOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# insula
tmp <- subset(neuro.dat, select = c(W1_LFLS_Insula, W2_LFLS_Insula, W3_LFLS_Insula, W4_LFLS_Insula))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# postcentral gyrus
tmp <- subset(neuro.dat, select = c(W1_LFLS_postcentral, W2_LFLS_postcentral, W3_LFLS_postcentral, W4_LFLS_postcentral))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# putamen
tmp <- subset(neuro.dat, select = c(W1_LFLS_put, W2_LFLS_put, W3_LFLS_put, W4_LFLS_put))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# rolandic operculum
tmp <- subset(neuro.dat, select = c(W1_LFLS_Rol, W2_LFLS_Rol, W3_LFLS_Rol, W4_LFLS_Rol))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# thalamus
tmp <- subset(neuro.dat, select = c(W1_LFLS_thal, W2_LFLS_thal, W3_LFLS_thal, W4_LFLS_thal))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# App vs. Unapp ----
# amygdala
tmp <- subset(neuro.dat, select = c(W1_app_unapp_amygd, W2_app_unapp_amygd, W3_app_unapp_amygd, W4_app_unapp_amygd))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# caudate
tmp <- subset(neuro.dat, select = c(W1_app_unapp_caudate, W2_app_unapp_caudate, W3_app_unapp_caudate, W4_app_unapp_caudate))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# anterior cingulate cortex
tmp <- subset(neuro.dat, select = c(W1_app_unapp_ACC, W2_app_unapp_ACC, W3_app_unapp_ACC, W4_app_unapp_ACC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# medial orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_app_unapp_medOFC, W2_app_unapp_medOFC, W3_app_unapp_medOFC, W4_app_unapp_medOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# middle orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_app_unapp_midOFC, W2_app_unapp_midOFC, W3_app_unapp_midOFC, W4_app_unapp_midOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# insula
tmp <- subset(neuro.dat, select = c(W1_app_unapp_Insula, W2_app_unapp_Insula, W3_app_unapp_Insula, W4_app_unapp_Insula))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# postcentral gyrus
tmp <- subset(neuro.dat, select = c(W1_app_unapp_postcentral, W2_app_unapp_postcentral, W3_app_unapp_postcentral, W4_app_unapp_postcentral))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# putamen
tmp <- subset(neuro.dat, select = c(W1_app_unapp_put, W2_app_unapp_put, W3_app_unapp_put, W4_app_unapp_put))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# rolandic operculum
tmp <- subset(neuro.dat, select = c(W1_app_unapp_Rol, W2_app_unapp_Rol, W3_app_unapp_Rol, W4_app_unapp_Rol))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# thalamus
tmp <- subset(neuro.dat, select = c(W1_app_unapp_thal, W2_app_unapp_thal, W3_app_unapp_thal, W4_app_unapp_thal))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# App vs. Water ----
# amygdala
tmp <- subset(neuro.dat, select = c(W1_app_water_amygd, W2_app_water_amygd, W3_app_water_amygd, W4_app_water_amygd))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# caudate
tmp <- subset(neuro.dat, select = c(W1_app_water_caudate, W2_app_water_caudate, W3_app_water_caudate, W4_app_water_caudate))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# anterior cingulate cortex
tmp <- subset(neuro.dat, select = c(W1_app_water_ACC, W2_app_water_ACC, W3_app_water_ACC, W4_app_water_ACC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# medial orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_app_water_medOFC, W2_app_water_medOFC, W3_app_water_medOFC, W4_app_water_medOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# middle orbitofrontal cortex
tmp <- subset(neuro.dat, select = c(W1_app_water_midOFC, W2_app_water_midOFC, W3_app_water_midOFC, W4_app_water_midOFC))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# insula
tmp <- subset(neuro.dat, select = c(W1_app_water_Insula, W2_app_water_Insula, W3_app_water_Insula, W4_app_water_Insula))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# postcentral gyrus
tmp <- subset(neuro.dat, select = c(W1_app_water_postcentral, W2_app_water_postcentral, W3_app_water_postcentral, W4_app_water_postcentral))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# putamen
tmp <- subset(neuro.dat, select = c(W1_app_water_put, W2_app_water_put, W3_app_water_put, W4_app_water_put))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# rolandic operculum
tmp <- subset(neuro.dat, select = c(W1_app_water_Rol, W2_app_water_Rol, W3_app_water_Rol, W4_app_water_Rol))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)

# thalamus
tmp <- subset(neuro.dat, select = c(W1_app_water_thal, W2_app_water_thal, W3_app_water_thal, W4_app_water_thal))
cor(tmp, use = "pairwise.complete.obs")
pairs(tmp, upper.panel = panel.cor, diag.panel = panel.hist, lower.panel = panel.smooth)



