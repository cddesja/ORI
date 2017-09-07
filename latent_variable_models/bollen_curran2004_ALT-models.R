# Bollen & Curren (2004)
# Example #1 - simulation using ALT model
# based on covariance matrix reported in Rogosa & Willet (1985)
library(lavaan)
n <- 500
lower <- '
.619
.453 .595
.438 .438 .587
.422 .430 .438 .595
.406 .422 .438 .453 .619
'
S <- getCov(lower, names = paste0("t", 1:5))

## Latent trajectory model, i.e., latent growth curve w/ random intercept & random slope ----
lt.model <- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5

# estimate the means
i ~ 1
s ~ 1

# estimate the variances/covariances
i ~~ i
s ~~ s
i ~~ s

# estimate the residual variances
t1 ~~ t1
t2 ~~ t2
t3 ~~ t3
t4 ~~ t4
t5 ~~ t5
'
lt.fit <- lavaan(lt.model, sample.cov = S, sample.nobs = n)
summary(lt.fit, fit.measures = T)
round(lt.fit@implied$cov[[1]], 3)

## Autoregressive model ----
ar.model <- '
t2 ~ t1
t3 ~ t2
t4 ~ t3
t5 ~ t4
t2 ~~ t2
t3 ~~ t3
t4 ~~ t4
t5 ~~ t5
'
ar.fit <- lavaan(ar.model, sample.cov = S, sample.nobs = n)
summary(ar.fit, fit.measures = T)
round(ar.fit@implied$cov[[1]], 3)

## ALT model ---
# Predetermined, i.e., T1 exogenous ----
alt.preT1.model <- '
i =~ 1*t2 + 1*t3 + 1*t4 + 1*t5
s =~ 1*t2 + 2*t3 + 3*t4 + 4*t5

# estimate the means
i ~ 1
s ~ 1
t1 ~ 1

# estimate the variances/covariances
i ~~ i
s ~~ s
i ~~ s
t1 ~~ t1
t1 ~~ i
t1 ~~ s

# estimate the residual variances
t2 ~~ e2*t2
t3 ~~ e3*t3
t4 ~~ e4*t4
t5 ~~ e5*t5

# autoregressive components
t2 ~ p2*t1
t3 ~ p3*t2
t4 ~ p4*t3
t5 ~ p5*t4
'
alt.preT1.fit <- lavaan(alt.preT1.model, sample.cov = S, sample.nobs = n)

# Replicates Table 2
summary(alt.preT1.fit, fit.measures = T)
round(inspect(alt.preT1.fit, "rsquare"), 2)

# Now, fit the latent trajectory model as a reduced ALT model 
# again with T1 predetermined
alt.lt.model <- '
i =~ 1*t2 + 1*t3 + 1*t4 + 1*t5
s =~ 1*t2 + 2*t3 + 3*t4 + 4*t5

# estimate the means
i ~ 1
s ~ 1
t1 ~ 1

# estimate the variances/covariances
i ~~ i
s ~~ s
i ~~ s
t1 ~~ t1
t1 ~~ i
t1 ~~ s

# estimate the residual variances
t2 ~~ e2*t2
t3 ~~ e3*t3
t4 ~~ e4*t4
t5 ~~ e5*t5

# autoregressive components
t2 ~ 0*t1
t3 ~ 0*t2
t4 ~ 0*t3
t5 ~ 0*t4
'
alt.lt.fit <- lavaan(alt.lt.model, sample.cov = S, sample.nobs = n)

# LRT, dfs are slightly different but statistics are identical
# to that present on p. 35
anova(alt.preT1.fit, alt.lt.fit)

# ALT model with T1 as endogenous ----
alt.model <- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5

# estimate the means
i ~ 1
s ~ 1

# estimate the variances/covariances
i ~~ i
s ~~ s
i ~~ s

# estimate the residual variances
t1 ~~ t1
t2 ~~ t2
t3 ~~ t3
t4 ~~ t4
t5 ~~ t5

# autoregressive components
t2 ~ t1
t3 ~ t2
t4 ~ t3
t5 ~ t4
'
alt.fit <- lavaan(alt.model, sample.cov = S, sample.nobs = n)
summary(alt.fit, fit.measures = T)
round(alt.model@implied$cov[[1]], 3)
