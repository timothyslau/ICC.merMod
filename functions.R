###################################################
### compute ICC in lme4
###################################################
# FINISHED: Negative Binomial, Binomial, Poisson, & Gaussian
# UNFINISHED: Gamma & Inverse Gaussian

# use
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q3/022538.html
# for example interpretation

# Negative Binomial ICC lme4 (numerator can also be a list of values)
# function based off of http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3916583/
ICC.NB <- function(model, numerator){
  require(lme4)
  mout <- data.frame(VarCorr(model)) # random intercept model variances
  sigma_a2 <- sum(mout[mout$grp %in% numerator, "vcov"]) # random effect(s) in numerator
  sigma_2 <- sum(mout["vcov"]) # sum of random effects variance in denominator
  beta <- as.numeric(fixef(model)["(Intercept)"]) # fixed effect intercept
  r <- getME(object = model, "glmer.nb.theta") # theta
  icc <- (exp(sigma_a2) - 1) / ((exp(sigma_2) - 1) + (exp(sigma_2) / r) + (-beta - (sigma_2 / 2)))
  return(icc)
}
# example (cid = a school)
ICC.NB(model = glmer.nb(formula = awards ~ 1 + (1 | cid), data = foreign::read.dta(file = "http://www.ats.ucla.edu/stat/data/hsbdemo.dta")), numerator = "cid")


# Binomial ICC lme4 (numerator can also be a list of values)
# function based off of http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3426610/
ICC.BIN <- function(model, numerator){
  require(lme4)
  mout <- data.frame(VarCorr(model)) # random intercept model variances
  level1 <- pi^2 / 3 # level 1 variance of all binomial models
  sigma_a2 <- sum(mout[mout$grp %in% numerator,"vcov"]) # random effect(s) in numerator
  sigma_2 <- sum(data.frame(VarCorr(model))[,"vcov"], level1) # sum of random effects variance in denominator
  icc <- sigma_a2 / sigma_2
  return(icc)
}

# example (herd = a herd of bovine)
ICC.BIN(model = glmer(formula = cbind(incidence, size - incidence) ~ 1 + (1 | herd), data = cbpp, family = binomial), numerator = "herd")


# Gaussian ICC lme4 (numerator can also be a list of values)
# function based off of http://www.ncbi.nlm.nih.gov/pmc/articles/PMC1540459/
ICC.GAU <- function(model, numerator){
  require(lme4)
  mout <- data.frame(VarCorr(model)) # random intercept model variances
  sigma_a2 <- sum(mout[mout$grp %in% numerator, "vcov"]) # random effect(s) in numerator
  sigma_2 <- sum(mout["vcov"]) # sum of random effects variance in denominator
  icc <- sigma_a2 / sigma_2
  return(icc)
}
# example (Subject = an individual person)
ICC.GAU(model = lmer(formula = Reaction ~ 1 + (1 | Subject), data = sleepstudy), numerator = "Subject")


# Poisson ICC lme4 (numerator can also be a list of values)
# function based off of p.22 http://www.ssicentral.com/supermix/Documentation/count_final.pdf
ICC.POI <- function(model, numerator){
  require(lme4)
  mout <- data.frame(VarCorr(model)) # random intercept model variances
  sigma_a2 <- sum(mout[mout$grp %in% numerator, "vcov"]) # random effect(s) in numerator
  sigma_2 <- sum(mout["vcov"]) # sum of random effects variance in denominator
  icc <- sigma_a2 / (1 + sigma_2)
  return(icc)
}
# example (cid = schools)
ICC.POI(model = glmer(formula = awards ~ 1 + (1 | cid), family = poisson, data = foreign::read.dta(file = "http://www.ats.ucla.edu/stat/data/hsbdemo.dta")), numerator = "cid")
