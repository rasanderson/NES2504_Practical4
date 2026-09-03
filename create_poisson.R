# Want to create dataset obviously unsuitable for lm but ok for poisson

library(mosaic)
sppcount_dat <- read.csv("Data/sppcount.csv")
gf_point(species ~ pollution, data = sppcount_dat)
sppcount_glm1 <- glm(species ~ pollution, data = sppcount_dat, family=poisson)
summary(sppcount_glm1)

pollution <- sppcount_dat$pollution
pollution
lambda <- exp(3.437 - 0.139*pollution)
lambda <- exp(3 - 0.639*pollution)
species2 <- rpois(n = 30, lambda = lambda)

sppcount_dat2 <- data.frame(species = species2, pollution = pollution)
gf_point(species ~ pollution, data = sppcount_dat2)
sppcount_lm   <- lm(species ~ pollution, data = sppcount_dat2)
sppcount_glm2 <- glm(species ~ pollution, data = sppcount_dat2, family=poisson)

library(DHARMa)
simulationoutput_lm <- simulateResiduals(fittedModel = sppcount_lm, plot = FALSE)
simulationoutput_glm2 <- simulateResiduals(fittedModel = sppcount_glm2, plot = FALSE)

plot(simulationoutput_lm)
plot(simulationoutput_glm2)
plot(sppcount_lm, which=c(1,2))
plot(sppcount_glm2, which=c(1,2))
write.csv(sppcount_dat, file = "Data/sppcount_shiny.csv")
write.csv(sppcount_dat2, file = "Data/sppcount.csv")
