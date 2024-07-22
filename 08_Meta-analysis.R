library(L3Assess)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(forcats)
library(ggtext)
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)
library(patchwork)
library(ggnewscale)
library(scales)
library(ggspatial)
library(tidyterra)
library(FSSgam)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(doSNOW)

working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_dir <- paste(working.dir, "Data", sep="/")
fig_dir <- paste(working.dir, "Figures", sep="/")
sp_dir <- paste(working.dir, "Spatial_data", sep="/")

dat <- data.frame(species=as.factor(c("L. nebulosus","E. armatus ", "C. auratus", "C. auricularis", "C. auricularis", "O. lineolatus", "O. lineolatus")),
                  mortality = c(0.27, 0.1, 0.23, 0.13, 0.32, 0.12, 0.088),
                  nat.mort = c(0.146, 0.3, 0.102, 0.41, 0.41, 0.36, 0.36),
                  upp_bnd = c(0.35, 0.034, 0.26, 0.23, 0.294, 0.24, 0.115),
                  lwr_bnd = c(0.2, 0.24, 0.2, 0.071, 0.337, 0.06, 0.067)) %>% 
  mutate(condition = 1-(mortality/(mortality+nat.mort))) %>% 
  mutate(condition_upp = 1-(lwr_bnd/(lwr_bnd+nat.mort))) %>% 
  mutate(condition_lwr = 1-(upp_bnd/(upp_bnd+nat.mort))) %>% 
  mutate(variance = (condition_lwr+condition_upp)/2) %>% 
  # group_by(species) %>% 
  # mutate(average = mean(condition)) %>% 
  # ungroup() %>% 
  distinct(species, .keep_all=T) %>% 
  filter(!species %in% "L. nebulosus") %>%
  mutate(max.length = c(500,1130,397,400),
         trophic.level = c(3.9,3.3,3.4,3.5)) %>% 
  as.data.frame()
  

Model1=gam(condition~s(max.length,bs='cr',k=1), family=gaussian(), data=dat, weights = 1/variance)

cont.preds = c("max.length", "trophic.level")
out.all     <- list()
var.imp     <- list()

model.set=generate.model.set(use.dat=dat,max.predictors=1,   # limit size here because null model already complex
                             test.fit=Model1,k=1,
                             cov.cutoff = 0.3,
                             factor.smooth.interactions = F,
                             smooth.smooth.interactions = F,
                             pred.vars.cont=cont.preds)

out.list <- fit.model.set(model.set,
                          max.models = 600,
                          parallel = T)
names(out.list)

out.list$failed.models # examine the list of failed models
mod.table <- out.list$mod.data.out  # look at the model selection table
mod.table <- mod.table[order(mod.table$AICc), ]
mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
out.i   <- mod.table[which(mod.table$delta.AICc <= 2), ]
out.all <- c(out.all,list(out.i))
var.imp <- c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2

names(out.all) <- cont.preds
names(var.imp) <- cont.preds
all.mod.fits   <- do.call("rbind",out.all)
all.var.imp    <- do.call("rbind",var.imp)

dat.new <- dat %>% 
  distinct(species, .keep_all=T)

testdata <- expand.grid(max.length=mean(dat.new$max.length)) %>%
  
  distinct()%>%
  glimpse()

fits <- predict.gam(Model1, newdata=testdata, type='response', se.fit=T)

predict.condition <- testdata%>%data.frame(fits) %>% 
  summarise(condition=mean(fit),se.fit=mean(se.fit))







