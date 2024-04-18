library(L3Assess)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(forcats)

working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_dir <- paste(working.dir, "Data", sep="/")
fig_dir <- paste(working.dir, "Figures", sep="/")

a4.width=160

#### Make data frame ####

dat <- data.frame(species=as.factor(c("L. nebulosus", "L. miniatus", "C. auratus", "C. auricularis", "C. auricularis", "O. lineolatus", "O. lineolatus")),
                  location = c("Ningaloo", "Abrolhos", "Perth Metro and South West", "Abrolhos", "Perth Metro", "Perth Metro", "South West"),
                  mortality = c(0.27, 0.26, 0.23, 0.04, 0.23, 0.12, 0.07),
                  upp_bnd = c(0.35, 0.48, 0.26, 0.25, 0.25, 0.24, 0.10),
                  lwr_bnd = c(0.2, 0.12, 0.2, 0.01, 0.2, 0.06, 0.05)) %>% 
  mutate(species.location = paste0(species, sep="_", location)) %>% 
  mutate(condition = 1-mortality) %>% 
  mutate(condition_upp = 1-lwr_bnd) %>% 
  mutate(condition_lwr = 1-upp_bnd) %>% 
  mutate(species.location=fct_relevel(species.location, "L. nebulosus_Ningaloo","L. miniatus_Abrolhos","C. auratus_Perth Metro and South West","C. auricularis_Abrolhos","C. auricularis_Perth Metro","O. lineolatus_Perth Metro","O. lineolatus_South West"))

colours <- c("#88CBED", "#A9439A", "#332387", "#117633", "#43A999", "#872155", "#CB6778")
species.labels <- c("L. nebulosus\n(Ningaloo)", "L. miniatus\n(Abrolhos)", "C. auratus\n(Metro and SW)", "C. auricularis\n(Abrolhos)", "C. auricularis\n(Metro)", "O. lineolatus\n(Metro)", "O. lineolatus\n(SW)")
#### Make Plot ####

condition_plot <- dat %>% 
  ggplot(.)+
  geom_point(aes(x=species.location, y=condition), fill=colours, colour=colours)+
  geom_linerange(aes(x=species.location, ymax=condition_upp, ymin=condition_lwr), colour=colours)+
  theme_classic()+
  scale_x_discrete(labels=species.labels)+
  ylab("Condition (1-F)")+
  xlab(NULL)+
  theme(axis.text.x=element_text(face="italic"))
condition_plot

setwd(fig_dir)
ggsave(condition_plot, filename="Condition_plot.png", height = a4.width*1, width = a4.width*1.1, units  ="mm", dpi = 300 )




