library(L3Assess)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(forcats)
library(ggtext)

working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_dir <- paste(working.dir, "Data", sep="/")
fig_dir <- paste(working.dir, "Figures", sep="/")

a4.width=160

#### Make data frame ####

dat <- data.frame(species=as.factor(c("L. nebulosus", "L. miniatus", "C. auratus", "C. auricularis", "C. auricularis", "O. lineolatus", "O. lineolatus")),
                  location = c("Ningaloo", "Abrolhos", "Perth Metro and South West", "Abrolhos", "Perth Metro", "Perth Metro", "South West"),
                  mortality = c(0.27, 0.26, 0.23, 0.04, 0.23, 0.12, 0.07),
                  nat.mort = c(0.146, 0.14, 0.102, 0.5, 0.5, 0.36, 0.36),
                  upp_bnd = c(0.35, 0.48, 0.26, 0.25, 0.25, 0.24, 0.10),
                  lwr_bnd = c(0.2, 0.12, 0.2, 0.01, 0.2, 0.06, 0.05)) %>% 
  mutate(species.location = paste0(species, sep="_", location)) %>% 
  mutate(condition = 1-(mortality/(mortality+nat.mort))) %>% 
  mutate(condition_upp = 1-(lwr_bnd/(lwr_bnd+nat.mort))) %>% 
  mutate(condition_lwr = 1-(upp_bnd/(upp_bnd+nat.mort))) %>% 
  mutate(species.location=fct_relevel(species.location, "L. nebulosus_Ningaloo","L. miniatus_Abrolhos","C. auratus_Perth Metro and South West",
                                      "C. auricularis_Perth Metro", "O. lineolatus_Perth Metro", "C. auricularis_Abrolhos","O. lineolatus_South West"))

colours <- c("#88CBED", "#A9439A", "#332387", "#117633", "#43A999", "#872155", "#CB6778")
species.labels <- c("*L. nebulosus*<br>(Ningaloo)", "*L. miniatus*<br>(Abrolhos)", "*C. auratus*<br>(Metro and SW)","*C. auricularis*<br>(Metro)",
                    "*O. lineolatus*<br>(Metro)", "*C. auricularis*<br>(Abrolhos)", "*O. lineolatus*<br>(SW)")
#### Make Plot ####

condition_plot <- dat %>% 
  ggplot(.)+
  geom_rect(xmin=0, xmax=7.5,ymin=0, ymax=0.5, colour=NA, fill="#e06666ff", alpha=0.15)+
  geom_rect(xmin=0, xmax=7.5,ymin=0.5, ymax=0.6, colour=NA, fill="#ffe599ff", alpha=0.15)+
  geom_rect(xmin=0, xmax=7.5,ymin=0.6, ymax=0.8, colour=NA, fill="#a4c2f4ff", alpha=0.15)+
  geom_rect(xmin=0, xmax=7.5,ymin=0.8, ymax=1, colour=NA, fill="#6aa84fff", alpha=0.15)+
  geom_point(aes(x=species.location, y=condition), fill="grey20", colour="grey20")+
  geom_linerange(aes(x=species.location, ymax=condition_upp, ymin=condition_lwr), colour="grey20")+
  theme_classic()+
  scale_x_discrete(labels=species.labels)+
  ylim(0,1)+
  ylab("Condition (1-[F/F+M])")+
  xlab(NULL)+
  theme(axis.text.x=ggtext::element_markdown())
condition_plot

setwd(fig_dir)
ggsave(condition_plot, filename="Condition_plot.png", height = a4.width*1, width = a4.width*1.1, units  ="mm", dpi = 300 )




