###################################################

# Setting up real data to go into the next step

###################################################
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(stringr)
library(forcats)
library(RColorBrewer)
library(geosphere)
library(abind)

rm(list = ls())

#### SET DIRECTORIES ####
working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

data_dir <- paste(working.dir, "Data", sep="/")
fig_dir <- paste(working.dir, "Figures", sep="/")
pop_dir <- paste(working.dir, "Population_Files", sep="/")

#### READ IN DATA ####
setwd(data_dir)

lengths <- read.csv("2010-09_Pilbara_stereoBRUV_Length (1).csv")
metadata <- read.csv("2010-09_Pilbara_stereoBRUV_Metadata (3).csv")

#### FORMAT DATA ####

full.data <- lengths %>% 
  left_join(., metadata, by="Sample") %>% 
  mutate(scientific = paste0(Genus, sep=" ", Species)) %>% 
  filter(scientific %in% c("Lutjanus sebae", "Epinephelus multinotatus", "Pristipomoides multidens"))

setwd(data_dir)
saveRDS(full.data, "Pilbara_data")






