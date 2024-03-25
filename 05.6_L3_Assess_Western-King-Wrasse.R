
rm(list=ls())
# devtools::install_github("SAlexHesp/L3AssessRPackage", build_vignettes=TRUE, force=TRUE)
library(L3Assess)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)

# Simulate data
SampleSize=5000 # sample size for retained catches (and same number for released fish, if an MLL is specified)
set.seed(123)
MaxAge = 8.5
TimeStep = 0.5 # model timestep (e.g. 1 = annual, 1/12 = monthly)
NatMort = 4.22/MaxAge
FishMort = 1.5 * NatMort
MaxLen = 400
LenInc = 20
MLL=NA # (minimum legal length) # retention set to 1 for all lengths if MLL set to NA and retention parameters not specified
SelectivityType=2 # 1=selectivity inputted as vector, 2=asymptotic logistic selectivity curve
SelectivityVec = NA # selectivity vector
SelParams = c(70, 30) # L50, L95-L50 for gear selectivity
RetenParams = c(NA, NA) # L50, L95-L50 for retention
DiscMort = 0 # proportion of fish that die due to natural mortality

# single sex, von Bertalanffy
GrowthCurveType = 1 # 1 = von Bertalanffy, 2 = Schnute
# Linf = 664
Linf = 484
# vbK = 0.241
vbK = 0.16
CVSizeAtAge = 0.05
GrowthParams = c(Linf, vbK)
RefnceAges = NA
Res=SimLenAndAgeFreqData(SampleSize, MaxAge, TimeStep, NatMort, FishMort, MaxLen, LenInc, MLL, SelectivityType,
                         SelParams, RetenParams, SelectivityVec, DiscMort, GrowthCurveType, GrowthParams, RefnceAges, CVSizeAtAge)

# fit catch curve to simulated data
ObsDiscCatchFreqAtLen = NA # (or set to Res$ObsDiscCatchFreqAtLen)
PropReleased = NA # proportion of fish released, vector including mean and sd (option probably now obselete)
midpt=Res$midpt
lbnd=Res$lbnd
ubnd=Res$ubnd
length(ObsRetCatchFreqAtLen)
length(midpt)
InitFishMort = 0.25 # specify starting parameters
InitFishMort_logit = log(InitFishMort/(1-InitFishMort)) # logit transform
InitL50 = 70
InitDelta = 30
params = c(InitFishMort_logit, log(InitL50), log(InitDelta))
DistnType = 1

ObsRetCatchFreqAtLen = Res$ObsRetCatchFreqAtLen

FittedRes=GetLengthBasedCatchCurveResults(params, DistnType, GrowthCurveType, GrowthParams, RefnceAges, MLL, SelectivityType, ObsRetCatchFreqAtLen,
                                          lbnd, ubnd, midpt, SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, CVSizeAtAge, MaxAge, NatMort, TimeStep)
FittedRes$ResultsSummary
PlotLengthBasedCatchCurve_RetCatch(params, DistnType, MLL, SelectivityType, ObsRetCatchFreqAtLen, lbnd, ubnd, midpt,
                                   SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, GrowthCurveType, GrowthParams,
                                   RefnceAges, MaxAge, NatMort, TimeStep, MainLabel=NA,
                                   xaxis_lab=NA, yaxis_lab=NA, xmax=800, xint=50,
                                   ymax=0.15, yint=0.05, PlotCLs=TRUE, FittedRes, nReps=200)


# ***************************
# try fitting to 'real' data
# ***************************
working.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_dir <- paste(working.dir, "Data", sep="/")
fig_dir <- paste(working.dir, "Figures", sep="/")

#* Abrolhos ####

setwd(data_dir)

dat <- readRDS("australian-synthesis_complete_length_coris_auricularis.RDS") %>% 
  dplyr::filter_all(.vars_predicate = any_vars(str_detect(.,"Abrolhos.MF|Abrolhos.WAMSI"))) %>%
  # dplyr::filter_all(.vars_predicate = any_vars(str_detect(.,"Abrolhos_stereo-BRUVs"))) %>%
  filter(length<=MaxLen)
  
  
head(dat)
range(dat$length)
LenInterval = 30
LenCats <- seq(from=0, to=MaxLen+20, by=LenInterval)
LenCats


HistData <- hist(dat$length, breaks=LenCats, right=FALSE, col="light blue", main="", xlab="20 mm Length category", ylab="Frequency",las=1)
HistData

ObsRetCatchFreqAtLen = as.vector(HistData$counts)
midpt = as.vector(HistData$mids)
lbnd = midpt - (LenInterval/2)
ubnd = midpt + (LenInterval/2)
ObsDiscCatchFreqAtLen = NA # (or set to Res$ObsDiscCatchFreqAtLen)
PropReleased = NA # proportion of fish released, vector including mean and sd (option probably now obselete)
InitFishMort = 0.05 # specify starting parameters
InitFishMort_logit = log(InitFishMort/(1-InitFishMort)) # logit transform
InitL50 = 70
InitDelta = 30
params = c(InitFishMort_logit, log(InitL50), log(InitDelta))
DistnType = 1
ObsDiscCatchFreqAtLen = NA # (or set to Res$ObsDiscCatchFreqAtLen)
CVSizeAtAge = 0.025
TimeStep = 0.5 #0.5 # model timestep (e.g. 1 = annual, 1/12 = monthly)

FittedRes=GetLengthBasedCatchCurveResults(params, DistnType, GrowthCurveType, GrowthParams, RefnceAges, MLL, SelectivityType, ObsRetCatchFreqAtLen,
                                          lbnd, ubnd, midpt, SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, CVSizeAtAge, MaxAge, NatMort, TimeStep)

FittedRes$ResultsSummary

setwd(fig_dir)
jpeg(file="Catch-Curve_C-auricularis_Abrolhos.jpeg")
X_PlotLengthBasedCatchCurve_RetCatch(params, DistnType, MLL, SelectivityType, ObsRetCatchFreqAtLen, lbnd, ubnd, midpt,
                                     SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, GrowthCurveType, GrowthParams,
                                     RefnceAges, MaxAge, NatMort, TimeStep, MainLabel=NA,
                                     xaxis_lab=NA, yaxis_lab="Proportion (observed)", xmax=400, xint=50,
                                     ymax=0.5, yint=0.1, PlotCLs=TRUE, FittedRes, nReps=200, Error.Colour = "#84D6A4")
dev.off()


#* Metro ####
setwd(data_dir)

dat <- readRDS("australian-synthesis_complete_length_coris_auricularis.RDS") %>% 
  dplyr::filter_all(.vars_predicate = any_vars(str_detect(.,"Two.Rocks|TwoRocks|Warnbro|Marmion"))) %>%
  # dplyr::filter_all(.vars_predicate = any_vars(str_detect(.,"Abrolhos_stereo-BRUVs"))) %>%
  filter(length<=MaxLen)


head(dat)
range(dat$length)
LenInterval = 30
LenCats <- seq(from=0, to=MaxLen+20, by=LenInterval)
LenCats


HistData <- hist(dat$length, breaks=LenCats, right=FALSE, col="light blue", main="", xlab="20 mm Length category", ylab="Frequency",las=1)
HistData

ObsRetCatchFreqAtLen = as.vector(HistData$counts)
midpt = as.vector(HistData$mids)
lbnd = midpt - (LenInterval/2)
ubnd = midpt + (LenInterval/2)
ObsDiscCatchFreqAtLen = NA # (or set to Res$ObsDiscCatchFreqAtLen)
PropReleased = NA # proportion of fish released, vector including mean and sd (option probably now obselete)
InitFishMort = 0.05 # specify starting parameters
InitFishMort_logit = log(InitFishMort/(1-InitFishMort)) # logit transform
InitL50 = 70
InitDelta = 30
params = c(InitFishMort_logit, log(InitL50), log(InitDelta))
DistnType = 1
ObsDiscCatchFreqAtLen = NA # (or set to Res$ObsDiscCatchFreqAtLen)
CVSizeAtAge = 0.025
TimeStep = 0.5 #0.5 # model timestep (e.g. 1 = annual, 1/12 = monthly)

FittedRes=GetLengthBasedCatchCurveResults(params, DistnType, GrowthCurveType, GrowthParams, RefnceAges, MLL, SelectivityType, ObsRetCatchFreqAtLen,
                                          lbnd, ubnd, midpt, SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, CVSizeAtAge, MaxAge, NatMort, TimeStep)

FittedRes$ResultsSummary

setwd(fig_dir)
jpeg(file="Catch-Curve_C-auricularis_Metro.jpeg")
X_PlotLengthBasedCatchCurve_RetCatch(params, DistnType, MLL, SelectivityType, ObsRetCatchFreqAtLen, lbnd, ubnd, midpt,
                                     SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, GrowthCurveType, GrowthParams,
                                     RefnceAges, MaxAge, NatMort, TimeStep, MainLabel=NA,
                                     xaxis_lab=NA, yaxis_lab="Proportion (observed)", xmax=400, xint=50,
                                     ymax=0.5, yint=0.1, PlotCLs=TRUE, FittedRes, nReps=200, Error.Colour = "#CCEBC5")

dev.off()


#* Capes ####
setwd(data_dir)

dat <- readRDS("australian-synthesis_complete_length_coris_auricularis.RDS") %>% 
  dplyr::filter_all(.vars_predicate = any_vars(str_detect(.,"Capes|south-west|Ngari"))) %>%
  dplyr::filter_all(.vars_predicate = any_vars(str_detect(.,"2019|2020|2021|2021|2023"))) %>%
  filter(length<=MaxLen)


head(dat)
range(dat$length)
LenInterval = 30
LenCats <- seq(from=0, to=MaxLen+20, by=LenInterval)
LenCats


HistData <- hist(dat$length, breaks=LenCats, right=FALSE, col="light blue", main="", xlab="20 mm Length category", ylab="Frequency",las=1)
HistData

ObsRetCatchFreqAtLen = as.vector(HistData$counts)
midpt = as.vector(HistData$mids)
lbnd = midpt - (LenInterval/2)
ubnd = midpt + (LenInterval/2)
ObsDiscCatchFreqAtLen = NA # (or set to Res$ObsDiscCatchFreqAtLen)
PropReleased = NA # proportion of fish released, vector including mean and sd (option probably now obselete)
InitFishMort = 0.05 # specify starting parameters
InitFishMort_logit = log(InitFishMort/(1-InitFishMort)) # logit transform
InitL50 = 70
InitDelta = 30
params = c(InitFishMort_logit, log(InitL50), log(InitDelta))
DistnType = 1
ObsDiscCatchFreqAtLen = NA # (or set to Res$ObsDiscCatchFreqAtLen)
CVSizeAtAge = 0.075
TimeStep = 0.5 #0.5 # model timestep (e.g. 1 = annual, 1/12 = monthly)

FittedRes=GetLengthBasedCatchCurveResults(params, DistnType, GrowthCurveType, GrowthParams, RefnceAges, MLL, SelectivityType, ObsRetCatchFreqAtLen,
                                          lbnd, ubnd, midpt, SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, CVSizeAtAge, MaxAge, NatMort, TimeStep)

FittedRes$ResultsSummary

setwd(fig_dir)
jpeg(file="Catch-Curve_C-auricularis_Capes.jpeg")
X_PlotLengthBasedCatchCurve_RetCatch(params, DistnType, MLL, SelectivityType, ObsRetCatchFreqAtLen, lbnd, ubnd, midpt,
                                     SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, GrowthCurveType, GrowthParams,
                                     RefnceAges, MaxAge, NatMort, TimeStep, MainLabel=NA,
                                     xaxis_lab=NA, yaxis_lab="Proportion (observed)", xmax=400, xint=50,
                                     ymax=0.5, yint=0.1, PlotCLs=TRUE, FittedRes, nReps=200, Error.Colour = "#8DD3C7")

dev.off()
