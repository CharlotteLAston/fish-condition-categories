
rm(list=ls())
# devtools::install_github("SAlexHesp/L3AssessRPackage", build_vignettes=TRUE, force=TRUE)
library(L3Assess)
library(dplyr)
library(stringr)
library(ggplot2)

#### SAMPLE SIZE FOR SPANGLED EMPEROR ####

MaxAge = 30
TimeStep = 1 
NatMort = 4.22/MaxAge
FishMort = 1.5 * NatMort
MaxLen = 800
LenInc = 20
MLL=NA 
SelectivityType = 2 # 1=selectivity inputted as vector, 2=asymptotic logistic selectivity curve
SelectivityVec = NA # selectivity vector
SelParams = c(391, 182)  # bruv curve

RetenParams = c(NA, NA) 
DiscMort = 0 
GrowthCurveType = 1
Linf = 573.1
vbK = 0.282
CVSizeAtAge = 0.05
GrowthParams = c(Linf, vbK)
RefnceAges = NA
ObsDiscCatchFreqAtLen = NA 
PropReleased = NA 
InitFishMort = 0.25 
InitFishMort_logit = log(InitFishMort/(1-InitFishMort))
DistnType = 1

InitL50 = 400
InitDelta = 100

params = c(InitFishMort_logit, log(InitL50), log(InitDelta))

samplesize <-  c(100,250,500,750,1000,1500,2000,3000,5000,7500,10000)

results <- as.data.frame(array(0, dim=c(length(samplesize), 3)))

for(i in 1:length(samplesize)) {
  SampleSize = samplesize[i]
  Res_sim=SimLenAndAgeFreqData(SampleSize, MaxAge, TimeStep, NatMort, FishMort, MaxLen, LenInc, MLL, SelectivityType,
                               SelParams, RetenParams, SelectivityVec, DiscMort, GrowthCurveType, GrowthParams, RefnceAges, CVSizeAtAge)
  
  ObsRetCatchFreqAtLen = Res_sim$ObsRetCatchFreqAtLen
  midpt=Res_sim$midpt
  lbnd=Res_sim$lbnd
  ubnd=Res_sim$ubnd
  
  Results_sim=GetLengthBasedCatchCurveResults(params, DistnType, GrowthCurveType, GrowthParams, RefnceAges, MLL, SelectivityType, ObsRetCatchFreqAtLen,
                                              lbnd, ubnd, midpt, SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, CVSizeAtAge, MaxAge, NatMort, TimeStep)
  Est <- Results_sim$ParamEst[1,1]
  uprbnd <- Results_sim$ParamEst[1,3]
  lwrbnd <- Results_sim$ParamEst[1,2]
  Results <- data.frame(Est,uprbnd,lwrbnd)
  
  results[i, ] <- Results
  
}

results$samplesize <- samplesize

results.spango <- results %>% 
  rename(
    Fmort = V1,
    upperbnd = V2,
    lowerbnd = V3)

# results1

#* Plot spangled emperor sample sizes ####

Spango_Sample_Size <- results.spango %>% 
  ggplot() +
  geom_point(aes(x=samplesize, y=Fmort))+
  geom_errorbar(aes(x=samplesize, y=Fmort ,ymin=lowerbnd, ymax=upperbnd))+
  theme_classic()+
  ylab("Estimate of fishing mortality")+
  xlab("Sample size")+
  geom_vline(xintercept = 280, linetype="dashed")
Spango_Sample_Size 


#### SAMPLE SIZE FOR RED EMPEROR ####

MaxAge = 40
TimeStep = 1 # model timestep (e.g. 1 = annual, 1/12 = monthly)
NatMort = 4.22/MaxAge
FishMort = 1 * NatMort
MaxLen = 1000
LenInc = 20
MLL=NA # (minimum legal length) # retention set to 1 for all lengths if MLL set to NA and retention parameters not specified
SelectivityType=2 # 1=selectivity inputted as vector, 2=asymptotic logistic selectivity curve
SelectivityVec = NA # selectivity vector
SelParams = c(450, 40) # L50, L95-L50 for gear selectivity
RetenParams = c(NA, NA) # L50, L95-L50 for retention
DiscMort = 0 # proportion of fish that die due to natural mortality

# single sex, von Bertalanffy
GrowthCurveType = 1 # 1 = von Bertalanffy, 2 = Schnute
Linf = 524.77
# Linf = 573.1
# vbK = 0.241
vbK = 0.2330
CVSizeAtAge = 0.05
GrowthParams = c(Linf, vbK)
RefnceAges = NA


# fit catch curve to simulated data
ObsDiscCatchFreqAtLen = NA # (or set to Res$ObsDiscCatchFreqAtLen)
PropReleased = NA # proportion of fish released, vector including mean and sd (option probably now obselete)
length(ObsRetCatchFreqAtLen)
length(midpt)
InitFishMort = 0.25 # specify starting parameters
InitFishMort_logit = log(InitFishMort/(1-InitFishMort)) # logit transform
InitL50 = 400
InitDelta = 100
params = c(InitFishMort_logit, log(InitL50), log(InitDelta))
DistnType = 1

samplesize <-  c(100,250,500,750,1000,1500,2000,3000,5000,7500,10000)

results <- as.data.frame(array(0, dim=c(length(samplesize), 3)))

for(i in 1:length(samplesize)) {
  SampleSize = samplesize[i]
  Res_sim=SimLenAndAgeFreqData(SampleSize, MaxAge, TimeStep, NatMort, FishMort, MaxLen, LenInc, MLL, SelectivityType,
                               SelParams, RetenParams, SelectivityVec, DiscMort, GrowthCurveType, GrowthParams, RefnceAges, CVSizeAtAge)
  
  ObsRetCatchFreqAtLen = Res_sim$ObsRetCatchFreqAtLen
  midpt=Res_sim$midpt
  lbnd=Res_sim$lbnd
  ubnd=Res_sim$ubnd
  
  Results_sim=GetLengthBasedCatchCurveResults(params, DistnType, GrowthCurveType, GrowthParams, RefnceAges, MLL, SelectivityType, ObsRetCatchFreqAtLen,
                                              lbnd, ubnd, midpt, SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, CVSizeAtAge, MaxAge, NatMort, TimeStep)
  Est <- Results_sim$ParamEst[1,1]
  uprbnd <- Results_sim$ParamEst[1,3]
  lwrbnd <- Results_sim$ParamEst[1,2]
  Results <- data.frame(Est,uprbnd,lwrbnd)
  
  results[i, ] <- Results
  
}

results$samplesize <- samplesize

results.redemp <- results %>% 
  rename(
    Fmort = V1,
    upperbnd = V2,
    lowerbnd = V3)

# results1

#* Plot red emperor sample sizes ####

RedEmp_Sample_Size <- results.redemp %>% 
  ggplot() +
  geom_point(aes(x=samplesize, y=Fmort))+
  geom_errorbar(aes(x=samplesize, y=Fmort ,ymin=lowerbnd, ymax=upperbnd))+
  theme_classic()+
  ylab("Estimate of fishing mortality")+
  xlab("Sample size")+
  geom_vline(xintercept = 92, linetype="dashed")
RedEmp_Sample_Size 

#### SAMPLE SIZE FOR RED THROAT ####

# Simulate data
MaxAge = 30
TimeStep = 1 # model timestep (e.g. 1 = annual, 1/12 = monthly)
NatMort = 4.22/MaxAge
FishMort = 1.5 * NatMort
MaxLen = 700
LenInc = 20
MLL=NA # (minimum legal length) # retention set to 1 for all lengths if MLL set to NA and retention parameters not specified
SelectivityType=2 # 1=selectivity inputted as vector, 2=asymptotic logistic selectivity curve
SelectivityVec = NA # selectivity vector
SelParams = c(391, 150) # L50, L95-L50 for gear selectivity
RetenParams = c(NA, NA) # L50, L95-L50 for retention
DiscMort = 0 # proportion of fish that die due to natural mortality

# single sex, von Bertalanffy
GrowthCurveType = 1 # 1 = von Bertalanffy, 2 = Schnute
# Linf = 664
Linf = 573.1
# vbK = 0.241
vbK = 0.282
CVSizeAtAge = 0.05
GrowthParams = c(Linf, vbK)
RefnceAges = NA


# fit catch curve to simulated data
ObsDiscCatchFreqAtLen = NA # (or set to Res$ObsDiscCatchFreqAtLen)
PropReleased = NA # proportion of fish released, vector including mean and sd (option probably now obselete)
length(ObsRetCatchFreqAtLen)
length(midpt)
InitFishMort = 0.25 # specify starting parameters
InitFishMort_logit = log(InitFishMort/(1-InitFishMort)) # logit transform
InitL50 = 400
InitDelta = 100
params = c(InitFishMort_logit, log(InitL50), log(InitDelta))
DistnType = 1

samplesize <-  c(100,250,500,750,1000,1500,2000,3000,5000,7500,10000)

results <- as.data.frame(array(0, dim=c(length(samplesize), 3)))

for(i in 1:length(samplesize)) {
  SampleSize = samplesize[i]
  Res_sim=SimLenAndAgeFreqData(SampleSize, MaxAge, TimeStep, NatMort, FishMort, MaxLen, LenInc, MLL, SelectivityType,
                               SelParams, RetenParams, SelectivityVec, DiscMort, GrowthCurveType, GrowthParams, RefnceAges, CVSizeAtAge)
  
  ObsRetCatchFreqAtLen = Res_sim$ObsRetCatchFreqAtLen
  midpt=Res_sim$midpt
  lbnd=Res_sim$lbnd
  ubnd=Res_sim$ubnd
  
  Results_sim=GetLengthBasedCatchCurveResults(params, DistnType, GrowthCurveType, GrowthParams, RefnceAges, MLL, SelectivityType, ObsRetCatchFreqAtLen,
                                              lbnd, ubnd, midpt, SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, CVSizeAtAge, MaxAge, NatMort, TimeStep)
  Est <- Results_sim$ParamEst[1,1]
  uprbnd <- Results_sim$ParamEst[1,3]
  lwrbnd <- Results_sim$ParamEst[1,2]
  Results <- data.frame(Est,uprbnd,lwrbnd)
  
  results[i, ] <- Results
  
}

results$samplesize <- samplesize

results.redthroat <- results %>% 
  rename(
    Fmort = V1,
    upperbnd = V2,
    lowerbnd = V3)

# results1

#* Plot red throat sample sizes ####

RedThroat_Sample_Size <- results.redthroat %>% 
  ggplot() +
  geom_point(aes(x=samplesize, y=Fmort))+
  geom_errorbar(aes(x=samplesize, y=Fmort ,ymin=lowerbnd, ymax=upperbnd))+
  theme_classic()+
  ylab("Estimate of fishing mortality")+
  xlab("Sample size")+
  geom_vline(xintercept = 145, linetype="dashed")
RedThroat_Sample_Size 


#### SAMPLE SIZE FOR WESTERN KING WRASSE ####

# Simulate data
MaxAge = 8.5
TimeStep = 0.5 # model timestep (e.g. 1 = annual, 1/12 = monthly)
NatMort = 4.22/MaxAge
FishMort = 0.16
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
CVSizeAtAge = 0.1
GrowthParams = c(Linf, vbK)
RefnceAges = NA


# fit catch curve to simulated data
ObsDiscCatchFreqAtLen = NA # (or set to Res$ObsDiscCatchFreqAtLen)
PropReleased = NA # proportion of fish released, vector including mean and sd (option probably now obselete)
length(ObsRetCatchFreqAtLen)
length(midpt)
InitFishMort = 0.16 # specify starting parameters
InitFishMort_logit = log(InitFishMort/(1-InitFishMort)) # logit transform
InitL50 = 70
InitDelta = 30
params = c(InitFishMort_logit, log(InitL50), log(InitDelta))
DistnType = 1

samplesize <-  c(100,250,500,750,1000,1500,2000,3000,5000,7500,10000)

results <- as.data.frame(array(0, dim=c(length(samplesize), 3)))

for(i in 1:length(samplesize)) {
  SampleSize = samplesize[i]
  Res_sim=SimLenAndAgeFreqData(SampleSize, MaxAge, TimeStep, NatMort, FishMort, MaxLen, LenInc, MLL, SelectivityType,
                               SelParams, RetenParams, SelectivityVec, DiscMort, GrowthCurveType, GrowthParams, RefnceAges, CVSizeAtAge)
  
  ObsRetCatchFreqAtLen = Res_sim$ObsRetCatchFreqAtLen
  midpt=Res_sim$midpt
  lbnd=Res_sim$lbnd
  ubnd=Res_sim$ubnd
  
  Results_sim=GetLengthBasedCatchCurveResults(params, DistnType, GrowthCurveType, GrowthParams, RefnceAges, MLL, SelectivityType, ObsRetCatchFreqAtLen,
                                              lbnd, ubnd, midpt, SelectivityVec, PropReleased, ObsDiscCatchFreqAtLen, DiscMort, CVSizeAtAge, MaxAge, NatMort, TimeStep)
  Est <- Results_sim$ParamEst[1,1]
  uprbnd <- Results_sim$ParamEst[1,3]
  lwrbnd <- Results_sim$ParamEst[1,2]
  Results <- data.frame(Est,uprbnd,lwrbnd)
  
  results[i, ] <- Results
  
}

results$samplesize <- samplesize

results.wkw <- results %>% 
  rename(
    Fmort = V1,
    upperbnd = V2,
    lowerbnd = V3)

# results1

#* Plot western king wrasse sample sizes ####

WKW_Sample_Size <- results.wkw %>% 
  ggplot() +
  geom_point(aes(x=samplesize, y=Fmort))+
  geom_errorbar(aes(x=samplesize, y=Fmort ,ymin=lowerbnd, ymax=upperbnd))+
  theme_classic()+
  ylab("Estimate of fishing mortality")+
  xlab("Sample size")+
  geom_vline(xintercept = 8165, linetype="solid")+
  geom_vline(xintercept = 1305, linetype="dashed", colour="darkgreen")+
  geom_vline(xintercept = 6861, linetype="dashed", colour="purple")
WKW_Sample_Size 




