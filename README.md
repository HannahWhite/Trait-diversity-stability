# Trait-diversity-stability

Investigating the relationship between trait diversity and community stability


CreatTraitMatrix.R uses traits downloaded from European Bird Trait Database (https://datadryad.org/stash/dataset/doi:10.5061/dryad.n6k3n) and Elton Traits (https://esajournals.onlinelibrary.wiley.com/doi/10.1890/13-1917.1).

FDcalcAllTraits.R calculates FD at each site for each time point as well as across all time points.

bird.long.RData contains all processed data for analyses with stability metrics calculated in CommunityCoVCalculation.R, CoVEffectDiversity.R, FDCalcAllTraits.R and June_Bird compositional stability.Rmd.

Analyses on bird.long.RData can be found in StabilityModels5years.R

#Archive files#

Land cover.Rmd uses the longform version of our land cover data to take a preliminary look at our land cover data to inform our modeling approach.
