# Trait-diversity-stability

Investigating the relationship between trait diversity and community stability


CreatTraitMatrix.R uses traits downloaded from European Bird Trait Database (https://datadryad.org/stash/dataset/doi:10.5061/dryad.n6k3n) and Elton Traits (https://esajournals.onlinelibrary.wiley.com/doi/10.1890/13-1917.1).

FDcalculation.R calculates FD at each site for each time point as well as across all time points.

June_Bird compositional stability.Rmd is an updated file which uses code from FDcalculation.R to correct col names, then calculates the compositional temporal variability and the abundance-based asynchony of the bird communities at each site. 

Land cover.Rmd uses the longform version of our land cover data to take a preliminary look at our land cover data to inform our modeling approach.
