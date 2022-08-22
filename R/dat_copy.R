# get data files from hmpu-workflow repo (local only)

library(here)

# get data ------------------------------------------------------------------------------------

fls <- list.files(here('../hmpu-workflow/data'), full.names = T, pattern = '\\.RData$|\\.csv$')

file.copy(fls, to = here('data/'), overwrite = T)

#  get R functions ----------------------------------------------------------------------------

file.copy(here('../hmpu-workflow/R/funcs.R'), here('R/'), overwrite = T)
