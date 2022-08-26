# get data files from hmpu-workflow repo (local only)

library(here)

# get data ------------------------------------------------------------------------------------

fls <- list.files(here('../hmpu-workflow/data'), full.names = T, pattern = '\\.RData$|\\.csv$')

file.copy(fls, to = here('data/'), overwrite = T)

#  get R functions ----------------------------------------------------------------------------

file.copy(here('../hmpu-workflow/R/funcs.R'), here('R/'), overwrite = T)

# fluccslkup ----------------------------------------------------------------------------------

load(file = url('https://github.com/tbep-tech/reasonable-assurance-analysis/raw/main/data/fluccslkup.RData'))

save(fluccslkup, file = here('data/fluccslkup.RData'))
