library(networkD3)
library(dplyr)
library(webshot)
library(here)

source(here('R/funcs.R'))

# sankey --------------------------------------------------------------------------------------

fluccs <- read.csv(here('data/FLUCCShabsclass.csv'))
data(chgdatmanu)
data(subtchgdatmanu)

a1 <- alluvout2(chgdatmanu, fluccs, mrg = 230, height = 700, width = 725, fontSize = 16)
a2 <- alluvout2(subtchgdatmanu, fluccs, mrg = 130, height = 700, width = 750, fontSize = 16)

saveNetwork(a1, here("figs/chgdatalluv.html"))
saveNetwork(a2, here("figs/subtchgdatalluv.html"))

# go to chrome and use svgcrowbar to svg, then inkscape svg to png
# webshot screws up the labels
