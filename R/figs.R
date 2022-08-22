library(networkD3)
library(dplyr)
library(tidyr)
library(webshot)
library(here)
library(sf)
library(ggmap)
library(ggspatial)

source(here('R/funcs.R'))


# gis workflow --------------------------------------------------------------------------------

# by hand, original at C:\Users\mbeck\Desktop\TBEP\HMPU personal comp

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

# opportunity map -----------------------------------------------------------------------------

# data copied from commit e6026f831e90cffca8ea906c8fe53c85ceb5746a in hmpu-workflow
# these are the original HMPU layers in the report
data("nativersrv")
data("restorersrv")
data("nativelyr")
data("restorelyr")
data("coastal")
data("tbshed")

oppdat <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal)

p <- oppmap_fun(oppdat, tbshed, northloc = 'tr', scaleloc = 'tl',
                buffdist = 0.04)

png(here('figs/oppmap.png'), height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# restoration potential map -------------------------------------------------------------------

data("restorelyr")

restdat <- restdat_fun(restorelyr)

p <- restmap_fun(restdat, tbshed, northloc = 'tr', scaleloc = 'tl',
                 buffdist = 0.04)

png(here('figs/restmap.png'), height = 5, width = 8, res = 300, units = 'in')
p
dev.off()
