library(networkD3)
library(dplyr)
library(tidyr)
library(webshot)
library(here)
library(sf)
library(ggmap)
library(ggspatial)
library(tibble)
library(grid)
library(USAboundaries)

source(here('R/funcs.R'))

# lulc map ------------------------------------------------------------------------------------

load(file = here('data/lulc2020.RData'))
load(file = here('data/sgdat2020.RData'))
load(file = here('data/fluccslkup.RData'))

# from T:\05_GIS\TBEP\2017_LULC_TBEP_Extent+2020_Seagrass.mxd
fluccsconv <- list(
    'Urban/Open Lands' = c(110, 120, 130, 140, 150, 170, 180, 182, 190, 740, 810, 820, 830),
    'Mining' = c(160),
    'Reclaimed Mined Lands' = c(165),
    'Agriculture' = c(210, 214, 220, 230, 240, 250, 255, 260),
    'Vegetated/Forested/Natural' = c(310, 320, 330, 410, 411, 412, 420, 434, 440, 710, 720),
    'Wetlands' = c(610, 611, 612, 615, 620, 621, 630, 641 ,642, 643, 644, 652, 653, 660),
    'Oyster' = c(654),
    'Tidal Flat' = c(651, 721),
    'Seagrass/SAV' = c(911, 912),
    'Water' = c(510, 520, 530, 540, 572)
  ) %>%
  enframe(value = 'LEV3') %>%
  unnest('LEV3') %>%
  left_join(fluccslkup, by = 'LEV3') %>%
  mutate(
    col = case_when(
      name == 'Urban/Open Lands' ~ '#FF0000',
      name == 'Mining' ~ '#C500FF',
      name == 'Reclaimed Mined Lands' ~ '#6D276B',
      name == 'Agriculture' ~ '#FFFF00',
      name == 'Vegetated/Forested/Natural' ~ '#00734C',
      name == 'Wetlands' ~ '#38A800',
      name == 'Oyster' ~ '#FFAA00',
      name == 'Tidal Flat' ~ '#FFEABE',
      name == 'Seagrass/SAV' ~ '#55FF00',
      name == 'Water' ~ '#0070FF'
    )
  )

tomap1 <- lulc2020 %>%
  left_join(fluccsconv, by = 'FLUCCSCODE') %>%
  filter(!is.na(name) )%>%
  st_transform(crs = 4326)

tomap2 <- sgdat2020 %>%
  left_join(fluccsconv, by = 'FLUCCSCODE') %>%
  filter(!is.na(name)) %>%
  st_transform(crs = 4326)

# colors
cols <- fluccsconv %>%
  select(name, col) %>%
  unique %>%
  deframe()

# layer extent as bbox plus buffer
dat_ext <- tomap1 %>%
  st_bbox %>%
  st_as_sfc %>%
  st_buffer(dist = 0.04) %>%
  st_bbox %>%
  unname

# stamen base map
bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'toner-background', zoom = 11)

# change opacity of basemap
mapatt <- attributes(bsmap1)
bsmap1_transparent <- matrix(adjustcolor(bsmap1,
                                         alpha.f = 0.2),
                             nrow = nrow(bsmap1))
attributes(bsmap1_transparent) <- mapatt

# for inset
states <- us_states() %>%
  filter(name %in% c('Florida', 'Georgia', 'Alabama'))
ylimrng <- states %>%
  filter(name %in% 'Florida')
statebuff <- st_buffer(ylimrng, dist = 0.25)
insetbb <- dat_ext %>%
  st_as_sfc(crs = 4326)
insetylim <- st_bbox(statebuff)[c('ymin', 'ymax')]
insetxlim <- st_bbox(statebuff)[c('xmin', 'xmax')]

lbs1 <- tibble(
  lon = -85.9, lat = 25.6, label = 'Gulf of\nMexico'
) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)
lbs2 <- tibble(
  lon = -81.2, lat = 29, label = 'Florida'
) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

pinset <- ggplot() +
  geom_sf(data = states, fill = 'grey', colour = 'grey') +
  geom_sf(data = insetbb, fill = NA, color = 'blue', size = 1.25) +
  geom_sf_text(data = lbs1, aes(label = label), size = 3.25) +
  geom_sf_text(data = lbs2, aes(label = label), size = 3.5, angle = -65) +
  coord_sf(ylim = insetylim, xlim = insetxlim) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = '#FFFFFF', colour = 'white'),
    panel.border = element_rect(colour = 'black', fill = 'transparent')
  )

# plot
plulc <- ggmap(bsmap1_transparent) +
  geom_sf(data = tomap1, aes(fill = name), color = NA, inherit.aes = F, alpha = 1) +
  geom_sf(data = tomap2, aes(fill = name), color = NA, inherit.aes = F, alpha = 1) +
  scale_fill_manual(values = cols, drop = F) +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position  = 'right',
    legend.justification = 'top',
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle = 30, hjust = 1),
    panel.background = element_rect(fill = 'white'),
    axis.ticks = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'grey', fill = NA)
  ) +
  annotation_scale(location = 'tr') +
  annotation_north_arrow(location = 'bl', which_north = "true", height = grid::unit(0.75, "cm"),
                         width = grid::unit(0.75, "cm"))

p <- plulc +
  inset(ggplotGrob(pinset), xmin = -81.85, xmax = -81.35, ymin = 27.4, ymax = 27.7)

png(here('figs/lulcmap.png'), height = 5, width = 7, res = 300, units = 'in')
p
dev.off()

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

data("tbshed")
load(file = here('data/oppdat.RData'))

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
