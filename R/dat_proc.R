library(sf)
library(tidyverse)
library(here)
library(units)

source(here('R/funcs.R'))


# targets as metric (ha, mi) ------------------------------------------------------------------

trgsmetric <- structure(
  list(
    Category = structure(
      c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
      .Label = c("Subtidal", "Intertidal", "Supratidal"),
      class = "factor"
      ),
    HMPU_TARGETS = structure(
      1:15,
      .Label = c("Hard Bottom", "Artificial Reefs", "Tidal Flats", "Seagrasses", "Oyster Bars", "Living Shorelines", "Total Intertidal", "Mangrove Forests", "Salt Barrens", "Salt Marshes", "Tidal Tributaries", "Coastal Uplands", "Non-Forested Freshwater Wetlands", "Forested Freshwater Wetlands", "Native Uplands"),
      class = "factor"
      ),
    Target2030 = c(171, 67, 6564, 16188, 89, 34, 8641, 6192, 221, 1945, 6, 1525, 27898, 61628, 57082),
    Target2050 = c(171, 67, 6564, 16188, 191, 90, 9633, 6192, 322, 2208, 29, 1707, 29052, 61810, 57507),
    rationale = c(
      "Protect existing hard bottom; continue to identify new hard bottom area using proven mapping techniques",
      "Protect existing artificial reefs; enhance habitat complexity where feasible; expand reef area to promote fish and wildlife benefits",
      "Identify and protect existing persistent tidal flats; assess restoration potential of other non-vegetated subtidal areas",
      "Protect existing seagrasses; establish new HMPU lower limit of 16,188 hectares; assess restoration potential of non-vegetated subtidal areas",
      "2030: Protect existing oysters + restore 20 hectares; increase target by 20 hectares each out-decade; consider filtration rate to refine long-term goal; an oyster habitat suitability index (HSI) will inform opportunity space",
      "2030: Construct 1.6 kilometers of LS each year; includes privately owned seawalls; need better definition of opportunity areas; increase target to 2.4 & 3.2 kilometers per year for out decades",
      "2030: Protect existing intertidal mosaic + restore 405 hectares (based on hydric soils); increase target by 61 hectares each out-decade; includes the mosaic of mangrove, salt barren, and salt marsh habitats",
      "Protect existing mangrove forests; restore opportunistically within the intertidal mosaic",
      "2030: Protect existing salt barrens + restore 20 hectares; increase target by 20 hectares per out decade",
      "2030: Protect existing low salinity salt marshes + restore 101 hectares; increase target by 20 hectares each out-decade; significant land acquisition and/or Public Private Partnerships required to achieve this 2030 target and 2050 goal",
      "Inventory mapped tidal tributaries and assess/rank restoration potential; restore ~6.4 kilometers (1%) of urban tidal creek habitat where feasible; increase target by 3.2 kilometers per out decade",
      "2030: Protect existing coastal uplands + specifically restore 61 hectares (upland restoration total = 243 hectares); increase target by 20 hectares each out decade",
      "2030: Protect existing non-forested freshwater wetlands + restore 546 hectares; increase target by 20 hectares each out decade",
      "2030: Protect existing forested freshwater wetlands + restore 61 hectares; increase target by 20 hectares each out decade",
      "2030: Protect existing native uplands + specifically restore 182 hectares (upland restoration total = 243 hectares); increase target by 20 hectares each out decade; focus on pine flatwoods and protect current extent (22,953 hectares)"
      )
    ),
  class = "data.frame",
  row.names = c(NA, -15L)
  )

save(trgsmetric, file = here('data/trgsmetric.RData'))

# LULC change analysis ----------------------------------------------------

data(coastal)

# get final year
maxyr <- 2017

load(file = here(paste0('data/lulc', maxyr, '.RData')))
maxdat <- maxyr %>%
  paste0('lulc', .) %>%
  get %>%
  lulc_est(coastal, fluccs, sumout = F) %>%
  rename(Category = HMPU_TARGETS) %>%
  st_union(by_feature = TRUE) %>%
  mutate(
    Category = paste0(Category, ', ', maxyr)
  )

# year
yr <- 1990

cat(yr, 'importing\n')
print(Sys.time() - strt)

# current year, add coastal stratum
load(file = here(paste0('data/lulc', yr, '.RData')))
a <- yr %>%
  paste0('lulc', .) %>%
  get %>%
  lulc_est(coastal, fluccs, sumout = F) %>%
  rename(Category = HMPU_TARGETS) %>%
  st_union(by_feature = TRUE) %>%
  mutate(Category = paste0(Category, ', ', yr))
b <- maxdat

cat('\tintersecting...\n')

# so intersect doesnt complain about attributes
st_agr(a) = "constant"
st_agr(b) = "constant"

# some clean up stuff for slivers
a <- a %>%
  st_set_precision(1e5) %>%
  st_make_valid() %>%
  st_buffer(dist = 0)
b <- b %>%
  st_set_precision(1e5) %>%
  st_make_valid() %>%
  st_buffer(dist = 0)
aunion <- a %>%
  st_union %>%
  st_set_precision(1e5) %>%
  st_make_valid() %>%
  st_buffer(dist = 0)
bunion <- b %>%
  st_union %>%
  st_set_precision(1e5) %>%
  st_make_valid() %>%
  st_buffer(dist = 0)

# get full union
op1 <- st_difference(a, bunion)
op2 <- st_difference(b, aunion) %>%
  rename(Category.1 = Category)
op3 <- st_intersection(a, b)

chgdatmanu <- bind_rows(op1, op2, op3) %>%
  mutate(
    yr = unique(na.omit(sub('^.*\\,\\s([0-9]+)$', '\\1', Category))),
    yr.1 = unique(na.omit(sub('^.*\\,\\s([0-9]+)$', '\\1', Category.1))),
    Category = ifelse(is.na(Category), paste0('other, ', yr), as.character(Category)),
    Category.1 = ifelse(is.na(Category.1), paste0('other, ', yr.1), as.character(Category.1)),
    Acres = st_area(.),
    Acres = set_units(Acres, 'hectares'),
    Acres = as.numeric(Acres)
  ) %>%
  select(-yr, -yr.1) %>%
  st_set_geometry(NULL) %>%
  select(Category.1, Category, Acres) %>%
  group_by(Category.1, Category) %>%
  summarise(Acres = sum(Acres)) %>%
  ungroup %>%
  select(source = Category, target = Category.1, value = Acres) %>%
  data.frame(stringsAsFactors = F)

save(chgdatmanu, file = here('data', 'chgdatmanu.RData'), version = 2)

# subtidal change analysis ------------------------------------------------

# get final year
maxyr <- 2018

# get final year data, add coastal uplands
maxdat <- load(file = here(paste0('data/sgdat', maxyr, '.RData')))
maxdat <- maxyr %>%
  paste0('sgdat', .) %>%
  get %>%
  mutate(
    FLUCCSCODE = as.integer(FLUCCSCODE)
  ) %>%
  left_join(fluccs, by = 'FLUCCSCODE') %>%
  select(Category = HMPU_TARGETS) %>%
  st_union(by_feature = TRUE) %>%
  mutate(
    Category = paste0(Category, ', ', maxyr)
  )

yr <- 1988

# current year, add coastal stratum
load(file = here(paste0('data/sgdat', yr, '.RData')))
a <- yr %>%
  paste0('sgdat', .) %>%
  get %>%
  mutate(
    FLUCCSCODE = as.integer(FLUCCSCODE)
  ) %>%
  left_join(fluccs, by = 'FLUCCSCODE') %>%
  select(Category = HMPU_TARGETS) %>%
  st_union(by_feature = TRUE) %>%
  mutate(
    Category = paste0(Category, ', ', yr)
  )
b <- maxdat

cat('\tintersecting...\n')

# so intersect doesnt complain about attributes
st_agr(a) = "constant"
st_agr(b) = "constant"

# some clean up stuff for slivers
a <- a %>%
  st_set_precision(1e5) %>%
  st_make_valid() %>%
  st_buffer(dist = 0)
b <- b %>%
  st_set_precision(1e5) %>%
  st_make_valid() %>%
  st_buffer(dist = 0)
aunion <- a %>%
  st_union %>%
  st_set_precision(1e5) %>%
  st_make_valid() %>%
  st_buffer(dist = 0)
bunion <- b %>%
  st_union %>%
  st_set_precision(1e5) %>%
  st_make_valid() %>%
  st_buffer(dist = 0)

# get full union
op1 <- st_difference(a, bunion)
op2 <- st_difference(b, aunion) %>%
  rename(Category.1 = Category)
op3 <- st_intersection(a, b)

subtchgdatmanu <- bind_rows(op1, op2, op3) %>%
  mutate(
    yr = unique(na.omit(sub('^.*\\,\\s([0-9]+)$', '\\1', Category))),
    yr.1 = unique(na.omit(sub('^.*\\,\\s([0-9]+)$', '\\1', Category.1))),
    Category = ifelse(is.na(Category), paste0('other, ', yr), as.character(Category)),
    Category.1 = ifelse(is.na(Category.1), paste0('other, ', yr.1), as.character(Category.1)),
    Acres = st_area(.),
    Acres = set_units(Acres, 'acres'),
    Acres = as.numeric(Acres)
  ) %>%
  select(-yr, -yr.1) %>%
  st_set_geometry(NULL) %>%
  select(Category.1, Category, Acres) %>%
  group_by(Category.1, Category) %>%
  summarise(Acres = sum(Acres)) %>%
  ungroup %>%
  select(source = Category, target = Category.1, value = Acres) %>%
  data.frame(stringsAsFactors = F)

save(subtchgdatmanu, file = here('data', 'subtchgdatmanu.RData'), version = 2)
