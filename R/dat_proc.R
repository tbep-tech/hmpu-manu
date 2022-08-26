library(sf)
library(tidyverse)
library(here)
library(units)

source(here('R/funcs.R'))

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
