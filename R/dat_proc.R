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

# opportunity data for opportunity map --------------------------------------------------------

# data copied from commit e6026f831e90cffca8ea906c8fe53c85ceb5746a in hmpu-workflow
# these are the original HMPU layers in the report
data("nativersrv")
data("restorersrv")
data("nativelyr")
data("restorelyr")
data("coastal")

oppdat <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal)

save(oppdat, file = here('data/oppdat.RData'))

# restoration data for restoration potential map ----------------------------------------------

# data copied from commit e6026f831e90cffca8ea906c8fe53c85ceb5746a in hmpu-workflow
# these are the original HMPU layers in the report
data("restorelyr")

restdat <- restdat_fun(restorelyr)

save(restdat, file = here('data/restdat.RData'))

# summary of areas in opportunity and restoration potential -----------------------------------

load(file = here('data/oppdat.RData'))
load(file = here('data/restdat.RData'))

# summarizes area and formats for manu by type in oppareas
areasumfun <- function(x, flt){

  out <- x %>%
    filter(grepl(flt, cat)) %>%
    group_by(var) %>%
    summarise(
      val = sum(val),
      .groups = 'drop'
    ) %>%
    mutate(
      val = case_when(
        var == 'areaha' ~ paste(formatC(round(val, 0), format = 'd', big.mark = ','), 'ha'),
        var == 'areaper' ~ paste0(round(val, 1), '%')
      )
    ) %>%
    deframe %>%
    as.list

  return(out)

}

opparea <- oppdat %>%
  mutate(
    areaha = st_area(.),
    areaha = set_units(areaha, 'hectare')
  ) %>%
  st_set_geometry(NULL) %>%
  group_by(cat) %>%
  summarise(
    areaha = sum(areaha),
    areaper = 100 * (areaha / 587200)
  ) %>%
  pivot_longer(cols = c('areaha', 'areaper'), names_to = 'var', values_to = 'val')

native <- areasumfun(opparea, 'Native')
restorable <- areasumfun(opparea, 'Restorable')
existing <- areasumfun(opparea, 'Existing')
proposed <- areasumfun(opparea, 'Proposed')
reservation <- areasumfun(opparea, 'Reservation')

potential <- restdat %>%
  mutate(
    areaha = st_area(.),
    areaha = set_units(areaha, 'hectare')
  ) %>%
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(
    areaha = sum(areaha),
    areaper = 100 * (areaha / 587200)
  ) %>%
  pivot_longer(cols = c('areaha', 'areaper'), names_to = 'var', values_to = 'val') %>%
  group_by(HMPU_TARGETS) %>%
  nest() %>%
  mutate(
    data = purrr::map(data, function(x){

      x %>%
        mutate(
          val = case_when(
            var == 'areaha' ~ paste(formatC(round(val, 0), format = 'd', big.mark = ','), 'ha'),
            var == 'areaper' ~ paste0(round(val, 1), '%')
          ),
          val = ifelse(val == '0%', '< 1%', val)
        ) %>%
        deframe %>%
        as.list
    })
  ) %>%
  deframe()

areas <- list(
  native = native,
  restorable = restorable,
  existing = existing,
  proposed = proposed,
  reservation = reservation,
  potential = potential
)

save(areas, file = here('data/areas.RData'))

