library(flextable)
library(tidyverse)
library(here)

# subtidal habitat change ---------------------------------------------------------------------

data(subtacres)

losscol <- 'red'
gaincol <- 'green'

# format for the table
totab <- subtacres %>%
  filter(HMPU_TARGETS %in% c('Seagrasses', 'Tidal Flats', 'Oyster Bars')) %>%
  rename(
    `Habitat Type` = HMPU_TARGETS,
    yr = name,
    Hectares = Acres
  ) %>%
  filter(yr <= 2018) %>%
  mutate(
    Hectares = Hectares / 2.471
  ) %>%
  tidyr::pivot_wider(names_from = yr, values_from = Hectares) %>%
  mutate(
    `1988 to 2018` = `2018` - `1988`,
    `% change` = 100 * (`2018` - `1988`) / `1988`,
    chgcol = ifelse(`% change` < 0, 0, 1)
  )

thm <- function(x){
  flextable::colformat_double(x, digits = 0, na_str = '-') %>%
    flextable::align(align = 'center', part = 'all') %>%
    flextable::align(align = 'left', j = 1, part = 'all') %>%
    flextable::border_inner() %>%
    flextable::width(width = 0.7, j = 1) %>%
    flextable::width(width = 5.8 / (ncol_keys(.) - 1), j = 2:ncol_keys(.)) %>%
    flextable::padding(padding = 0, part = 'all') %>%
    flextable::fontsize(size = 7, part = 'all') %>%
    flextable::padding(padding = 0, part = 'all') %>%
    flextable::fontsize(size = 7, part = 'all')
}

# table
subttab <- flextable::flextable(totab, col_keys = grep('^chgcol$', names(totab), value = T, invert = T)) %>%
  flextable::bg(i = ~ `chgcol` == 0, j = '% change', bg = losscol) %>%
  flextable::bg(i = ~ `chgcol` == 1, j = '% change', bg = gaincol) %>%
  thm

# subttab
save(subttab, file = here('tabs/subttab.RData'))

# inter/suptratidal habitat change ------------------------------------------------------------

data(acres)

losscol <- 'red'
gaincol <- 'green'

# format for the table
totab <- acres %>%
  rename(
    `Habitat Type` = HMPU_TARGETS,
    yr = name,
    Hectares = Acres
  ) %>%
  filter(yr <= 2017) %>%
  mutate(
    Stratum = case_when(
      `Habitat Type` %in% c('Mangrove Forests', 'Salt Barrens', 'Salt Marshes') ~ 'Intertidal',
      T ~ 'Supratidal'
    ),
    Hectares = Hectares / 2.471
  ) %>%
  tidyr::pivot_wider(names_from = yr, values_from = Hectares) %>%
  mutate(
    `1990 to 2017` = `2017` - `1990`,
    `% change` = 100 * (`2017` - `1990`) / `1990`,
    chgcol = case_when(
      `% change` < 0 & `Habitat Type` != 'Developed' ~ 0,
      `% change` >= 0 & `Habitat Type` != 'Developed' ~ 1,
      `% change` < 0 & `Habitat Type` == 'Developed' ~ 1,
      `% change` >= 0 & `Habitat Type` == 'Developed' ~ 0,
    )
  ) %>%
  arrange(Stratum, `Habitat Type`) %>%
  as_grouped_data(groups = 'Stratum')

thm <- function(x){
  flextable::colformat_double(x, digits = 0, na_str = '') %>%
    flextable::align(align = 'center', part = 'all') %>%
    flextable::align(align = 'left', j = 1, part = 'all') %>%
    flextable::border_inner() %>%
    flextable::width(width = 0.7, j = 1) %>%
    flextable::width(width = 5.8 / (ncol_keys(.) - 1), j = 2:ncol_keys(.)) %>%
    flextable::padding(padding = 0, part = 'all') %>%
    flextable::fontsize(size = 7, part = 'all') %>%
    flextable::padding(padding = 0, part = 'all') %>%
    flextable::fontsize(size = 7, part = 'all')
}

# table
acretab <- flextable::flextable(totab, col_keys = grep('^chgcol$', names(totab), value = T, invert = T)) %>%
  flextable::bg(i = ~ `chgcol` == 0, j = '% change', bg = losscol) %>%
  flextable::bg(i = ~ `chgcol` == 1, j = '% change', bg = gaincol) %>%
  thm

# acretab
save(acretab, file = here('tabs/acretab.RData'))
