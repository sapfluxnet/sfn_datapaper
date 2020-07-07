# script to generate the DOIs table
# 
# 
# libraries
library(tidyverse)
library(sapfluxnetr)


sapwood_site_md <- read_sfn_metadata('data/0.1.4/RData/sapwood') %>%
  magrittr::extract('site_md')
site_md_all <- read_sfn_metadata('data/0.1.4/RData/plant') %>%
  magrittr::extract('site_md') %>%
  dplyr::bind_rows(sapwood_site_md) %>%
  dplyr::distinct()

dois_table <- site_md_all %>%
  dplyr::select(
    si_code, si_paper, dplyr::starts_with('si_contact'),
    dplyr::starts_with('si_addcontr'),
    -si_addcontr_email, -si_contact_email
  ) %>%
  tidyr::unite(
    col = 'Main_contributor', si_contact_firstname, si_contact_lastname,
    sep = ' '
  ) %>%
  tidyr::unite(
    col = 'Additional_contributor', si_addcontr_firstname, si_addcontr_lastname,
    sep = ' '
  ) %>%
  tidyr::pivot_longer(
    cols = ends_with('_contributor'),
    values_to = 'Contributors',
    names_to = 'Contributor order'
  ) %>%
  dplyr::filter(Contributors != 'NA NA') %>%
  dplyr::mutate(
    `Contributor institution` = dplyr::if_else(
      `Contributor order` == 'Main_contributor', si_contact_institution,
      si_addcontr_institution
    )
  ) %>%
  dplyr::select(
    `Site code` = si_code,
    `Contributor name` = Contributors,
    `Contributor order`,
    `Contributor institution`,
    `DOI` = si_paper
  )


