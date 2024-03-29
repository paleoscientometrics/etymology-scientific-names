## *******************************************
##
## Project: Etymology of dinosaur names
##
## Purpose of script: To prepare data
##
## Authors: Nussaïbah B. Raja, Emma M. Dunne
## Copyright (c) N. Raja & E. Dunne, 2024
## Email: nussaibah.raja.schoob@fau.de
##        emma.dunne@fau.de
##
## Date Created: 2022-05-19
## Last Modified: 2024-02-09
##
## *******************************************
##
## Notes:
##   
##
## *******************************************


# Load libraries and set up -----------------------------------------------

library(dplyr)
library(countrycode)
library(magrittr)


# Load data ---------------------------------------------------------------

## species names:
species <- readxl::read_excel("data/final_data.xlsx", sheet = 1) %>% 
  filter(!is.na(data_enterer)) %>% 
  filter(group != "aves") %>% 
  select(genus = 2, accepted_name, genus_cleaned, species = 3, synonyms = 1, taxon_status, ref_pubyr, ref_language, primary_reference, group,
         type_country, country_reposited, sp_named_after, sp_language = starts_with("sp_language"), 
         sp_if_person_name, sp_if_person_country, aff_country = starts_with("countries_of_authors"),
         reason_offence = "reason for potential offence")

nrow(species) #1715 with aves, 1425 without

## genus names only
genus <- readxl::read_excel("data/final_data.xlsx", sheet = 2) %>% 
  filter(group != "aves") %>% 
  select(genus = 1, genus_cleaned, ref_pubyr, primary_reference, group, taxon_status, gen_named_after, 
         gen_language = starts_with("gen_language"), gen_if_person_name, gen_if_person_country,
         reason_offence = "reason for potential offence")

nrow(genus) #1246 without aves

## data on country history
countries_hist <- read.csv("data/countries_history.csv")
names(countries_hist)[7] <- "colonial"

countries_hist %<>%  # modify some parts of this:
  mutate(year_end = as.numeric(ifelse(year_end == "present", "2023", year_end))) %>% 
  mutate(cc = countrycode(modern_border, "country.name", "iso3c"),
         cc2 = countrycode(territory_of, "country.name", "iso3c"))

countries_hist %>% # search for duplicates
  janitor::get_dupes(entry)

names_sp <- names(species) # get column names from species data


## add old borders to some countries
species %<>% 
  left_join(countries_hist %>% 
              filter(!entry %in% c("Germany", "Tanzania", "South Africa", "Mongolia")), c("type_country" = "entry")) %>% 
  rowwise() %>% 
  mutate(
    year_end = ifelse(is.na(year_end), -9999, year_end),
    year_start = ifelse(is.na(year_start), -9999, year_start),
    type_country2 = ifelse(between(
      ref_pubyr, year_start, year_end), territory_of, type_country),
    type_country = ifelse(between(
      ref_pubyr, year_start, year_end), modern_border, type_country)) %>% 
  ungroup() %>% 
  select(all_of(names_sp), type_country2, colonial) %>% 
  relocate(
    type_country2, .after =type_country
  )

## deal with Mongolia, South Africa and Tanzania
sp_countries <- countries_hist %>% 
  filter(entry %in% c("Tanzania", "South Africa", "Mongolia"))

for(i in 1:nrow(sp_countries)){
  
  tmp <- sp_countries[i,]
  
  species %<>% 
    mutate(type_country2 = ifelse(type_country == tmp$entry & between(ref_pubyr, tmp$year_start, tmp$year_end),
                                  tmp$territory_of, type_country2))
}

## Changes USSR to Russia
species %>% # how many?
  filter(type_country2 == "USSR") %>%  nrow() # 16

species %<>% 
  mutate(type_country2 = ifelse(type_country2 == "USSR", "Russia", type_country2))


## add country codes
species %<>% 
  mutate(across(c(type_country, type_country2), ~ countrycode(.x, "country.name", "iso3c"),
                .names = "{gsub('country', 'cc', .col)}"), 
         .after = type_country2)

## add type_country
genus <- genus %>% left_join(
  species %>% distinct(ref_pubyr, genus, type_cc, type_cc2) 
)

## identify duplicates
genus %>% 
  janitor::get_dupes(genus) 

## filter these out
genus <- genus %>% 
  filter(!(genus == "Brodavis" & type_cc != "CAN")) %>% 
  filter(!(genus == "Martinavis" & type_cc != "FRA"))

genus %>% 
  janitor::get_dupes(genus) # 0, good


## Save copies to folder:
saveRDS(species, "data/species.rds")
saveRDS(genus, "data/genus.rds")




# Clean affiliations ------------------------------------------------------

## Get affiliation data:
refs_un <- readxl::read_excel("data/final_data.xlsx", sheet=3)
refs_un$aff_country <- gsub(",", ";", refs_un$aff_country)

refs_un$aff_country[refs_un$aff_country == "Unknown"] <- NA
any(grepl(",", refs_un$aff_country)) # FALSE

## Clean up affiliations
affs <- strsplit(refs_un$aff_country,";")
counts <- strsplit(refs_un$aff_country, ";")

n <- unlist(lapply(counts, length))

for(i in which(n==1)){
  if(!is.na(counts[[i]])){
    counts[[i]] <- rep(counts[[i]], length(affs[[i]]))
    
  }
}


future::plan("multisession", workers=7)

u_count <- furrr::future_map(counts, ~{
  .x <- unique(trimws(gsub("\\(.+\\)", "", .x)))
  
  .x <- countrycode(.x, "country.name", "iso3c")
  unique(.x) # removes duplicated countries, e.g. China; China; USA. 
})

future::plan("sequential")

refs_un %<>% 
  select(primary_reference, ref_pubyr) %>% 
  tibble::add_column(aff_cc = u_count) 

refs_un$aff_cc2 <- NA

for(i in 1:nrow(countries_hist)){
  tmp <- countries_hist[i,]
  
  refs_un %<>% 
    mutate(aff_cc2 = ifelse(between(ref_pubyr, tmp$year_start, tmp$year_end) & is.na(aff_cc2),
                            list(
      gsub(tmp$modern_border, tmp$territory_of, unlist(aff_cc))), aff_cc))
  
}

## Save to folder:
saveRDS(refs_un, "data/references.rds")



# Eponyms -----------------------------------------------------------------

## Load eponyms data
eponyms <- readxl::read_excel("data/person_data.xlsx")

## filter species data
epo_sp <- species %>% 
  filter(sp_named_after == "person") %>% 
  select(genus, species, person_country = sp_if_person_country) %>% 
  mutate(taxon_name = paste(genus, species), .before =genus)

## and genus data
epo_gen <- genus %>% 
  filter(gen_named_after == "person") %>% 
  select(genus, person_country = gen_if_person_country) %>% 
  mutate(taxon_name = genus, .before =genus)

epo_all <- bind_rows(epo_sp, epo_gen) # bind
  
janitor::get_dupes(epo_all, taxon_name) #0
janitor::get_dupes(eponyms, taxon_name) #0

## join to main eponyms data object
epo_all <- left_join(epo_all, eponyms, by = "taxon_name")

## Clean up:
epo_all$person_country[epo_all$person_country %in% c("German", 
                                                     "Germany (born in what was then Prussia but is now part of Poland)")] <- "Germany"
epo_all$person_country[epo_all$person_country %in% c("Mongol Empire")] <- "Mongolia"

epo_all %<>% 
  mutate(person_country = strsplit(person_country, ";"),
         person_cc = lapply(person_country, countrycode, origin = "country.name", destination = "iso3c"))

# Note: not given a country: =
# * Roman Republic
# * Gurkani (Timurid Empire)
# * Unknown

## save to folder
saveRDS(epo_all, "data/eponyms.rds")
