## ---------------------------
##
## Project: Etymology of Dinosaur Names
##
## Purpose of script: 
## Summarises the different etymologies of dinosaur names overall and over time
##
## Author: Nussaïbah B. Raja
## Copyright (c) N. Raja, 2021
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2021-08-16
## Last Modified: 2022-09-18
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# Load packages -----------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(patchwork)
library(rvest)

# Plot option -------------------------------------------------------------

theme_set(theme_hc() +
            theme(axis.title.y = element_text(angle=90, vjust=1),
                  axis.title = element_text(face="bold"),
                  legend.title = element_text(face="bold"),
                  plot.tag = element_text(size=10))
)

# Load data ---------------------------------------------------------------

dat_sp <- readRDS("data/species.rds")
dat_gen <- readRDS("data/genus.rds")

sp <- dat_sp %>% select(genus, species, ref_pubyr, accepted_name, language = sp_language, type_cc, type_cc2, named_after = sp_named_after)

gen <- dat_gen %>%  select(genus, ref_pubyr, language = gen_language, type_cc, type_cc2, named_after = gen_named_after)

dat <- bind_rows(sp, gen)

dat <- dat %>% 
  mutate(language_clean = case_when(
    is.na(language) ~ "Latin/Greek",
    language %in%c("Latin", "Greek", "N/A", "Greek; Latin", "Latin; Greek",
                   "Greek and Latin", "Greek/Latin", "Latin+Greek", 
                   "Latin/Greek hybrid", "Ancient Greek", "Greek & Latin",
                   "Latin;Greek", "Latinised Greek", "Greek; latin", 
                   "Greek?") ~ "Latin/Greek",
    grepl("Chinese|Pinyin|Mandarin", language)~ "Chinese",
    language %in% c("Spanish and Greek", "Latin & Spanish",
                    "Latin; Spanish", "Spanish+Greek", "Spanish/Greek",
                    "Spanish; Latin", "Greek; Spanish") ~ "Spanish",
    language %in% c("Old English",
                    "early 20th century African-American English")~ "English",
    grepl("Mapuche", language) ~ "Mapuche",
    grepl("Tehuelch", language) ~ "Tehuelche",
    TRUE ~ language))

dat$language_clean[dat$named_after %in% c("person", "group", "company/corporation", 
                                          "location/geography", "age", "folklore/religion")] <- NA

dat <- dat %>% filter(!is.na(language_clean))

table(dat$named_after)
# behavioural characteristics      environmental conditions     in relation to other taxa 
# 109                            54                           121 
# morphological characteristics                         other                          size 
# 539                           181                            96 
# unknown 
# 12

dat %>% 
  group_by(language_clean) %>% 
  tally() %>% 
  arrange(desc(n)) %>%  View()


# trying to categorise the languages
pg <- read_html("https://www.infoplease.com/countries/languages-spoken-in-each-country-of-the-world")

country_lang <- pg %>% 
  html_table() 

country_lang <- country_lang[[1]]

# adding others
country_lang[country_lang$Country == "Argentina", 2] <- gsub("indigenous \\(", "indigenous \\(Mapuche, 	Tehuelche", country_lang[country_lang$Country == "Argentina", 2])
country_lang[country_lang$Country == "Brazil", 2] <- paste(country_lang[country_lang$Country == "Brazil", 2], 
                                                           ", indigenous (Tupi, Guarani, Quechua, Tupian, Tupi-Guarani)")
country_lang[country_lang$Country == "United States", 2] <- paste(country_lang[country_lang$Country == "Brazil", 2], 
                                                                  ", indigenous (Sioux, Blackfeet Indian, Zuni, Absaroka, Iñupiaq,",
                                                                  "Blackfoot", "Crow", "Oglala", "Arapaho", "Ute)")
country_lang[country_lang$Country == "Uzbekistan", 2] <- paste(country_lang[country_lang$Country == "Uzbekistan", 2], 
                                                               "Kazakh")
country_lang <- country_lang %>% add_row(Country = "Greenland", `Recognized Languages` = "Kalaallisut")


dat <- dat %>% 
  mutate(local_language = map2_chr(language_clean, type_cc, \(x, y){
    
    n <- grep(x, country_lang$`Recognized Languages`)
    
    if(length(n) > 0 & x != "Latin/Greek"){
      
      # look for country
      # we assume that if a language is spoken in a country, these will be a local language
      # so far not taking into consideration the indigeneity of the language
      cc <- countrycode::countrycode(country_lang$Country[n], origin = "country.name", destination = "iso3c")
      
      if(y %in% cc) res <- "yes" else res <- "unknown"
      
    } else res <- "unknown"
    
    # browser()
    return(res)
    
  })
  
  )

dat$local_language[dat$language_clean == "Latin/Greek"] <- "ignore"


View(dat %>% filter(local_language == "unknown"))

# categorie remaining ones
dat[grepl("Egyptian|-Latin", dat$language_clean) & dat$local_language == "unknown","local_language"] <- "no"
dat[grepl("English|Persian|Malay|Spanish|Arabic", dat$language_clean) & dat$local_language == "unknown","local_language"] <- "no"
dat[dat$local_language == "unknown","local_language"] <- "yes"

dat <- dat %>% 
  filter(!is.na(language_clean)) %>%
  mutate(language_cat = case_when(language_clean %in% c("Latin/Greek", "Chinese") ~ language_clean,
                                  TRUE ~ "Other languages")) 

table(dat$language_cat) %>% 
  prop.table()

pal <- RColorBrewer::brewer.pal(5, "PRGn")[-1]
pal <- pal[c(2,1,3,4)]
pal <- scales::alpha(pal, 0.9)

dat %>% 
  ggplot() +
  geom_density(aes(ref_pubyr, after_stat(count), fill = language_cat),
               position = "fill", colour="grey80") +
  scale_fill_manual(values=pal) +
  labs(x="Year of publication", y=glue::glue("Percentage of dinosaur names"),
       fill="Languages") +
  scale_y_continuous(breaks=seq(0,1, 0.25),
                     labels=seq(0,1, 0.25)*100)

ggsave("figs/Fig_S_languages.svg", w = 8, h = 5)

dat <- dat %>% 
  mutate(region = countrycode::countrycode(type_cc, "iso3c", "region23"))

dat %>% 
  filter(region %in% "South America") %>% 
  group_by(type_cc) %>% 
  tally() 

dat %>% 
  filter(region %in% "South America") %>% 
  group_by(language_clean) %>% 
  tally() 

dat$local_language[dat$local_language != "ignore"] %>% 
  table() %>% 
  prop.table()
