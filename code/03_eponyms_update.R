## ---------------------------
##
## Project: Etymology of Dinosaur Names
##
## Purpose of script: 
## Compares the country of the eponym to the type country and the affiliation(s) of
## the "namer(s)".
##
## Author: Nussaïbah B. Raja
## Copyright (c) N. Raja, 2022
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2021-08-16
## Last Modified: 2022-10-23
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
library(extrafont)
library(patchwork)
library(countrycode)
library(grid)

# Plot option -------------------------------------------------------------
extrafont::loadfonts()

theme_set(theme_hc(base_family = "Roboto") +
            theme(axis.title.y = element_text(angle=90, vjust=1),
                  axis.title = element_text(face="bold"),
                  legend.title = element_text(face="bold"),
                  plot.tag = element_text(size=10))
)

pal <- c("#f0ead2", "#dde5b6", "#adc178", "#a98467", "#6c584c")

# Load data ---------------------------------------------------------------

dat <- read.csv(file.path("data", "dino_data.csv")) %>% 
  filter(data_enterer != "" & group != "trace")

df.person <- read.csv(file.path("data", "person_data.csv")) 

df.person <- df.person %>% left_join(
  dat %>% select(taxon_name, type_country, sp_if_person_country) )

# Country -----------------------------------------------------------------

# Note: need to sort out some country name, old names, disputed

df.person$type_code <- countrycode(df.person$type_country, "country.name", "iso3c", nomatch = NULL) 
df.person$person_code <-    countrycode(df.person$sp_if_person_country, "country.name", "iso3c", nomatch=NULL)

# TODO: check the empty ones and those that cannot be assigned a country

df.person <- df.person %>% mutate(local = ifelse(type_code == person_code, "yes", "no"))
tot <- nrow(df.person)

prop.table(table(df.person$local))

top_local <- df.person %>% group_by(type_code, local) %>% 
  tally() %>% 
  group_by(type_code) %>% 
  mutate( tot = sum(n), 
          prop = round(n/sum(n), 3) * 100) %>% 
  filter(local=="yes" & tot > 5) %>% 
  arrange(desc(prop))

top_local <- top_local %>% mutate(country = countrycode(type_code, "iso3c", "country.name"),
                     region = countrycode(type_code, "iso3c", "region23")) %>% 
  ungroup() %>% 
  select("Source country" = country, "Source region"=region, 
         "Number of local eponyms"=n, "Total eponyms"=tot, "%" = prop)
  
write.csv(top_local, "output/top_local.csv", row.names = F)

df.person %>% filter(type_code == "MNG") %>% group_by(person_code) %>% 
  tally()

df.person %>% filter(type_country)

# Reasons -----------------------------------------------------------------




reasons <- df.person %>% 
  mutate(ending = case_when(grepl("i$", taxon_name) ~ "-i",
                            grepl("ae$", taxon_name) ~ "-ae",
                            grepl("orum$", taxon_name) ~ "-orum",
                            grepl("arum$", taxon_name) ~ "-arum",
                            TRUE ~ "other")) %>% 
  pivot_longer(cols=contains("reason.1"), names_to = "reason_type", values_to = "reason") %>% 
  select(-reason_type) %>% 
  filter(reason != "")

reasons %>% group_by(ending) %>%  tally() %>% 
  ungroup() %>%  mutate(prop = n / sum(n)*100)

nrow(reasons)

reasons %>% filter(ending == "-arum") %>% View()
reasons %>% filter(ending == "other") %>% View()

reasons %>% group_by(ending, name) %>%  tally() %>% 
  group_by(ending) %>% 
  mutate(prop = n / sum(n)) %>%  View()

reasons_summ <- reasons %>% 
  group_by(reason, ending) %>% 
  tally() %>% 
  mutate(ending = factor(ending, levels=rev(c("-i", "-ae", "-orum", "other"))),
         reason = factor(reason, levels=rev(c("contributions to paleontology/geology",
                                          "discoverer/collector",
                                          "funder/donor/land owner",
                                          "research leader/facilitator",
                                          "museum personnel",
                                          "family/friends",
                                          "hospitality/assistance",
                                          "other"))))

reasons_summ2 <- reasons %>% 
  group_by(reason, ending, name) %>% 
  tally() %>% 
  mutate(ending = factor(ending, levels=rev(c("-i", "-ae", "-orum", "other"))),
         reason = factor(reason, levels=rev(c("contributions to paleontology/geology",
                                              "discoverer/collector",
                                              "funder/donor/land owner",
                                              "research leader/facilitator",
                                              "museum personnel",
                                              "family/friends",
                                              "hospitality/assistance",
                                              "other"))))

# professional capacity
reasons_summ2 %>%  filter(ending=="-ae") %>% 
  mutate(professional = ifelse(reason %in% c("other", "family/friends", "hospitality/assistance"), "no", "yes")) %>% 
  group_by(professional, name) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>%  mutate(prop = n/sum(n))


pal <- RColorBrewer::brewer.pal(5, "PRGn")[c(1,2,5)]
pal <- c(pal, "lightgrey")
names(pal) <- c("-ae", "-orum", "-i", "other")
pal <- pal[c("-i", "-ae", "-orum", "other")]

ggplot(reasons_summ, aes(x=reason, y=n,
                         fill=ending)) +
  geom_bar(stat="identity", width = 0.6) +
  coord_flip()+
  labs(y = "number of cases", x="reasons",
       fill="eponym ending") +
  scale_fill_manual(values=pal)

ggsave("figs/Fig_03_reasons.svg", width=8, height=5)  


