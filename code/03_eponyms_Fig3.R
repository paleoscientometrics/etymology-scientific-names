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
library(magrittr)
library(ggthemes)
library(patchwork)
library(countrycode)
library(grid)

# Plot option -------------------------------------------------------------
theme_set(theme_hc() +
            theme(axis.title.y = element_text(angle=90, vjust=1),
                  axis.title = element_text(face="bold"),
                  legend.title = element_text(face="bold"),
                  plot.tag = element_text(size=10))
)

pal <- c("#f0ead2", "#dde5b6", "#adc178", "#a98467", "#6c584c")

# Load data ---------------------------------------------------------------

dat_sp <- readRDS("data/species.rds")
dat_gen <- readRDS("data/genus.rds")

df.person <- readxl::read_excel("data/person_data.xlsx") 

dat_sp %<>% #filter(sp_named_after %in% c("group", "person")) %>% 
  select(taxon_name = accepted_name, genus, species, type_cc, type_cc2, person_name = sp_if_person_name, person_country = sp_if_person_country)

dat_gen %<>% #filter(gen_named_after %in% c("group", "person")) %>% 
  select(genus, taxon_name = genus, type_cc, type_cc2, person_name = gen_if_person_name, person_country = gen_if_person_country)

dat <- bind_rows(dat_gen, dat_sp)

df.person <- df.person %>% left_join(
  dat %>% select(taxon_name, type_cc, type_cc2, person_name, person_country), by=c("taxon_name"))

rem <- janitor::get_dupes(df.person, taxon_name) %>%  
  filter(is.na(person_name.y))

df.person <- anti_join(df.person, rem) 

dupes <- janitor::get_dupes(df.person, taxon_name) # some issues with joining

rem <- dupes %>% 
  mutate(dist = stringdist::stringdist(person_name.x, person_name.y)) %>%  
  select(taxon_name, person_name.x, person_name.y, dist) %>% 
  group_by(taxon_name) %>% 
  mutate(sel = min(dist)) %>% 
  filter(dist != sel)

df.person <- anti_join(df.person, rem)

missing <- df.person %>% 
  filter(is.na(person_country)) %>% 
  separate(taxon_name, into=c("genus", "species"), sep = " ")

missing <- missing %>% 
  select(all_of(names(missing)[1:10])) %>% 
  rename(person_name = person_name.x) %>% 
  left_join(dat_sp, by=c("genus", "species")) %>% 
  mutate(taxon_name = paste(genus, species)) %>% 
  select(all_of(names(df.person)))

df.person %<>% 
  filter(!is.na(person_country)) %>% 
  bind_rows(missing)

# still some missing but we'll just ignore them

df.person <- df.person %>% 
  filter(!is.na(person_country))

nrow(df.person)

# Country -----------------------------------------------------------------
df.person$person_country[grepl("German", df.person$person_country)] <- "Germany"
df.person$person_country[df.person$person_country == "UK (Wales)"] <- "UK"

df.person <- df.person %>% 
  mutate(person_country = map(person_country, ~{strsplit(.x, split = ";")[[1]]}),
         person_cc = map(person_country, ~{countrycode(.x, "country.name", "iso3c", nomatch=NULL)}))


# check the empty ones and those that cannot be assigned a country
n <- which(!sapply(df.person$person_cc, function(x) all(grepl(pattern = "[A-Z]{3}", x))))
df.person[n,] %>%  View() #those are ok

df.person <- df.person_og <- df.person %>%  distinct(taxon_name, .keep_all = TRUE)

df.person <- df.person %>% 
  unnest(cols = person_cc) %>% 
  mutate(local = ifelse(type_cc == person_cc, "yes", "no"))

# check on the dups
dupes <- df.person %>% 
  janitor::get_dupes(taxon_name) %>% 
  filter(local == "yes")

df.person$local[df.person$taxon_name %in% dupes$taxon_name] <- "yes"

df.person <- df.person %>% 
  distinct(df.person$taxon_name, .keep_all = TRUE)

nrow(df.person) == nrow(df.person_og)

tot <- nrow(df.person)

prop.table(table(df.person$local))

top_local <- df.person %>% group_by(type_cc, local) %>% 
  tally() %>% 
  group_by(type_cc) %>% 
  mutate( tot = sum(n), 
          prop = round(n/sum(n), 3) * 100) %>% 
  filter(local=="yes" & tot > 5) %>% 
  arrange(desc(prop))

top_local <- top_local %>% mutate(country = countrycode(type_cc, "iso3c", "country.name"),
                     region = countrycode(type_cc, "iso3c", "region23")) %>% 
  ungroup() %>% 
  select("Source country" = country, "Source region"=region, 
         "Number of local eponyms"=n, "Total eponyms"=tot, "%" = prop)
  
write.csv(top_local, "output/top_local.csv", row.names = F)

df.person %>% filter(type_cc == "MNG") %>% group_by(person_cc) %>% 
  tally()

# A tibble: 6 × 2
# person_cc     n
# <chr>     <int>
# 1 FRA           1
# 2 JPN           1
# 3 MNG           3
# 4 POL           6
# 5 RUS          13
# 6 USA          11


# Reasons -----------------------------------------------------------------
df.person <- df.person_og

reasons <- df.person %>% 
  mutate(ending = case_when(grepl("i$", taxon_name) ~ "-i",
                            grepl("ae$", taxon_name) ~ "-ae",
                            grepl("orum$", taxon_name) ~ "-orum/-arum",
                            grepl("arum$", taxon_name) ~ "-orum/-arum", # only one
                            TRUE ~ "other")) %>% 
  pivot_longer(cols=contains("reason"), names_to = "reason_type", values_to = "reason") %>% 
  select(-reason_type) %>% 
  filter(reason != "")

reasons %>% group_by(ending) %>%  tally() %>% 
  ungroup() %>%  mutate(prop = n / nrow(df.person) *100)

# # A tibble: 4 × 3
# ending          n  prop
# <chr>       <int> <dbl>
#   1 -ae            47  7.67
# 2 -i            612 99.8 
# 3 -orum/-arum    24  3.92
# 4 other          19  3.10

reasons %>% filter(ending == "other") %>% View()

ending_name <- reasons %>% 
  distinct(taxon_name, .keep_all = TRUE) %>% 
  group_by(ending, name) %>%  tally() %>% 
  group_by(ending) %>% 
  mutate(prop = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = name, values_from = prop) 

write.csv(ending_name, "output/ending_name.csv", row.names = FALSE)

ord <- table(reasons$reason) %>% 
  enframe() 

#write.csv(ord, "data/reasons.csv", row.names = FALSE)
ord <- read.csv("data/reasons.csv")

codes <- ord %>% 
  select(old, new) %>% 
  deframe()

reasons %<>% 
  mutate(reason = recode(reason, !!!codes))

ord <- ord %>% 
  distinct(new)


reasons_summ <- reasons %>% 
  group_by(reason, ending) %>% 
  tally() %>% 
  mutate(ending = factor(ending, levels=rev(c("-i", "-ae", "-orum/-arum", "other"))),
         reason = factor(reason, levels=rev(ord$new)))

reasons_summ2 <- reasons %>% 
  group_by(reason, ending, name) %>% 
  tally() %>% 
  mutate(ending = factor(ending, levels=rev(c("-i", "-ae", "-orum/-arum", "other"))),
         reason = factor(reason, levels=rev(ord$new)))

# professional capacity
ending_name_pro <- reasons_summ2 %>%  filter(ending %in% c("-i", "-ae")) %>% 
  mutate(professional = ifelse(reason %in% c("other", "family/friends/mentor", "hospitality/assistance", 
                                             "funder/donor/land owner"), "no", "yes")) %>% 
  group_by(professional, ending, name) %>% 
  summarise(n=sum(n)) %>% 
  ungroup() %>%  mutate(prop = n/sum(n))

write.csv(ending_name, "output/ending_name_pro.csv", row.names = FALSE)

pal <- RColorBrewer::brewer.pal(5, "PRGn")[c(1,2,5)]
pal <- c(pal, "lightgrey")
names(pal) <- c("-ae", "-orum/-arum", "-i", "other")
pal <- pal[c("-i", "-ae", "-orum/-arum", "other")]

ggplot(reasons_summ, aes(x=reason, y=n,
                         fill=ending)) +
  geom_bar(stat="identity", width = 0.6) +
  coord_flip()+
  labs(y = "number of eponyms", x="",
       fill="eponym ending") +
  scale_fill_manual(values=pal)

ggsave("figs/Fig_03_reasons.svg", width=8, height=5)  
ggsave("figs/Fig_03_reasons.png", width=8, height=5) 

