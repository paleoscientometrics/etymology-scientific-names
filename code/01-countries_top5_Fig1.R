## *******************************************
##
## Project: Etymology of dinosaur names
##
## Purpose of script: To create Figure 1
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


library(tidyverse)
library(ggthemes)
library(patchwork)
library(countrycode)
library(grid)
library(svglite)
library(ggsankey) # devtools::install_github("davidsjoberg/ggsankey")

# Load data ---------------------------------------------------------------

dat <- readRDS("data/species.rds")
dat_gen <- readRDS("data/genus.rds")
eponyms <- readRDS("data/eponyms.rds")

## Our countries of interest
cc <- c("ARG", "CAN", "CHN", "DEU", "FRA", "GBR", "MNG", "RUS", "USA") 


# 1. Dinosaurs ---------------------------------------------------------------

## Get 'naming events'
nm_events <- dat %>% select(genus, species, type_cc, primary_reference) %>% 
  bind_rows(
    dat_gen %>% select(genus, type_cc, primary_reference)
  )

## top countries with most naming events:
dino5 <- nm_events %>%
  filter(type_cc %in% cc) %>% 
  group_by(type_cc) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  rename(described=n, code = type_cc) %>% 
  ungroup() 

  

# 2. Researchers -------------------------------------------------------------

## Get reference info for each naming event:
refs_un <- readRDS("data/references.rds") %>% 
  distinct(primary_reference, .keep_all = TRUE) %>% 
  filter(primary_reference %in% c(dat$primary_reference, dat_gen$primary_reference))

## Count publications per country
u_count2 <- data.frame(table(unlist(refs_un$aff_cc2)))
colnames(u_count2) <- c("code", "n")

## top countries with most publications naming dinosaurs:
count5 <- u_count2 %>% 
  arrange(desc(n)) %>% 
  rename(publications=n) %>%
  filter(code %in% cc)  %>% # Mongolia not in top 5
  ungroup()



# 3. First author ------------------------------------------------------------

## Get info on first authors of primary references:
f_count <- lapply(refs_un$aff_cc2, function(x) x[1])

f_count2 <- data.frame(table(unlist(f_count)))
colnames(f_count2) <- c("code", "n")

## top countries with most first-author publications:
fcount5 <- f_count2 %>% 
  filter(code %in% cc)  %>% 
  arrange(desc(n)) %>% 
  rename(first_author=n) %>% 
  ungroup()



# Eponyms -----------------------------------------------------------------

## top countries with most eponyms:
person5 <- eponyms %>% 
  tidyr::unnest(cols = person_cc) %>% 
  group_by(person_cc) %>% 
  tally() %>% 
  filter(person_cc %in% cc) %>% 
  arrange(desc(n)) %>% 
  rename(code=person_cc, all_eponyms=n) %>% 
  ungroup()



# Eponym-foreign ----------------------------------------------------------


## top countries with most eponyms from foreign countries:
fperson5 <- eponyms %>% 
  left_join(
    nm_events %>%  select(genus, species, type_cc)
  ) %>% 
  filter(type_cc != person_cc) %>% 
  unnest(cols = person_cc) %>% 
  group_by(person_cc) %>% 
  tally() %>% 
  rename(code=person_cc, eponyms_foreign=n) %>% 
  arrange(desc(eponyms_foreign)) %>% 
  mutate(rank = 1:nrow(.)) %>% 
  filter(code %in% cc)

# Merge data
df_country <- purrr::reduce(
  list(dino5, count5, fcount5, person5, fperson5), full_join, by="code") 



# Sankey diagram ----------------------------------------------------------

## Create Figure 1

## collect all data from above:
df <- df_country %>% 
  make_long(described, publications, first_author, all_eponyms, eponyms_foreign)

## set up nodes and levels for Sankey diagram:
df$value <- df$node
levels(df$x)
df$country <- rep(countrycode(df_country$code, "iso3c", "country.name"), each=5)

df <- df %>% 
  filter(!is.na(node)) %>% 
  group_by(x) %>% 
  arrange(desc(value)) %>% 
  mutate(order=c(9:1)[1:length(x)]) %>% 
  ungroup()

pal <- RColorBrewer::brewer.pal(7,"Greens")
pal <- c(rep("grey80", 4), pal[3:7])
names(pal) <- 1:9

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node, 
               fill=factor(order),
               value=value)) +
  geom_sankey(flow.alpha = 0.4, node.color = 1) +
  geom_sankey_text(aes(label=country), hjust=1.4) +
  geom_sankey_label(aes(label=value), hjust=-0.2, fill="white", alpha=0.5, 
                    label.r=unit(0.5, "lines")) +
  scale_fill_manual(values=pal, breaks = 9:1, labels=1:9) +
  scale_x_discrete(labels=c("Most dinosaurs described", 
                            "Most publications\ndescribing dinosaurs",
                            "Most first authors\nin publications\ndescribing dinosaurs",
                            "Most dinosaur eponyms",
                            "Most eponyms of dinosaurs\nfoundin another country"))+
  theme_sankey(base_size = 14) +
  guides(fill=guide_legend(ncol=1)) +
  labs(x=NULL, fill="Order") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=10),
        legend.title = element_text(face="bold"))

ggsave("figs/Fig_01_summary.svg", w=10, h=8)
ggsave("figs/Fig_01_summary.pdf", w=10, h=8)
ggsave("figs/Fig_01_summary_raw.png", w=10, h=8)

