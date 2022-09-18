library(tidyverse)
library(future)
library(furrr)
library(ggthemes)
library(patchwork)
library(countrycode)
library(grid)
library(ggsankey)

# Load data ---------------------------------------------------------------

dat <- read.csv(file.path("data", "dino_data_cleaned.csv"), fileEncoding = "UTF-8") %>% 
  filter(data_enterer != "" & !group %in% c("trace", "egg"))

cc <- c("ARG", "CAN", "CHN", "DEU", "FRA", "GBR", "MNG", "RUS", "USA") # countries of interest

# Dinosaurs ---------------------------------------------------------------

# disputed territories: 
# Nipponosaurus sachalinensis - Russia now, then Japan -> Japan
# Riabininohadros weberae - USSR (then), Ukraine/Russia (disputed now) -> Russia
# Telmatosaurus transsylvanicus - Romania (but then it was part of Hungary) -> Hungary
# Zalmoxes robustus - Romania (but then it was part of Hungary) -> Hungary

dat$code <- countrycode(dat$type_country, "country.name", "iso3c")
dat$code[dat$type_country=="Russia (then part of Japan)"] <- "JPN"
dat$code[dat$type_country=="USSR (then), Ukraine/Russia (disputed now)"] <- "RUS"
dat$code[dat$type_country=="Romania (but then it was part of Hungary)"] <- "HUN" 

n <- which(is.na(dat$code) & !is.na(dat$type_country) & dat$type_country != "")
dat$type_country[n] # 0

dino5 <- dat %>%
  filter(category=="sp" & 
           !taxon_status %in% c("invalid", "corrected", "corrected_to",
         "invalid", "invalid (other)", "nomen dubium", "nomen nudum",
         "nomen oblitum"
         )) %>% 
  filter(code %in% cc) %>% 
  group_by(code) %>% 
  tally() %>% 
  na.omit() %>% 
  arrange(desc(n)) %>% 
  rename(described=n) %>% 
  ungroup() 
  
  
# UK not in top 5
  
# Researchers -------------------------------------------------------------

# affiliations with only one country added
refs_un <- dat %>% distinct(primary_reference, .keep_all = T) # removing duplicated references
refs_un$countries_of_authors..separated.by... <- gsub(",", ";", refs_un$countries_of_authors..separated.by...)

any(grepl(",", refs_un$countries_of_authors..separated.by...)) # FALSE

# cleaning affs
affs <- strsplit(refs_un$affiliations_of_authors..separated.by...,";")
counts <- strsplit(refs_un$countries_of_authors..separated.by..., ";")

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

u_count2 <- data.frame(table(unlist(u_count)))
colnames(u_count2) <- c("code", "n")

count5 <- u_count2 %>% 
  arrange(desc(n)) %>% 
  rename(publications=n) %>%
  filter(code %in% cc)  %>% # Mongolia not in top 5
  ungroup()

# First author ------------------------------------------------------------
f_count <- lapply(u_count, function(x) x[1])

f_count2 <- data.frame(table(unlist(f_count)))
colnames(f_count2) <- c("code", "n")

fcount5 <- f_count2 %>% 
  filter(code %in% cc)  %>% 
  arrange(desc(n)) %>% 
  rename(first_author=n) %>% 
  ungroup()

# Eponyms -----------------------------------------------------------------
df.person <- dat %>%  filter(sp_named_after == "person") %>% 
  select(ref_pubyr, taxon_name, type_country, sp_if_person_country, 
         sp_if_person_gender,
         author_aff = affiliations_of_authors..separated.by...,
         author_country = countries_of_authors..separated.by...,
         type_code=code, category)

# some cleaning
df.person$sp_if_person_country <- gsub(",", ";", df.person$sp_if_person_country)
df.person$sp_if_person_country <- strsplit(df.person$sp_if_person_country, ";")
df.person <- unnest(df.person)

df.person$sp_if_person_country[df.person$sp_if_person_country=="German"] <- "Germany"
df.person$sp_if_person_country[df.person$sp_if_person_country=="Mongol Empire"] <- "Mongolia"

# no included roman republic

df.person$person_code <-  countrycode(df.person$sp_if_person_country, "country.name", "iso3c")

# genera
df.per_gen <- dat %>%  filter(gen_named_after=="person") %>% 
  select(ref_pubyr, taxon_name=genus, type_country, gen_if_person_country, 
         author_aff = affiliations_of_authors..separated.by...,
         author_country = countries_of_authors..separated.by...,
         type_code=code, category) %>% 
  arrange(category) %>% 
  distinct(taxon_name, .keep_all = T) %>% 
  mutate(gen_if_person_country= case_when(gen_if_person_country=="Columbia"~ "Colombia",
                                          gen_if_person_country=="French"~ "France",
                                          gen_if_person_country=="Italian"~ "Italy",
                                          TRUE~gen_if_person_country),
         gen_if_person_country = strsplit(gen_if_person_country, ";")) %>% 
  unnest()

df.per_gen$person_code <- countrycode(df.per_gen$gen_if_person_country, "country.name", "iso3c")
df.per_gen$person_code[df.per_gen$gen_if_person_country == "Germany (born in what was then Prussia but is now part of Poland)"] <- "DEU"

# ditched: Roman Republic, Timurid Empire

df.person <- df.person %>%  select(taxon_name, type_code, person_code) %>% 
  bind_rows(df.per_gen %>%  select(taxon_name, type_code, person_code))

person5 <- df.person %>% group_by(person_code) %>% 
  tally() %>% 
  na.omit() %>% 
  filter(person_code %in% cc) %>% 
  arrange(desc(n)) %>% 
  rename(code=person_code, all_eponyms=n) %>% 
  ungroup()


# Eponym-foreign ----------------------------------------------------------

fperson5 <- df.person %>% 
  na.omit() %>% 
  filter(type_code != person_code) %>% 
  filter(person_code %in% cc) %>% 
  group_by(person_code) %>% 
  tally() %>% 
  rename(code=person_code, eponyms_foreign=n)

# Merge data
df_country <- purrr::reduce(
  list(dino5, count5, fcount5, person5, fperson5), full_join, by="code") 

# Sankey diagram ----------------------------------------------------------
df <- df_country %>% 
  make_long(described, publications, first_author, all_eponyms, eponyms_foreign)

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
  theme(legend.position = "right",
        axis.text.x = element_text(size=10),
        legend.title = element_text(face="bold"))

ggsave("figs/Fig_01_summary.svg", w=10, h=8)
