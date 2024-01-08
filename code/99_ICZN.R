library(rvest)
library(dplyr)

pg <- read_html("https://www.iczn.org/about-the-iczn/commissioners/")

tab <- pg %>% 
  html_table()

cc <- gsub(";.*", "", tab[[1]]$X3)
cc <- gsub(".*\\(|\\)", "", cc)

cc <- countrycode::countrycode(cc, "country.name", "iso3c")

x <- cbind.data.frame(cc, region = countrycode::countrycode(cc,"iso3c", "region23"), 
      continent = countrycode::countrycode(cc,"iso3c", "continent")) 

x2 <- x %>% mutate(continent = ifelse(continent =="Americas", region, continent)) %>% 
  group_by(continent) %>% 
  tally()

x2 %>% mutate( n2 = n/sum(n) * 100)
