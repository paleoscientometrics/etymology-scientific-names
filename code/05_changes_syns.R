sp <- readxl::read_excel("data/final_data.xlsx", sheet = 1) 
gen <- readxl::read_excel("data/final_data.xlsx", sheet = 2) 

syn <- sp$SYNONYMY

syn_change <- grep("originally|also", syn, value = TRUE, ignore.case = TRUE)

syn_change <- gsub("also|originally| revised to|, grammar corrected but same etymology|identified as|", "", syn_change, ignore.case = T)

syn_change <- trimws(unlist(strsplit(syn_change, split = ",|and|;")))
syn_change <- syn_change[syn_change != ""]

length(syn_change) / (nrow(sp) + nrow(gen))

offense <- c(sp$`reason for potential offence`, gen$`reason for potential offence`)

offense <- offense[offense != "-" & !is.na(offense)]
length(offense) / (nrow(sp) + nrow(gen))
