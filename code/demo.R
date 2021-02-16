library(googlesheets4)

#authenticate
gs4_auth(
	cache = ".secrets",
	email = "nussaibahraja@gmail.com" #change to your email
)

# sheet id
ss="1tWlbyVkapv7pn2fMu_ewNm4n_wPxXKuAyf04IrwY6Jc"

# read sheet
tax_sheet <- range_read(ss, sheet=1)

# make some updates 
tax_sheet$`category (dinosaur or ichofossils)`[grep("pes$|pus$", tax_sheet$genus)] <- "maybe ichnofossil" 

range_write(ss=ss, 
			data=data.frame(tax_sheet$`category (dinosaur or ichofossil)`),
			range="O2", # column in which this data is, start with 2 because we don't want to edit the col names
			col_names=FALSE)

# to overwrite the whole thing
range_write(ss=ss, 
			data=tax_sheet, 
			range="A1", # column in which this data is, start with 2 because we don't want to edit the col names
			col_names=TRUE)



## Import .txt file of dinosaur trace taxa names:
trace_terms <- readLines("./data/Dino_trace_terms.txt")
trace_terms <- trace_terms[trace_terms != ""]
exclude_terms <- c(trace_terms,
                   "Fenestrosaurus","Ovoraptor","Ornithoides" #names from popular article by Osborne (1925) that should not be in the database
)
exclude_terms <- exclude_terms[exclude_terms != ""] #remove any blank entries that may have crept in

category <- rep(NA, length(exclude_terms))
n <- grep(" ", exclude_terms)
category[n] <- "sp"
category[-n] <- "genus"

tax_sheet$`category (dinosaur or ichofossil)`[tax_sheet$accepted_name %in% exclude_terms[n]] <- "trace"
tax_sheet$`category (dinosaur or ichofossil)`[tax_sheet$genus %in% exclude_terms[-n]] <- "trace"


