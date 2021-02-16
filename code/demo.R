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
			data=tax_sheet[,"category (dinosaur or ichofossils)"], 
			range="O2", # column in which this data is, start with 2 because we don't want to edit the col names
			col_names=FALSE)
