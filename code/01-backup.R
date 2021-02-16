# Backup script
library(googlesheets4)

#authenticate
gs4_auth(token=Sys.getenv("GARGLE_PASSWORD"), email = "nussaibahraja@gmail.com")

# sheet id
ss="1tWlbyVkapv7pn2fMu_ewNm4n_wPxXKuAyf04IrwY6Jc"

# read and save
tax_sheet <- range_read(ss, sheet=1)
write.csv(tax_sheet, file.path("archive", paste0(Sys.Date(), "_", "backup.csv")), row.names = FALSE)

