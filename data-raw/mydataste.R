library(openxlsx)

Libro2 <- read_excel("data-raw/Libro2.xlsx")

# Data cleaning code here...
# (Do NOT put data analysis code here!)

# This should be the last line.
# Note that names are unquoted.
# I like using overwrite = T so everytime I run the script the
# updated objects are saved, but the default is overwrite = F
usethis::use_data(Libro2, overwrite = T)
