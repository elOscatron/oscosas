library(readxl)

demographics <- read_excel("data/Libro2.xlsx")

usethis::use_data(demographics, overwrite = T)
