suppressMessages(library(pacman))
pacman::p_load(tidyverse, readxl)

fld_book <- readxl::read_excel(path = "D:/Bean_project/_photos/Fieldbook_climbing_DAR18B.xlsx", sheet = "Observation")
table(fld_book$`previous generation`)
fld_book$`Unique Identifier`[fld_book$`previous generation` == "17APB01085.000"]
fld_book$`Unique Identifier`[fld_book$`previous generation` == "17APB01372.000"]
