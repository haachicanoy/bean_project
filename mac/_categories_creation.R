# Creating categories for classification test
# H. Achicanoy
# Universidad del Valle, 2019

options(scipen = 999, warn = -1)

suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, readxl))

# Loading field book
fld_book <- readxl::read_excel(path = "./Documents/Data/Computer_vision/thesis/Fieldbook_climbing_DAR18B.xlsx", sheet = "Observation")

# Arranging labels
fld_book$`previous generation` <- gsub(pattern = ".", replacement = "_", x = fld_book$`previous generation`, fixed = T)
fld_book$`previous generation` <- gsub(pattern = "x", replacement = "_", x = fld_book$`previous generation`, fixed = T)

fld_book$`Unique Identifier` <- gsub(pattern = ".", replacement = "_", x = fld_book$`Unique Identifier`, fixed = T)

# Moving files
outDir <- "./Documents/Data/Computer_vision/thesis/_img_class"
ifelse(test = !dir.exists(outDir),
       yes = dir.create(outDir, recursive = T),
       no = cat("Output directory already exists.\n"))

classes <- fld_book$`previous generation` %>% unique %>% na.omit %>% as.character

list.files2 <- Vectorize(FUN = list.files, vectorize.args = "path")
file.copy2  <- Vectorize(FUN = file.copy, vectorize.args = c("from", "to"))
for(i in 291:length(classes)){
  newGen       <- fld_book$`Unique Identifier`[fld_book$`previous generation` == classes[i]] %>% na.omit %>% as.character
  if(length(list.files2(path = paste0("./Documents/Data/Computer_vision/thesis/_img_sgmn/", newGen), pattern = "*.jpg$", full.names = T) %>% unlist %>% as.character) > 0){
    
    newGen_paths <- list.files2(path = paste0("./Documents/Data/Computer_vision/thesis/_img_sgmn/", newGen), pattern = "*.jpg$", full.names = T) %>% unlist %>% as.character
    newGen_names <- list.files2(path = paste0("./Documents/Data/Computer_vision/thesis/_img_sgmn/", newGen), pattern = "*.jpg$", full.names = F) %>% unlist %>% as.character
    
    if(!dir.exists(paste0(outDir, "/", classes[i]))){ dir.create(paste0(outDir, "/", classes[i])) }
    file.copy2(from = newGen_paths,
               to   = paste0(outDir, "/", classes[i], "/", newGen_names),
               overwrite = F
    )
    
  } else {
    cat("Previous generation without reference images.\n")
  }
  
}
