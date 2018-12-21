# Moving files
# H. Achicanoy
# CIAT, 2018

# ============================================================================== #
# Colored images from CGIAR OneDrive space to local paths
# ============================================================================== #

# Define root directory
root <- choose.dir() # Open path
# root <- "D:/CGIAR/Diaz, Harold (CIAT) - fotos darien"

# Varieties list
vars <- list.dirs(path = root, full.names = F, recursive = F)

# List color images
imgs    <- lapply(1:length(vars), function(i){ list.files(path = paste0(root, "/", vars[i]), pattern = "*_color.jpg$") })
no_imgs <- which(unlist(lapply(imgs, length)) == 0)
imgs    <- unlist(imgs)
vars    <- vars[-no_imgs]; rm(no_imgs)

# Vectorizing file.copy function
file.copy2 <- Vectorize(file.copy, vectorize.args = c("from", "to"))

# Moving files from root to selected directory
outDir <- choose.dir()
file.copy2(from = paste0(root, "/", vars, "/", imgs), to = paste0(outDir, "/", imgs))

# ============================================================================== #
# Colored and segmentation images from CGIAR OneDrive space to local paths
# ============================================================================== #

# Define root directory
root <- choose.dir() # Open path
# root <- "D:/CGIAR/Diaz, Harold (CIAT) - fotos darien"

# Varieties list
vars <- list.dirs(path = root, full.names = F, recursive = F)

# List color images
imgs    <- lapply(1:length(vars), function(i){ list.files(path = paste0(root, "/", vars[i]), pattern = "*_color.jpg$") })
imgs_sg <- lapply(1:length(vars), function(i){ list.files(path = paste0(root, "/", vars[i]), pattern = "*_seg.jpg$") })
no_imgs <- which(unlist(lapply(imgs, length)) == 0)
imgs    <- unlist(imgs)
imgs_sg <- unlist(imgs_sg)
vars    <- vars[-no_imgs]; rm(no_imgs)

# Vectorizing file.copy function
file.copy2  <- Vectorize(file.copy, vectorize.args = c("from", "to"))
dir.exists2 <- Vectorize(dir.exists, vectorize.args = c("paths"))
dir.create2 <- Vectorize(dir.create, vectorize.args = c("path"))

# Moving files from root to selected directory
outDir <- choose.dir()
file.copy2(from = paste0(root, "/", vars, "/", imgs), to = if(!dir.exists2(paste0(outDir, "/", vars))){dir.create2(paste0(outDir, "/", vars), recursive = T); paste0(outDir, "/", vars, "/", imgs)} else {paste0(outDir, "/", vars, "/", imgs)})
file.copy2(from = paste0(root, "/", vars, "/", imgs_sg), to = paste0(outDir, "/", vars, "/", imgs_sg))

# ============================================================================== #
# Order colored photos by principal seed color
# ============================================================================== #

# Load Field Book file
input <- choose.files()
bn_tbl <- readxl::read_excel(input, sheet = "Observation")

colors <- sort(na.omit(unique(bn_tbl$PSC)))
inDir  <- choose.dir(caption = "Select colored images folder")
outDir <- choose.dir(caption = "Choose colored images folder to be ordered")

lapply(1:length(colors), function(i){
  
  if(!dir.exists(paths = paste0(outDir, "/", colors[i]))){dir.create(paste0(outDir, "/", colors[i]))}
  mtch_by_color <- as.character(na.omit(gsub(pattern = ".000",
                                             replacement = "",
                                             x = bn_tbl$`Unique Identifier`, fixed = T)[bn_tbl$PSC == colors[i]]))
  mtch_by_color <- paste0(mtch_by_color, "_000_color.jpg")
  all_files     <- list.files(path = inDir)
  mtch_by_files <- mtch_by_color[mtch_by_color %in% all_files]
  file.copy2(from = paste0(inDir, "/", mtch_by_files),
             to = paste0(outDir, "/", colors[i], "/", mtch_by_files))
  
})
