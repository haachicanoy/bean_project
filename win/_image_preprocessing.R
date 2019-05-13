# Image processing in R: image segmentation
# Harold Achicanoy
# Universidad del Valle, 2019

g <- gc(); rm(list = ls()); options(warn = -1, scipen = 999)

suppressMessages(library(pacman))
suppressMessages(pacman::p_load(raster, imager, corpcor, EBImage, CRImage, tidyverse, FactoMineR))

# List images for processing
# img_path <- "D:/Bean_project/_photos/_colored_seg_images"
img_path <- choose.dir()
out_path <- "D:/Bean_seeds_similarity/_photos/_thesis/segmented"
if(!dir.exists(out_path)){dir.create(out_path, recursive = T)}
list.files2 <- Vectorize(FUN = list.files, vectorize.args = "path")
img_code <- list.dirs(path = img_path, recursive = F, full.names = F)
img_list <- list.files2(path = paste0(img_path, "/", img_code),
                       pattern = "color{1}",
                       full.names = T)
img_blck <- list.files2(path = paste0(img_path, "/", img_code),
                        pattern = "seg{1}",
                        full.names = T)
rm(list.files2)

# --------------------------------------------------------------------------------------------- #
# Image processing own functions
# --------------------------------------------------------------------------------------------- #
# PCA with color spaces
color_trns <- function(img = img_rsz){
  
  img_imgr  <- imager::as.cimg(img) %>% suppressMessages()
  img_HSV   <- imager::RGBtoHSV(im = img_imgr)
  img_Lab   <- imager::RGBtoLab(im = img_imgr)
  img_XYZ   <- imager::RGBtoXYZ(im = img_imgr)
  img_YCbCr <- imager::RGBtoYCbCr(im = img_imgr)
  
  rgb_df <- lapply(X = 1:3, function(i){
    i <<- i
    as.data.frame(as.numeric(img_imgr[,,i]))
  })
  rgb_df <- do.call(cbind, rgb_df); colnames(rgb_df) <- c("R", "G", "B")
  hsv_df <- lapply(X = 1:3, function(i){
    i <<- i
    as.data.frame(as.numeric(img_HSV[,,i]))
  })
  hsv_df <- do.call(cbind, hsv_df); colnames(hsv_df) <- c("H", "S", "V")
  lab_df <- lapply(X = 1:3, function(i){
    i <<- i
    as.data.frame(as.numeric(img_Lab[,,i]))
  })
  lab_df <- do.call(cbind, lab_df); colnames(lab_df) <- c("L", "a", "b")
  xyz_df <- lapply(X = 1:3, function(i){
    i <<- i
    as.data.frame(as.numeric(img_XYZ[,,i]))
  })
  xyz_df <- do.call(cbind, xyz_df); colnames(xyz_df) <- c("X", "Y", "Z")
  ycbcr_df <- lapply(X = 1:3, function(i){
    i <<- i
    as.data.frame(as.numeric(img_YCbCr[,,i]))
  })
  ycbcr_df <- do.call(cbind, ycbcr_df); colnames(ycbcr_df) <- c("Y", "Cb", "Cr")
  color_channels <- cbind(rgb_df, hsv_df, lab_df, xyz_df, ycbcr_df)
  
  pca_svd <- function(x, modos){
    
    n     <- dim(x)[1]
    x0    <- scale(x) # *(sqrt(n)/sqrt(n-1))
    svd_o <- corpcor::fast.svd(x0)
    comp  <- svd_o$u[,1:modos, drop = F] %*% diag(svd_o$d[1:modos], length(svd_o$d[1:modos]), length(svd_o$d[1:modos])) 
    vect  <- svd_o$v[,1:modos]
    output <- list(comp, vect)
    return(output)
    
  }
  fpca <- pca_svd(x = color_channels, modos = 5)
  
  return(fpca)
  
}
img_color_pca <- color_trns(img = img_rsz)
pltt_pca <- function(pca = img_color_pca, img = img_rsz){
  
  rStack <- lapply(1:ncol(pca[[1]]), function(i){
    x <- raster(x = t(matrix(data = pca[[1]][,i], nrow = dim(img)[1], ncol = dim(img)[2], byrow = F)))
  })
  rStack <- raster::stack(rStack)
  return(rStack)
  
}
aux <- pltt_pca(pca = img_color_pca, img = img_rsz)
names(aux) <- paste0("PC_", 1:nlayers(aux))
plot(aux)

# --------------------------------------------------------------------------------------------- #
# Image processing for one image
# --------------------------------------------------------------------------------------------- #
prepImage <- function(img_pth_ifr = img_blck[27],
                      img_pth_rgb = img_list[27],
                      img_nm = img_code[27],
                      desired_dim = 100){
  
  img_rgb <- img_pth_rgb %>% EBImage::readImage() # Load RGB image
  
  # Process over infrared image
  img <- img_pth_ifr %>% EBImage::readImage() # Load infrared image
  img_gray <- img %>% EBImage::channel("gray") # Converting to grayscale image
  img_fltd <- img_gray # Apply image filtering. img_fltd <- img_gray %>% EBImage::medianFilter(size = 2)
  img_thr0 <- img_fltd <= EBImage::otsu(img_fltd) # Global threshold of image
  img_thr1 <- img_fltd %>% EBImage::thresh(w = 10, h = 10, offset = 0.05)
  img_thr1@.Data <- 1 - img_thr1@.Data # Local adaptative threshold
  kernel <- EBImage::makeBrush(size = 5, shape = "gaussian", sigma = 1) # Create a Gaussian kernel
  img_mplg <- img_thr0 %>% EBImage::opening(kern = kernel) # Morphological operators: opening, erode, dilate, closing
  img_wtrs <- EBImage::watershed(EBImage::distmap(img_mplg), 3) # Apply Watershed algorithm 
  # plot(colorLabels(img_wtrs), all = TRUE) # Visualize Watershed results
  fts_shap <- EBImage::computeFeatures.shape(img_wtrs) # Extract shape statistics from segmented regions
  bxplt <- boxplot(fts_shap[,1]) # Identify outliers
  
  # Create individual segmented images
  img_labels <- img_wtrs@.Data %>% as.numeric %>% unique %>% sort
  img_labels <- img_labels[-1]
  if(length(bxplt$out) > 0){
    img_labels <- img_labels[-as.numeric(names(bxplt$out))]
  }
  img_crpd <- lapply(1:length(img_labels), function(i){
    
    wtrs.mat <- img_wtrs@.Data
    wtrs.mat[which(wtrs.mat != img_labels[i])] <- 0
    wtrs.mat[which(wtrs.mat == img_labels[i])] <- 1
    
    wtrs.ind <- EBImage::Image(array(data = wtrs.mat, dim = c(nrow(wtrs.mat), ncol(wtrs.mat), 3)))
    img_ind <- img_rgb * wtrs.ind
    
    ind <- which(img_wtrs@.Data == img_labels[i], arr.in = T)
    ind <- apply(ind, 2L, range)
    rownames(ind) <- c("min", "max")
    
    img_crp <- img_ind[ind["min","row"]:ind["max","row"], ind["min","col"]:ind["max","col"],]
    
    return(img_crp)
    
  })
  img_crpd <- img_crpd[which(unlist(lapply(img_crpd, ncol)) < desired_dim)]
  img_crpd <- img_crpd[which(unlist(lapply(img_crpd, nrow)) < desired_dim)]
  
  if(1 %in% (desired_dim - unlist(lapply(img_crpd, nrow)))){
    mtch <- which((desired_dim - unlist(lapply(img_crpd, nrow))) == 1)
    img_crpd <- img_crpd[-mtch]
  }
  if(1 %in% (desired_dim - unlist(lapply(img_crpd, ncol)))){
    mtch <- which((desired_dim - unlist(lapply(img_crpd, ncol))) == 1)
    img_crpd <- img_crpd[-mtch]
  }
  
  # Fit each individual image to desired dimension
  adjImg <- function(img = img_crpd[[33]], desired_dim = 90){
    # Extract channel information
    r <- img@.Data[,,1]
    g <- img@.Data[,,2]
    b <- img@.Data[,,3]
    # Determine number of columns and rows to add
    rows2add <- (desired_dim - dim(r)[1])/2
    cols2add <- (desired_dim - dim(r)[2])/2
    # Adding columns and rows to R channel
    r <- rbind(matrix(rep(0, floor(rows2add)*dim(r)[2]), nrow = floor(rows2add)),
               r,
               matrix(rep(0, ceiling(rows2add)*dim(r)[2]), nrow = ceiling(rows2add)))
    r <- cbind(matrix(rep(0, floor(cols2add)*dim(r)[1]), ncol = floor(cols2add)),
               r,
               matrix(rep(0, ceiling(cols2add)*dim(r)[1]), ncol = ceiling(cols2add)))
    # Adding columns and rows to G channel
    g <- rbind(matrix(rep(0, floor(rows2add)*dim(g)[2]), nrow = floor(rows2add)),
               g,
               matrix(rep(0, ceiling(rows2add)*dim(g)[2]), nrow = ceiling(rows2add)))
    g <- cbind(matrix(rep(0, floor(cols2add)*dim(g)[1]), ncol = floor(cols2add)),
               g,
               matrix(rep(0, ceiling(cols2add)*dim(g)[1]), ncol = ceiling(cols2add)))
    # Adding columns and rows to B channel
    b <- rbind(matrix(rep(0, floor(rows2add)*dim(b)[2]), nrow = floor(rows2add)),
               b,
               matrix(rep(0, ceiling(rows2add)*dim(b)[2]), nrow = ceiling(rows2add)))
    b <- cbind(matrix(rep(0, floor(cols2add)*dim(b)[1]), ncol = floor(cols2add)),
               b,
               matrix(rep(0, ceiling(cols2add)*dim(b)[1]), ncol = ceiling(cols2add)))
    # Creating array and image
    img_adjtd <- EBImage::rgbImage(red = r, green = g, blue = b)
    return(img_adjtd)
  }
  img_fnal <- img_crpd
  for(i in 1:length(img_crpd)){img_fnal[[i]] <- adjImg(img = img_crpd[[i]], desired_dim = desired_dim); print(i)}
  outDir <- paste0(out_path, "/", img_nm)
  if(!dir.exists(outDir) | (dir.exists(outDir) & length(list.files(outDir)) == 0)){
    dir.create(path = outDir, recursive = T)
    lapply(X = 1:length(img_fnal),
           FUN = function(i){
             if(!file.exists(paste0(outDir, "/", img_nm, "_", i, ".jpg"))){
               EBImage::writeImage(x = img_fnal[[i]],
                                   files = paste0(outDir, "/", img_nm, "_", i, ".jpg"),
                                   type = "jpeg",
                                   quality = 100)
             }
           }
    )
  }
  
  return(cat(paste0("Process done for seed: ", img_nm, "\n")))
}

lapply(X = 1:length(img_code),
       FUN = function(i) prepImage(img_pth_ifr = img_blck[i],
                                   img_pth_rgb = img_list[i],
                                   img_nm = img_code[i],
                                   desired_dim = 100))

# Process done for seed: 18ADB02618_000 [609]

# img_blck_flt <- img_blck[c(3,4,5,7,10,11,12,13,14,15,16,18,19,21,22,24,27,31,32,33,39,41)]
# img_list_flt <- img_list[c(3,4,5,7,10,11,12,13,14,15,16,18,19,21,22,24,27,31,32,33,39,41)]
# img_code_flt <- img_code[c(3,4,5,7,10,11,12,13,14,15,16,18,19,21,22,24,27,31,32,33,39,41)]
# lapply(X = 10:length(img_blck_flt),
#        FUN = function(i) prepImage(img_pth_ifr = img_blck_flt[i],
#                                    img_pth_rgb = img_list_flt[i],
#                                    img_nm = img_code_flt[i],
#                                    desired_dim = 100))
