#### Getting started analyzing Biodiversity and Tenure in Brazil ####

# this script should be run before the others to load packages, etc.
# there should be no outputs, but should only load objects and functions
# packages should be loaded in each script
# author: Andrea Pacheco
# first run: 25.09.2023

# libraries
# Move on from any deprecated rgdal and raster parts of my workflow to sf and terra

# install.packages("terra")
# # install.packages("rgdal")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("RPostgreSQL")
# install.packages("exactextractr")
# install.packages("cowplot")
# install.packages("blogdown")
# install.packages("tidyterra")
# install.packages("rspatial/geodata")
# install.packages("geobr")
# install.packages("geos")
# install.packages("tidyverse")
# install.packages("fasterize")
# install.packages("classInt")
# install.packages("biscale")
# install.packages("pals")
# install.packages("hrbrthemes")
# install.packages("GGally")
wdmain <- "N:/eslu/priv/pacheco/whoOwnsBRBD/"

# function for reading in only specific columns
read_my_shp = function(f){
  s = st_read(f)
  return(s[,c("X_uid_","tipo","uf", "geometry")]) # choose specific columns to keep lighter versions of these data: an identifier, the state, and the geometry
}
# establish my projection: South America Albers Equal Area 
my_crs_SAaea <- "+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

# 1. make 1 km2 mask to base rest of rasters on
setwd(paste0(wdmain,"/data/raw/Biodiversity_v20231009")) # use my biodiversity data as a basis
r <- rast(list.files()[grep("Richness", list.files())[1]])
r <- project(r, my_crs_SAaea)
mask <- r*0

# LEARNING TO USE TERRA INSTEAD OF RASTER ----
# x <- rast()
# plot(x)
# x <- rast(ncol=36, nrow=18, xmin=-1000, xmax=1000,ymin=-100, ymax=900)
# res(x) <- 100
# crs(x) <- "+proj=utm +zone=48 +datum=WGS84"
# 
# # values function: useful for counting values in the raster (instead of freq)
# r <- rast(ncol=10, nrow=10)
# ncell(r)
# hasValues(r)
# values(r) <- 1:ncell(r)
# set.seed(0)
# values(r) <- runif(ncell(r))
# hasValues(r)
# sources(r) #check if in memory layers actually have cell values
# values(r)[10:20]
# plot(r, main = "raster with 100 cells")
# # other functions
# dim(r)
# xmax(r)
# 
# # get the name of an example file installed with the package
# # do not use this construction of your own files
# filename <- system.file("ex/meuse.tif", package="terra")
# filename
# r <- rast(filename)
# plot(r)
# 
# filename <- system.file("ex/logo.tif", package="terra")
# filename
# b <- rast(filename)
# b
# b[[2]] #layers on the raster still work with [[]]


# important/useful functions!
# crop = normal crop
# trim = removes OUTER cells with NAs, extend = the opposite
# merge = merges 2 or more rasters into one object. must have same res and origin.
# aggregate and disagg are the same as before
# resample (remember to not use this for creating a larger resolution - use aggregate for this.)

# mask function: punches holes from the mask to the desired raster
# r <- rast(ncols=10, nrows=10)
# m <- rast(ncols=10, nrows=10)
# values(r) <- 1:100
# set.seed(1965)
# x <- round(3 * runif(ncell(r)))
# x[x==0] <- NA
# values(m) <- x
# mr <- mask(r,m) # so, this shows the mask function punches holes from the mask to the desired raster 

# rasterize Polygons
# f <- system.file("ex/lux.shp", package="terra")
# v <- vect(f)
# r <- rast(v, ncols=75, nrows=100)
# z <- rasterize(v, r, "NAME_2")
# plot(z)

# SMALL TEST intersection of UCs and IND to figure out what needs to be done ----

# # Land tenure data folders ----
# setwd(paste0(wdmain,"/data/processed/"))
# 
# # A) Read and standardize PAs/UCs and indigenous lands: ----
# 
# uc <- st_read("landTenure_UC/landTenure_UCs_MMA_20231212_SAalbers.shp", stringsAsFactors = F, options = "ENCODING=latin1")
# # fix empty geometries
# uc <- st_transform(uc, my_crs_SAaea) # fix projection
# uc$subcateg <- uc$LTcateg # add/clear up this column
# uc$LTcateg <- uc$group
# uc$id <- paste0("UC-", 1:nrow(uc))
# uc <- select(uc, c("LTcateg", "id", "geometry"))
# 
# 
# ind <- st_read("landTenure_IND/landTenure_indigenous_20231212_SAalbers.shp")
# ind <- st_transform(ind, my_crs_SAaea)
# ind$LTcateg <- "indigenous"
# ind$id <- paste0("IN-", 1:nrow(ind))
# ind <- select(ind, c("LTcateg","id", "geometry"))


# test_uc <- uc[136,] #136, 246
# test_ind <- ind[484,]
# test <- rbind(test_ind, test_uc)
# plot(test["LTcateg"])
# 
# uc.ind_intersects <- st_intersects(test_uc, test_ind) # one intersection = intersects with one other feature (not every single line)
# uc.ind_intersection <- st_intersection(test_uc, test_ind) # returns geometry of the shared portion of x and y
# uc.ind_intersection
# plot(uc.ind_intersection$geometry) # can clearly see that it is indeed ONLY the areas that overlap
# # difference in behavior when using one or two objects in the function:
# bound_intersection <- st_intersection(test) 
# # the bound_intersection has three observations instead of one. 
# # it KEEPS the original features shapes
# plot(bound_intersection$geometry)
# plot(bound_intersection["n.overlaps"]) # we get this column
# plot(bound_intersection[1,]$geometry) # the first observation is all the indigenous with NO overlaps
# plot(bound_intersection[2,]$geometry) # the second, is the one with overlap
# plot(bound_intersection[3,]$geometry) # the third, is the part of UC with no overlaps
# 
# # TEST difference bt UCs and IND
# uc.ind_diff <- st_difference(test_uc, test_ind)
# uc.ind_diff # returns one feature - which is essentially feature 3 in the above example. the parts where they don't overlap
#  (the parts of the x that don't overlap with y)


# trick from eduardo?
# acc_sf <- st_transform(acc_sf, crs = 3857)

# classIntervals function ----
# function (var, n, style = "quantile", rtimes = 3, ..., 
#           intervalClosure = c("left", "right"), dataPrecision = NULL, 
#           warnSmallN = TRUE, warnLargeN = TRUE, largeN = 3000L, samp_prop = 0.1, 
#           gr = c("[", "]")) 
# {
#   if (is.factor(var)) 
#     stop("var is categorical")
#   TZ <- NULL
#   POSIX <- FALSE
#   DATE <- FALSE
#   if (!is.numeric(var)) {
#     if (inherits(var, "POSIXt")) {
#       TZ <- attr(var, "tzone")
#       POSIX <- TRUE
#       var <- unclass(as.POSIXct(var))
#     }
#     else if (inherits(var, "Date")) {
#       var <- unclass(var)
#       DATE <- TRUE
#     }
#     else {
#       stop("var is not numeric")
#     }
#   }
#   UNITS <- NULL
#   if (inherits(var, "units")) {
#     UNITS <- paste0(gr[1], as.character(attr(var, "units")), 
#                     gr[2])
#   }
#   intervalClosure <- match.arg(intervalClosure)
#   ovar <- var
#   if (length(style) > 1L) 
#     style <- style[1L]
#   if (any(is.na(var))) {
#     warning("var has missing values, omitted in finding classes")
#     var <- c(na.omit(var))
#   }
#   if (any(!is.finite(var))) {
#     warning("var has infinite values, omitted in finding classes")
#     is.na(var) <- !is.finite(var)
#   }
#   nobs <- length(unique(var))
#   if (nobs == 1) 
#     stop("single unique value")
#   needn <- !(style %in% c("dpih", "headtails", 
#                           "box"))
#   if (missing(n)) 
#     n <- nclass.Sturges(var)
#   if (n < 2 & needn) 
#     stop("n less than 2")
#   n <- as.integer(n)
#   pars <- NULL
#   if (n > nobs & needn) {
#     if (warnSmallN) {
#       warning(paste("n greater than number of different finite values", 
#                     "n reset to number of different finite values", 
#                     sep = "\\n"))
#     }
#     n <- nobs
#   }
#   if (n == nobs & needn) {
#     if (warnSmallN) {
#       warning(paste("n same as number of different finite values", 
#                     "each different finite value is a separate class", 
#                     sep = "\\n"))
#     }
#     sVar <- sort(unique(var))
#     dsVar <- diff(sVar)
#     brks <- c(sVar[1] - (mean(dsVar)/2), sVar[1:(length(sVar) - 
#                                                    1)] + (dsVar/2), sVar[length(sVar)] + (mean(dsVar)/2))
#     style = "unique"
#   }
#   else {
#     sampling <- FALSE
#     if (warnLargeN && (style %in% c("fisher", "jenks"))) {
#       if (nobs > largeN) {
#         warning("N is large, and some styles will run very slowly; sampling imposed")
#         sampling <- TRUE
#         nsamp <- as.integer(ceiling(samp_prop * nobs))
#         if (nsamp > largeN) 
#           nsamp <- as.integer(largeN)
#       }
#     }
#     if (style == "fixed") {
#       dots <- list(...)
#       fixedBreaks <- sort(dots$fixedBreaks)
#       if (is.null(fixedBreaks)) 
#         stop("fixed method requires fixedBreaks argument")
#       if (!is.numeric(fixedBreaks)) {
#         if (inherits(fixedBreaks, "POSIXt") && 
#             POSIX) {
#           fixedBreaks <- unclass(as.POSIXct(fixedBreaks))
#         }
#         else if (inherits(fixedBreaks, "DATE") && 
#                  DATE) {
#           fixedBreaks <- unclass(fixedBreaks)
#         }
#         else {
#           stop("fixedBreaks must be numeric")
#         }
#       }
#       if (any(diff(fixedBreaks) < 0)) 
#         stop("decreasing fixedBreaks found")
#       if (min(var) < fixedBreaks[1] || max(var) > fixedBreaks[length(fixedBreaks)]) 
#         warning("variable range greater than fixedBreaks")
#       brks <- fixedBreaks
#     }
#     else if (style == "sd") {
#       svar <- scale(var)
#       pars <- c(attr(svar, "scaled:center"), attr(svar, 
#                                                   "scaled:scale"))
#       names(pars) <- c("center", "scale")
#       dots <- list(...)
#       if (is.null(dots$sd_m)) {
#         sbrks <- pretty(x = svar, n = n, ...)
#       }
#       else {
#         sbrks <- dots$sd_m
#         stopifnot(is.numeric(sbrks))
#       }
#       brks <- c((sbrks * pars[2]) + pars[1])
#       if (!is.finite(brks[1])) 
#         brks[1] <- min(var)
#       if (!is.finite(brks[length(brks)])) 
#         brks[length(brks)] <- max(var)
#       if (any(order(brks) != 1:length(brks))) 
#         brks <- unique(sort(brks))
#     }
#     else if (style == "equal") {
#       brks <- seq(min(var), max(var), length.out = (n + 
#                                                       1))
#     }
#     else if (style == "pretty") {
#       brks <- c(pretty(x = var, n = n, ...))
#     }
#     else if (style == "quantile") {
#       brks <- c(quantile(x = var, probs = seq(0, 1, 1/n), 
#                          ...))
#       names(brks) <- NULL
#     }
#     else if (style == "kmeans") {
#       pars <- try(kmeans(x = var, centers = n, ...))
#       if (inherits(pars, "try-error")) {
#         warning("jittering in kmeans")
#         jvar <- jitter(rep(x = var, times = rtimes))
#         pars <- try(kmeans(x = jvar, centers = n, ...))
#         if (inherits(pars, "try-error")) 
#           stop("kmeans failed after jittering")
#         else {
#           cols <- match(pars$cluster, order(c(pars$centers)))
#           rbrks <- unlist(tapply(jvar, factor(cols), 
#                                  range))
#         }
#       }
#       else {
#         cols <- match(pars$cluster, order(c(pars$centers)))
#         rbrks <- unlist(tapply(var, factor(cols), range))
#       }
#       names(rbrks) <- NULL
#       brks <- .rbrks(rbrks)
#     }
#     else if (style == "hclust") {
#       pars <- hclust(dist(x = var, method = "euclidean"), 
#                      ...)
#       rcluster <- cutree(tree = pars, k = n)
#       rcenters <- unlist(tapply(var, factor(rcluster), 
#                                 mean))
#       cols <- match(rcluster, order(c(rcenters)))
#       rbrks <- unlist(tapply(var, factor(cols), range))
#       names(rbrks) <- NULL
#       brks <- .rbrks(rbrks)
#     }
#     else if (style == "bclust") {
#       pars <- try(bclust(x = var, centers = n, ...))
#       if (inherits(pars, "try-error")) {
#         warning("jittering in bclust")
#         jvar <- jitter(rep(x = var, times = rtimes))
#         pars <- try(bclust(x = jvar, centers = n, ...))
#         if (inherits(pars, "try-error")) 
#           stop("bclust failed after jittering")
#         else {
#           cols <- match(pars$cluster, order(c(pars$centers)))
#           rbrks <- unlist(tapply(jvar, factor(cols), 
#                                  range))
#         }
#       }
#       else {
#         cols <- match(pars$cluster, order(c(pars$centers)))
#         rbrks <- unlist(tapply(var, factor(cols), range))
#       }
#       names(rbrks) <- NULL
#       brks <- .rbrks(rbrks)
#     }
#     else if (style == "fisher") {
#       if (sampling) {
#         pars <- fish(x = c(range(var), sample(x = var, 
#                                               size = nsamp)), k = n)
#       }
#       else {
#         pars <- fish(x = var, k = n)
#       }
#       brks <- pars[n, 1]
#       for (i in n:1) brks <- c(brks, (pars[i, 2] + pars[(i - 
#                                                            1), 1])/2)
#       brks <- c(brks, pars[1, 2])
#       colnames(pars) <- c("min", "max", "class mean", 
#                           "class sd")
#     }
#     else if (style == "jenks") {
#       intervalClosure = "right"
#       if (storage.mode(var) != "double") 
#         storage.mode(var) <- "double"
#       if (sampling) {
#         message("Use \"fisher\" instead of \"jenks\" for larger data sets")
#         d <- sort(c(range(var), sample(x = var, size = nsamp)))
#       }
#       else {
#         d <- sort(var)
#       }
#       k <- n
#       mat1 <- matrix(1, length(d), k)
#       mat2 <- matrix(0, length(d), k)
#       mat2[2:length(d), 1:k] <- .Machine$double.xmax
#       v <- 0
#       for (l in 2:length(d)) {
#         s1 = s2 = w = 0
#         for (m in 1:l) {
#           i3 <- l - m + 1
#           val <- d[i3]
#           s2 <- s2 + val * val
#           s1 <- s1 + val
#           w <- w + 1
#           v <- s2 - (s1 * s1)/w
#           i4 <- trunc(i3 - 1)
#           if (i4 != 0) {
#             for (j in 2:k) {
#               if (mat2[l, j] >= (v + mat2[i4, j - 1])) {
#                 mat1[l, j] <- i3
#                 mat2[l, j] <- v + mat2[i4, j - 1]
#               }
#             }
#           }
#         }
#         mat1[l, 1] <- 1
#         mat2[l, 1] <- v
#       }
#       kclass <- 1:k
#       kclass[k] <- length(d)
#       k <- length(d)
#       last <- length(d)
#       for (j in length(kclass):1) {
#         id <- trunc(mat1[k, j]) - 1
#         kclass[j - 1] <- id
#         k <- id
#         last <- k - 1
#       }
#       brks <- d[c(1, kclass)]
#     }
#     else if (style == "dpih") {
#       h <- dpih(var, ...)
#       dots <- list(...)
#       if (!is.null(dots$range.x)) {
#         vmin <- dots$range.x[1]
#         vmax <- dots$range.x[2]
#       }
#       else {
#         vmin <- min(var)
#         vmax <- max(var)
#       }
#       brks <- seq(vmin, vmax, by = h)
#     }
#     else if (style == "headtails") {
#       dots <- list(...)
#       thr <- ifelse(is.null(dots$thr), 0.4, dots$thr)
#       thr <- min(1, max(0, thr))
#       head <- var
#       breaks <- min(var, na.rm = TRUE)
#       for (i in 1:100) {
#         mu <- mean(head, na.rm = TRUE)
#         breaks <- c(breaks, mu)
#         ntot <- length(head)
#         head <- head[head > mu]
#         prop <- length(head)/ntot
#         keepiter <- prop <= thr & length(head) > 1
#         if (isFALSE(keepiter)) {
#           break
#         }
#       }
#       brks <- sort(unique(c(breaks, max(var, na.rm = TRUE))))
#     }
#     else if (style == "maximum") {
#       x_sort <- sort(var)
#       diffs <- diff(x_sort)
#       n_breaks <- sort(diffs, decreasing = TRUE)[1:(n - 
#                                                       1)]
#       int_end_index <- which(diffs %in% n_breaks)
#       int_nb_index <- which(diffs %in% n_breaks) + 1
#       m <- matrix(c(x_sort[int_nb_index], x_sort[int_end_index]), 
#                   ncol = 2)
#       brks <- c(min(x_sort), rowSums(m)/2, max(x_sort))
#     }
#     else if (style == "box") {
#       dots <- list(...)
#       iqr_mult <- ifelse(is.null(dots$iqr_mult), 1.5, dots$iqr_mult)
#       stopifnot(iqr_mult >= 0)
#       qtype <- ifelse(is.null(dots$type), 7, dots$type)
#       legacy <- ifelse(is.null(dots$legacy), FALSE, dots$legacy)
#       qv <- unname(quantile(var, type = qtype))
#       iqr <- iqr_mult * (qv[4] - qv[2])
#       upfence <- qv[4] + iqr
#       lofence <- qv[2] - iqr
#       bb <- vector(mode = "numeric", length = 7)
#       if (lofence < qv[1]) {
#         if (legacy) {
#           bb[1] <- lofence
#           bb[2] <- floor(qv[1])
#         }
#         else {
#           bb[1] <- -Inf
#           bb[2] <- lofence
#         }
#       }
#       else {
#         bb[2] <- lofence
#         bb[1] <- qv[1]
#       }
#       if (upfence > qv[5]) {
#         if (legacy) {
#           bb[7] <- upfence
#           bb[6] <- ceiling(qv[5])
#         }
#         else {
#           bb[7] <- +Inf
#           bb[6] <- upfence
#         }
#       }
#       else {
#         bb[6] <- upfence
#         bb[7] <- qv[5]
#       }
#       bb[3:5] <- qv[2:4]
#       brks <- bb
#     }
#     else stop(paste(style, "unknown"))
#   }
#   if (is.null(brks)) 
#     stop("Null breaks")
#   if (POSIX) {
#     ovar <- .POSIXct(ovar, TZ)
#     brks <- .POSIXct(brks, TZ)
#   }
#   else if (DATE) {
#     ovar <- as.Date(ovar, origin = "1970-01-01")
#     brks <- as.Date(brks, origin = "1970-01-01")
#   }
#   res <- list(var = ovar, brks = brks)
#   attr(res, "style") <- style
#   attr(res, "parameters") <- pars
#   attr(res, "nobs") <- nobs
#   attr(res, "call") <- match.call()
#   attr(res, "intervalClosure") <- intervalClosure
#   attr(res, "dataPrecision") <- dataPrecision
#   attr(res, "var_units") <- UNITS
#   class(res) <- "classIntervals"
#   res
# }
