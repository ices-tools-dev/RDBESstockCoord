## this is a re-write of fun_make_relation from RDBESstockCoord
## names in getCodeList("SpecWoRMS") and getCodeList("SpecASFIS") seem to have changed - this fixes that
## also obtaining StockListbyArea is inefficient and incomplete - tidier from icesVocab

#' Function to create stock_relation from ICES databases
#'
#' @param year Year from which to extract the ICES stock databases
#'
#' @returns
#' @export
#'
#' @examples
funMakeRelation <- function(year){
  require(icesVocab)
  require(icesSD)
  require(data.table)
  
  # Import codes from ICES ----
  # species codes
  codes_aph <- icesVocab::getCodeList("SpecWoRMS")
  names(codes_aph)[names(codes_aph) == "Key"] <- "speciesCode"
  names(codes_aph)[names(codes_aph) == "Description"] <- "SpeciesName"

  codes_FAO <- icesVocab::getCodeList("SpecASFIS")
  names(codes_FAO)[names(codes_FAO) == "Key"] <- "Species"
  names(codes_FAO)[names(codes_FAO) == "Description"] <- "SpeciesName"

  codes_aph_FAO <- merge(codes_FAO, codes_aph, by = "SpeciesName")
  codes_aph_FAO <- subset(codes_aph_FAO,
                          select=c("Species",
                                   "SpeciesName",
                                   "speciesCode"))

  # stock list by year
  StockListbyEG <- icesSD::getSD(year=year)
  StockListbyEG <- subset(StockListbyEG,
                          select=c("ExpertGroup",
                                   "StockKey",
                                   "StockKeyLabel",
                                   "SpeciesScientificName",
                                   "StockKeyDescription"))

  names(StockListbyEG)[names(StockListbyEG) == "StockKeyLabel"] <- "StockCode"
  names(StockListbyEG)[names(StockListbyEG) == "ExpertGroup"] <- "EG"
  names(StockListbyEG)[names(StockListbyEG) == "SpeciesScientificName"] <- "SpeciesName"

  StockListbyEG <- StockListbyEG[nzchar(StockListbyEG$StockCode), ]

  # stock list by area
  StockListbyArea <- getCodeTypeRelation("ICES_StockCode","ICES_Area")

  names(StockListbyArea)[names(StockListbyArea) == "ICES_StockCode"] <- "StockCode"
  names(StockListbyArea)[names(StockListbyArea) == "ICES_Area"] <- "ICESArea"
  
  # ICES area list
  ICES_Area <- icesVocab::getCodeList("ICES_Area")
  ICES_Area_27 <- subset(ICES_Area, substr(Key, 1, 3) == "27." & Deprecated == F)
  
  # Fix problems with areas in StockListbyArea ----
  ## This should be before adding FMU's
  ## 1. step - stock specific problems ----
  ## This is done per AWG
  ### HAWG ----
  StockListbyArea[nrow(StockListbyArea) + 1, ] <- c("san.sa.2r", "27.3.a")
  StockListbyArea[nrow(StockListbyArea) + 1, ] <- c("san.sa.6", "27.3.c.22")
  
  ### NWWG ----
  StockListbyArea[nrow(StockListbyArea) + 1, ] <- c("cod.21.27.1.14", "27.14")
  
  # Area 27.3.a is not a part of her.27.3a47d when submitting data to ICES.
  # The area is split at the AWG 
  StockListbyArea <- subset(StockListbyArea,
                            !(
                              StockCode == "her.27.3a47d" &
                                ICESArea %in% c("27.3.a", "27.3.a.20", "27.3.a.21")
                            ))
  
  ### WGBFAS ----
  StockListbyArea[StockListbyArea$StockCode == "cod.27.21", "ICESArea"] <- "27.3.a.21"
  # Area 27.3.d.24 is not a part of cod.27.24-32 when submitting data to ICES.
  # The area is split at the AWG
  StockListbyArea <- subset(StockListbyArea,
                            !(StockCode == "cod.27.24-32" & ICESArea == "27.3.d.24"))
  
  ### WGNSSK ----
  ### WGWIDE ----
  
  
  ## 2. step - add overlying | underlying areas in area 27 ----
  # These fixes should be made in the ICES Vocab, not here, but for now...
  ### overlying
  ices_area_codes <- unique(ICES_Area_27$Key)
  
  StockListbyArea_areas_over <- StockListbyArea
  added_areas <- c() # This is just an output for checking additions
  
  repeat {
    
    n_before <- nrow(StockListbyArea_areas_over)
    
    for (i in seq_along(ices_area_codes)) {
      area <- paste0(ices_area_codes[i], "\\.")
      all_areas_under <- grep(paste0("^", area), ices_area_codes, value = TRUE)
      all_areas_under <- all_areas_under[all_areas_under != area]
      
      area_relation <- subset(StockListbyArea_areas_over, ICESArea %in% all_areas_under)
      area_relation <- dplyr::mutate(dplyr::group_by(area_relation, StockCode),
                                     no_area = length(ICESArea))
      area_relation <- subset(area_relation, no_area == length(all_areas_under))
      area_relation$ICESArea <- gsub(pattern = "\\\\.", "", area)
      
      StockListbyArea_areas_over <- 
        rbind(StockListbyArea_areas_over, 
              area_relation[, !names(area_relation) %in% "no_area", drop = FALSE])
      
      added_areas <- 
        rbind(added_areas, 
              area_relation[, !names(area_relation) %in% "no_area", drop = FALSE])
    }
    
    StockListbyArea_areas_over <- 
      unique(StockListbyArea_areas_over[, c("StockCode", "ICESArea")])
    
    n_after <- nrow(StockListbyArea_areas_over)
    
    # Stop once a full pass adds no new rows
    if (n_after == n_before) break
  }

  ### underlying
  StockListbyArea_areas_under <- StockListbyArea
  added_areas <- c() # This is just an output for checking additions
  
  repeat {
    
    n_before <- nrow(StockListbyArea_areas_under)
    
    for (i in seq_along(ices_area_codes)) {
      area <- ices_area_codes[i]
      pattern <- paste0("^", gsub("\\.", "\\\\.", area), "($|\\.)")
      areas_under <- grep(pattern, ices_area_codes, value = TRUE)
      areas_under <- areas_under[areas_under != area]
      
      if (length(areas_under) > 0) {
        area_relation <- subset(StockListbyArea_areas_under, ICESArea %in% area)
        if (nrow(area_relation) > 0) {
          area_relation$ICESArea <- paste(areas_under, collapse = ",")
          
          # base R version of separate_longer_delim
          split_areas <- strsplit(area_relation$ICESArea, ",")
          area_relation <- area_relation[rep(seq_len(nrow(area_relation)), lengths(split_areas)), ]
          area_relation$ICESArea <- unlist(split_areas)
          rownames(area_relation) <- NULL
          
          StockListbyArea_areas_under <- rbind(StockListbyArea_areas_under, area_relation)
          added_areas <- rbind(added_areas, area_relation)
        }
      }
    }
    
    StockListbyArea_areas_under <- 
      unique(StockListbyArea_areas_under[, c("StockCode", "ICESArea")])
    
    n_after <- nrow(StockListbyArea_areas_under)
    
    if (n_after == n_before) break
  }
  
  StockListbyArea_all_areas <- rbind(StockListbyArea_areas_over, StockListbyArea_areas_under)
  
  StockListbyArea_all_areas <- 
    unique(StockListbyArea_all_areas[, c("StockCode", "ICESArea")])

  # StockListbyArea <- rbind(StockListbyArea[!StockListbyArea$StockCode == "pil.27.8c9a",],
  #                          data.frame(StockCode = "pil.27.8c9a",
  #                                     ICESArea = c("27.8.c.e","27.8.c.w","27.9.a.n","27.9.a.s")))
  
  # Code FMU's when relevant ----
  ## This is done per AWG
  StockListbyAreaFMU <- StockListbyArea_all_areas
  StockListbyAreaFMU$FMU <- NA
  ## HAWG
  StockListbyAreaFMU$FMU[substr(StockListbyAreaFMU$StockCode, 1, 6) == "san.sa"] <-
    substr(StockListbyAreaFMU$StockCode, 5, 9)[substr(StockListbyAreaFMU$StockCode, 1, 6) == "san.sa"]
  StockListbyAreaFMU$FMU[StockListbyAreaFMU$StockCode == "her.27.irls" &
                           StockListbyAreaFMU$ICESArea == "27.7.a"] <- "27.7.a.s"
  StockListbyAreaFMU$FMU[StockListbyAreaFMU$StockCode == "her.27.nirs" &
                           StockListbyAreaFMU$ICESArea == "27.7.a"] <- "27.7.a.n"
  StockListbyAreaFMU$FMU[StockListbyAreaFMU$StockCode == "her.27.6aS7bc" &
                           StockListbyAreaFMU$ICESArea == "27.6.a"] <- "27.6.a.s"
  StockListbyAreaFMU$FMU[StockListbyAreaFMU$StockCode == "her.27.6aN" &
                           StockListbyAreaFMU$ICESArea == "27.6.a"] <- "27.6.a.n"
  ## NWWG
  StockListbyAreaFMU$FMU[StockListbyAreaFMU$StockCode == "reb.2127.dp"] <- "2127.dp"
  StockListbyAreaFMU$FMU[StockListbyAreaFMU$StockCode == "reb.2127.sp"] <- "2127.sp"
  
  
  # Combine code lists ----
  stock_relation <- merge(StockListbyEG,
                                StockListbyAreaFMU,
                                by = c("StockCode"), all.x = TRUE)
  
  ## Fix species ----
  ### Some stocks have more then one Species in SpeciesName and therefore do not match names in codes_aph_FAO
  ### It would be nice to replace this tidyr function with something else
  stock_relation <- tidyr::separate_longer_delim(data = stock_relation, cols = SpeciesName, ", ")
  
  ### Remove rows with 'Psetta maxima (historic name)'. Scophthalmus maximus is also present
  stock_relation <- subset(stock_relation, SpeciesName != "Psetta maxima (historic name)")

  stock_relation <- merge(stock_relation,
                          codes_aph_FAO,
                          by = "SpeciesName",
                          all.x = T)
  
  ### Not all species are present in ICES list with FAO codes and aphiaid's, so these are added here
  
  stock_relation$Species[is.na(stock_relation$Species)] <- 
    toupper(substr(stock_relation$StockCode, 1, 3))[is.na(stock_relation$Species)]
  
  stock_relation$speciesCode[stock_relation$SpeciesName == "Alopias"] <- 105740
  stock_relation$speciesCode[stock_relation$SpeciesName == "Ammodytes"] <- 125909
  stock_relation$speciesCode[stock_relation$SpeciesName == "Beryx"] <- 125700
  stock_relation$speciesCode[stock_relation$SpeciesName == "Lepidorhombus"] <- 126122
  stock_relation$speciesCode[stock_relation$SpeciesName == "Platichthys"] <- 126119

  ## Fix other ----
  stock_relation[stock_relation$Species == "PLE" & stock_relation$ICESArea == "27.3.a.20",
                 c("StockCode", "EG", "StockKey", "StockKeyDescription")] <- c("ple.27.420", "WGNSSK", "169189", "Plaice (Pleuronectes platessa) in Subarea 4 (North Sea) and in Subdivision 20 (Skagerrak)")
  

  # ### include area code 27.4 for stocks that have all 27.4 subareas
  # xx <- stock_relation[stock_relation$ICESArea %in% c("27.4.a", "27.4.b", "27.4.c") & stock_relation$StockCode != "mac.27.nea", ]
  # setDT(xx)
  # suppressMessages(suppressWarnings(zz <- dcast(xx, StockCode ~ ICESArea)))
  # 
  # zz$sums <- rowSums(zz[, 2:4])
  # zz <- zz[zz$sums >= 3, ]
  # 
  # xx <- xx[xx$StockCode %in% zz$StockCode, ]
  # xx$ICESArea <- "27.4"
  # xx <- unique(xx)
  # 
  # stock_relation <- rbind(stock_relation, xx)
  ###

  assign("stock_relation", stock_relation, .GlobalEnv)


}
