

funCompareIntercatchCEF <- function(IC_file_path = dat_path,
                                    CEF_file_path = "./CEF") {

  ##intercatch
  funReadIntercatch(dat_path = dat_path)

  #account for double SI record is age and length are have seperate files
  si$key = paste(si$Country,
                 si$Year,
                 si$Season,
                 si$Species,
                 si$FishingArea,
                 si$Fleet,
                 si$CatchCategory,
                 sep = "_")

  si <- si[!duplicated(si$key), ]

  #make domain for esier comparison
  sd$domainBiology = paste("IC",
                           sd$Season,
                           sd$FishingArea,
                           sd$Fleet,
                           sep = "_")


  ## CEF
  catches <- read.csv(paste0(CEF_file_path, "/catches_CEF_v17.1.csv"))
  distributions <- read.csv(paste0(CEF_file_path, "/distributions_CEF_v17.1.csv"))

  #only WG estimates are compared
  catches <- catches[catches$originType == "WGEstimate", ]


  ######### check stuff ###################
  print("----------------------")
  print(paste("Intercatch-total == CEF-total are", sum(si$Caton) == sum(catches$total)))
  print("----------------------")

  ## domain biology check
  catch <- data.table(catches[! is.na(catches$domainBiology), ])
  catch <- catch[ ,. (total = sum(total, na.rm = T)),
                  by = .(stock, speciesCode, catchCategory, seasonValue,
                         fleetValue, domainBiology)]

  distribution <- merge(catch, distributions,
                        by = c("stock", "speciesCode", "catchCategory", "domainBiology"),
                        all.y = T)
  ####

  stocks <- unique(catches$stock)
  fun <- function(i) {
    stck <- stocks[i]

    print("----------------------")
    print(stck)

    rel <- stock_relation[stock_relation$StockCode == stck, ]

    cat <- catches[catches$stock == stck, ]
    dst <- distribution[distribution$stock == stck, ]

    six <- si[si$FishingArea %in% rel$ICESArea & si$Species %in% rel$Species, ]
    sdx <- sd[sd$FishingArea %in% rel$ICESArea & sd$Species %in% rel$Species, ]
    names(six)[names(six) == "CatchCategory"] <- "catchCategory"
    names(sdx)[names(sdx) == "CatchCategory"] <- "catchCategory"

    # total landings
    setDT(six)
    sixTot <- six[ ,. (weightIC = sum(Caton)),
                   by = .(catchCategory)]

    setDT(cat)
    catTot <- cat[ ,. (weightCEF = sum(total)),
                   by = .(catchCategory)]

    tots <- merge(catTot, sixTot)
    tots$species <- rel$Species[1]
    tots$stock <- stck

    tots <- tots[, c("species", "stock", "catchCategory", "weightCEF", "weightIC")]

    tots$has_lengths <- "N"
    tots$has_ages = "N"
    tots$IC_CEF_Lengths = ""
    tots$max_pct_SOP_Lengths = ""
    tots$IC_CEF_Ages = ""
    tots$max_pct_SOP_Ages = ""

    ## length distribution SOP check
    ld <- data.table(sdx[sdx$CANUMtype == "lngt", ])
    wl <- data.table(dst[dst$distributionType == "LengthTotal", ])

    if (nrow(ld) > 0 & nrow(wl) > 0){
      wl <- dcast(wl,
                  catchCategory + distributionClass + domainBiology + total ~ variableType,
                  value.var = "value")


      # sop length
      wlSop <- wl[ ,. (sopCEF = sum(Number/1000 * WeightLive),
                       catonCEF = unique(total)),
                   by = .(catchCategory, domainBiology)]

      #here convet to number and round, to acound for CEF formatting procefdure, when comparing values
      ldSop <- ld[ ,. (sopIC = sum(round(NumberCaught*1000, 3)/1000 * MeanWeight),
                       catonIC = as.numeric(unique(SampledCatch))),
                   by = .(catchCategory, domainBiology)]

      totld <- merge(wlSop, ldSop, all = T)
      totld$sopIC[is.na(totld$sopIC)] <- 0
      totld$sopCEF[is.na(totld$sopCEF)] <- 0

      if (all(abs(totld$sopCEF - totld$sopIC) < 0.5)){
        print("Length samples: All numbers, weight and SOP are equal between files")
        tots$IC_CEF_Lengths = "Equal"
      } else{
        print("Length samples: Some SOP differes between files")
        tots$IC_CEF_Lengths = "Not Equal"
      }


      tots$has_lengths <- "Y"
      wlSop$pct <- abs((1-wlSop$sopCEF/wlSop$catonCEF)*100)
      wlSop$pct[is.infinite(wlSop$pct)] <- 0
      tots$max_pct_SOP_Lengths <- round(max(wlSop$pct), 1)

    }

    ## Age distribution SOP check
    ad <- data.table(sdx[sdx$CANUMtype == "age", ])
    wa <- data.table(dst[dst$distributionType == "Age", ])

    if (nrow(ad) > 0 & nrow(wa) > 0){
      wa <- dcast(wa,
                  catchCategory + distributionClass + domainBiology + total ~ variableType,
                  value.var = "value")
      # sop Age
      waSop <- wa[ ,. (sopCEF = sum(Number/1000 * WeightLive),
                       catonCEF = unique(total)),
                   by = .(catchCategory, domainBiology)]

      adSop <- ad[ ,. (sopIC = sum(round(NumberCaught*1000, 3)/1000 * MeanWeight),
                       catonIC = as.numeric(unique(SampledCatch))),
                   by = .(catchCategory, domainBiology)]

      totad <- merge(waSop, adSop, all = T)
      totad$sopIC[is.na(totld$sopIC)] <- 0
      totad$sopCEF[is.na(totld$sopCEF)] <- 0

      if (all(abs(totad$sopCEF - totad$sopIC) < 0.9)){
        print("Age samples: All numbers, weight and SOP are equal between files")
        tots$IC_CEF_Ages = "Equal"
      } else{
        print("Age samples: Some SOP differes between files")
        tots$IC_CEF_Ages = "Not Equal"
      }

      tots$has_ages <- "Y"
      waSop$pct <- abs((1-waSop$sopCEF/waSop$catonCEF)*100)
      waSop$pct[is.infinite(waSop$pct)] <- 0
      tots$max_pct_SOP_Ages <- round(max(waSop$pct), 1)

    }
    tots
  }
  out <- lapply(1:length(stocks), fun)
  out <- do.call("rbind", out)

  assign("IC_CEF_comparison", out, .GlobalEnv)
}
