#-*- coding: utf-8 -*-

### File: output.R
### Time-stamp: <2023-10-06 13:00:06 a23579>
###
### Created: 03/10/2023	08:23:29
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################
if (!exists("dataYear"))
    source("./0_settings.R")

library(icesTAF)
library(dplyr)
library(tidyr)
library(readr)


resDir <- "./output"
mkdir(resDir)
resDir <- normalizePath(resDir)

## ###########################################################################
## Output caton (with possible BMS allocations)
##   in a text file common to all catch categories:
sink(file = file.path(resDir, "caton_summary.txt"))

cat("## Catch in tons:\n\n")

catonS1 <- tapply(catonR.tot$Catch..kg,
                  as.list(catonR.tot[ , c("Year", "Catch.Cat."), drop = FALSE]),
                  sum, na.rm = TRUE) / 1000
catonS1 <- cbind(catonS1,
                 C = apply(catonS1, 1, sum, na.rm = TRUE))

print(catonS1)

cat("\n## Split raised/imported:\n\n")

catonS2 <- tapply(catonR.tot$Catch..kg,
                  as.list(catonR.tot[ , c("Discards.Imported.Or.Raised", "Catch.Cat.", "Year")] %>%
                          rename(" " = 1, "  " = 2)),
                  sum, na.rm = TRUE) / 1000
catonS2 <- abind(catonS2,
                 C = apply(catonS2, c(1, 3), sum, na.rm = TRUE),
                 along = 2)

print(catonS2)

sink(file = NULL)

## Stock overview with raised discards (for feeding Youen's QC scripts):
write.csv(catonR.tot, file = file.path(resDir, "StockOverview_raised.txt"),
          row.names = FALSE)


## Save canum and weca tables:
catchCat.NatAge <- catchCat.long.plus %>%
    ungroup() %>%
    select(-meanW, -sex) %>%
    pivot_wider(names_from = "age", names_prefix = "age_",
                values_from = "N")

catchCat.NatAge %>%
    write_csv(file = file.path(resDir, "canum_table.csv"),
              na = "")


catchCat.WatAge <- catchCat.long.plus %>%
    ungroup() %>%
    select(-N, -sex) %>%
    pivot_wider(names_from = "age", names_prefix = "age_",
                values_from = "meanW")

catchCat.WatAge %>%
    write_csv(file = file.path(resDir, "weca_table.csv"),
              na = "")

## Save intermediate results:
icesTAF::write.taf(catchCat.long.plus, dir = resDir)
icesTAF::write.taf(catonS1, dir = resDir)
icesTAF::write.taf(catonS2, dir = resDir)
icesTAF::write.taf(catonR.tot, dir = resDir, quote = TRUE)

## Format data for SA input (if BMS allocation is done for NS saithe):
if (isTRUE(getOption("allocateBMS")))
{
    ## ###########################################################################
    ## Output caton (with possible BMS allocations)
    ##   in a text file common to all catch categories:
    sink(file = file.path(resDir, "caton_summary_BMS_alloc.txt"))

    cat("## Catch in tons:\n\n")

    catonS1B <- tapply(catonR.totB$Catch..kg,
                       as.list(catonR.totB[ , c("Year", "Catch.Cat."), drop = FALSE]),
                       sum, na.rm = TRUE) / 1000
    catonS1B <- cbind(catonS1B,
                      C = apply(catonS1B, 1, sum, na.rm = TRUE))

    print(catonS1B)

    cat("\n## Split raised/imported:\n\n")

    catonS2B <- tapply(catonR.totB$Catch..kg,
                       as.list(catonR.totB[ , c("Discards.Imported.Or.Raised", "Catch.Cat.", "Year")] %>%
                               rename(" " = 1, "  " = 2)),
                       sum, na.rm = TRUE) / 1000
    catonS2B <- abind(catonS2B,
                      C = apply(catonS2B, c(1, 3), sum, na.rm = TRUE),
                      along = 2)

    print(catonS2B)

    sink(file = NULL)

    ## Stock overview with raised discards (for feeding Youen's QC scripts):
    write.csv(catonR.totB, file = file.path(resDir, "StockOverview_raised_BMS_alloc.txt"),
              row.names = FALSE)


    ## Save canum and weca tables:
    catchCat.NatAgeB <- catchCat.long.plusB %>%
        ungroup() %>%
        select(-meanW, -sex) %>%
        pivot_wider(names_from = "age", names_prefix = "age_",
                    values_from = "N")

    catchCat.NatAgeB %>%
        write_csv(file = file.path(resDir, "canum_tablee_BMS_alloc.csv"),
                  na = "")


    catchCat.WatAgeB <- catchCat.long.plusB %>%
        ungroup() %>%
        select(-N, -sex) %>%
        pivot_wider(names_from = "age", names_prefix = "age_",
                    values_from = "meanW")

    catchCat.WatAgeB %>%
        write_csv(file = file.path(resDir, "weca_table_BMS_alloc.csv"),
                  na = "")

    catchCat.long.plus_BMS_alloc <- catchCat.long.plusB
    catonS1_BMS_alloc <- catonS1B
    catonS2_BMS_alloc <- catonS2B
    catonR.tot_BMS_alloc <- catonR.totB

    ## Save intermediate results:
    icesTAF::write.taf(catchCat.long.plus_BMS_alloc, dir = resDir)
    icesTAF::write.taf(catonS1_BMS_alloc, dir = resDir)
    icesTAF::write.taf(catonS2_BMS_alloc, dir = resDir)
    icesTAF::write.taf(catonR.tot_BMS_alloc, dir = resDir, quote = TRUE)

    ## ##################################################
    ## SA formated inputs:
    mkdir(file.path(resDir, "Formated_SA_inputs"))

    selCol <- paste0("age_",
                     paste0(minAge:plusGroup,
                            c(rep("", plusGroup - minAge), "+")))

    cn <- catchCat.NatAgeB %>%
        filter(Catch.Cat. == "C") %>%
        select(all_of(selCol)) %>%
        as.numeric() * 1e-3

    ln <- catchCat.NatAgeB %>%
        filter(Catch.Cat. == "L") %>%
        select(all_of(selCol)) %>%
        as.numeric() * 1e-3

    lf <- ln / cn

    cw <- catchCat.WatAgeB %>%
        filter(Catch.Cat. == "C") %>%
        select(all_of(selCol)) %>%
        as.numeric() * 1e-3

    lw <- catchCat.WatAgeB %>%
        filter(Catch.Cat. == "L") %>%
        select(all_of(selCol)) %>%
        {as.numeric(.) * 1e-3} %>%
        ## Check method for filling gaps:
        {ifelse(is.na(.), cw, .)}

    dw <- catchCat.WatAgeB %>%
        filter(Catch.Cat. == "D") %>%
        select(all_of(selCol)) %>%
        {as.numeric(.) * 1e-3} %>%
        ## Check method for filling gaps:
        {ifelse(is.na(.), cw, .)}

    sw <- cw

    dataSuffix <- paste0("_", dataYear, "_age",
                         minAge, "-", plusGroup, "+.dat")

    write_lines(round(cn, 5),
                file = file.path(resDir, "Formated_SA_inputs",
                                 paste0("cn", dataSuffix)),
                sep = "\t", na = "NA")

    write_lines(cw,
                file = file.path(resDir, "Formated_SA_inputs",
                                 paste0("cw", dataSuffix)),
                sep = "\t", na = "NA")

    write_lines(dw,
                file = file.path(resDir, "Formated_SA_inputs",
                                 paste0("dw", dataSuffix)),
                sep = "\t", na = "NA")

    write_lines(lf,
                file = file.path(resDir, "Formated_SA_inputs",
                                 paste0("lf", dataSuffix)),
                sep = "\t", na = "NA")

    write_lines(lw,
                file = file.path(resDir, "Formated_SA_inputs",
                                 paste0("lw", dataSuffix)),
                sep = "\t", na = "NA")

    write_lines(sw,
                file = file.path(resDir, "Formated_SA_inputs",
                                 paste0("sw", dataSuffix)),
                sep = "\t", na = "NA")
}


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
