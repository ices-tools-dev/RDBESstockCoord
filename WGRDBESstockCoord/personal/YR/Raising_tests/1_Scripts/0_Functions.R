#-*- coding: utf-8 -*-

### File: 0_Functions.R
### Time-stamp: <2025-10-29 08:44:25 a23579>
###
### Created: 13/06/2025	15:11:19
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### 
####################################################################################################

##' Function to check groups consistency.
##'
##' 
##' @title Function to check groups consistency.
##' @name check_group_conditions
##' @param catch_data census (or assembled census+estimated+raised) data.
##' @param condition_list A (named) list of conditions, with one condition per group. Supported formats are logical
##'     (same length as number of rows in census data), expressions, quosures, calls. A character strings describing the
##'     condition (as in filter(...), but with quotes) will be tentatively converted to quosure.
##' @param conditionType raising/allocation "strata" or "matched_data" (the latter is allowed to have overlap among
##'     groups).
##' @param domain The domain, used to match records with extra info.
##' @param variableType Variable type (for info in the log-file only).
##' @param logFile Log-file path.
##' @param append Whether to append to the log file (FALSE: override it).
##' @return Group conditions as a named list of logicals (1 per group).
##' @author Yves Reecht
check_group_conditions <- function(catch_data,
                                   condition_list,
                                   conditionType = c("strata", "matched_data"),
                                   domain = c("domainCatchDis", "domainCatchBMS",
                                              "domainBiology"),
                                   sourceType = c("unspecified", "WGValue", "Official"),
                                   variableType = c("unspecified", "WeightLive", "Number"),
                                   logFile = NULL, append = FALSE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 Oct 2024, 11:41
    library(rlang)

    ## Only for one source and variable type at once:
    sourceType <- match.arg(sourceType,
                            c("unspecified", "WGValue", "Official"),
                            several.ok = FALSE)

    variableType <- match.arg(variableType,
                              c("unspecified", "WeightLive", "Number"),
                              several.ok = FALSE)

    domain <- match.arg(arg = domain,
                        choices = c("domainCatchDis", "domainCatchBMS", "domainBiology"),
                        several.ok = FALSE)

    on.exit(if (!is.null(logFile))
            {
                sink(file = NULL, type = "message")
                close(con = logFile)
            })

    conditionType <- match.arg(tolower(conditionType),
                               c("strata", "matched_data"))

    logFileBak <- logFile
    if (! is.null(logFile))
    {
        logFile <- file(description = logFile, open = ifelse(isTRUE(append), "a", "w"))
    }

    cond2logical <- function(x, catch_data = catch_data)
    {
        ## Condition provided as a logical
        ## (has to match the dimensions in catch_data):
        if (is.logical(x))
        {
            if (length(x) == nrow(catch_data))
            {
                return(replace(x, is.na(x), FALSE))
            }else{
                return(paste0("Condition does not match the number of rows: the census table has ",
                              nrow(catch_data),
                              " records, while the condition has ",
                              length(x), " entries."))
            }

        }

        ## Condition provided as a character string => to quosure:
        if (is.character(x) && length(x) == 1)
        {
            x <- eval(parse_expr(paste0("quo(", x, ")")))
        }

        ## Condition provided as quosure or call:
        if ("quosure" %in% class(x) ||
            "call" %in% class(x))
        {
            res <- tryCatch(seq_len(nrow(catch_data)) %in%
                            (catch_data %>%
                             mutate(idxTmp = 1:n()) %>%
                             filter(!!x) %>%
                             pull(idxTmp)),
                            error = function(e) return(e))
            return(res)
        }

        ## Condition provided as expression:
        if ("expression" %in% class(x))
        {
            res <- tryCatch(seq_len(nrow(catch_data)) %in%
                            (catch_data %>%
                             mutate(idxTmp = 1:n()) %>%
                             filter(eval(x)) %>%
                             pull(idxTmp)),
                            error = function(e) return(e))
            return(res)
        }

        ## If still in the function, the specification is not supported
        return(paste0("Unsupported stratum definition: class={\"",
                      paste(class(x), collapse = "\", \""),
                      "\"}, length=", length(x)))
    }

    cond_st <- sapply(condition_list,
                      cond2logical, catch_data = catch_data,
                      simplify = FALSE)

    ## Report if any condition invalid:

    idxErr <- sapply(cond_st,
                     function(x) any(c("error", "rlang_error") %in% class(x)))
    if (any(idxErr))
    {
        nm <- names(cond_st)
        if (is.null(nm)) nm <- paste0("Cond_", seq_along(cond_st))

        errMsgs <- paste0(nm[idxErr], ": ", sapply(cond_st[idxErr], as.character)) ##unlist(cond_st[idxErr], recursive = FALSE))

        stop("Invalid strata definitions: \n\t* ",
             paste(errMsgs, collapse = "\n\t* "))
    }

    ## Check consistency of conditions:
    sink(file = logFile, type = "message", append = isTRUE(append))

    message("\n\n",
            timestamp())
    message(paste0("\n## ############################################################",
                   "\n## Condition diagnostics: ",
                   "\n## conditionType = \"", conditionType, "\"",
                   "\n## domain = \"", domain,
                   "\"; variableType = \"", variableType, "\"\n"))

    ## Should variableType be used for filtering? [???]

    warn <- FALSE
    errorList <- NULL

    cond_mat <- sapply(cond_st, cbind)

    notIncluded <- (! as.logical(apply(cond_mat, 1, sum)))

    replicated <- apply(cond_mat, 1, sum) > 1

    if (conditionType == "strata")
    {

        notIncludedS <- notIncluded &
            catch_data$catchCategory %in% "LAN" &
            is.na(catch_data %>% pull(domain)) # Make generic with pull.
        if (any(notIncludedS))
        {
            warn <- TRUE

            warning(sum(notIncludedS),
                    " records without estimates are not included in any raising stratum",
                    immediate. = TRUE)
            message("Warning message:\n",
                    sum(notIncludedS), " records without estimates are not included in any raising stratum:\n")
            if (! is.null(logFile))
            {
                oO <- options("width")
                options("width" = 300)
                capture.output(print(as.data.frame(catch_data[notIncludedS, ]),
                                     max = 50 * ncol(catch_data[notIncludedS, ])),
                               file = logFile)
                options(oO)
            }else{
                print(as.data.frame(catch_data[notIncludedS, ]),
                      max = 10 * ncol(catch_data[notIncludedS, ]))
            }
        }

        if (any(replicated))
        {
            errorList <- c(errorList,
                           paste0("Error: ", sum(replicated), " records assigned to more than one stratum!"))
            message("Error: ", sum(replicated), " records assigned to more than one stratum!")
        }
    }else{ # Matched data:

        notIncludedM <- notIncluded &
            catch_data$catchCategory %in% "LAN" &
            ! is.na(catch_data %>% pull(domain)) # Make generic with pull
        if (any(notIncludedM))
        {
            warn <- TRUE

            warning(sum(notIncludedM), " records with estimates are not matched to any raising stratum")
            message("Warning message:\n",
                    sum(notIncludedM), " records with estimates are not matched to any raising stratum:\n")
            flush(stderr())
            if (! is.null(logFile))
            {
                oO <- options("width")
                options("width" = 300)
                capture.output(print(as.data.frame(catch_data[notIncludedM, ]),
                                     max = 50 * ncol(catch_data[notIncludedM, ])),
                               file = logFile)
                options(oO)
            }else{
                print(as.data.frame(catch_data[notIncludedM, ]),
                      max = 10 * ncol(catch_data[notIncludedM, ]))
            }
        }
    }

    if (! is.null(logFile))
    {
        sink(file = NULL, type = "message")

        if (isTRUE(warn))
            warning("Warnings were reported in the log file: \"",
                    logFileBak,
                    "\"")
    }

    if (length(errorList))
    {
        stop("\n\t* ",
             paste(errorList, collapse = "\n\t* "))
    }

    return(cond_st)

}


## ###########################################################################
## Misc. helper functions:


unit2ratio <- function(from, to)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 28 Oct 2025, 15:02

    conv <- tibble(from = {{from}}, to = {{to}}) %>% distinct()

    ## Valid conversions:
    lookup <- bind_rows(tibble(from = "t", to = "kg", ratio = 1e3),
                        tibble(from = "t", to = "g", ratio = 1e6),
                        tibble(from = "kg", to = "g", ratio = 1e3),
                        tibble(from = "kg", to = "t", ratio = 1e-3),
                        tibble(from = "g", to = "kg", ratio = 1e-3),
                        tibble(from = "g", to = "t", ratio = 1e-6),
                        tibble(from = "1000_pcs", to = "pcs", ratio = 1e3),
                        tibble(from = "pcs", to = "1000_pcs", ratio = 1e-3)) %>%
        mutate(valid = TRUE)

    res <- lookup %>%
        semi_join(conv,
                  by = c("from", "to")) %>%
        mutate(new = to) %>%
        ## For invalid conversions, the ratio is one and the new unit is the old one (from)
        bind_rows(conv %>%
                  anti_join(lookup,
                            by = c("from", "to")) %>%
                  mutate(ratio = 1, valid = FALSE, new = from))

    ## Should not happen, just as safety:
    if (nrow(res) == 0)
    {
        res <- tibble(from = {{from}}, to = {{to}},
                      ratio = 1, valid = FALSE)
    }

    return(res)
}

convert_field <- function(data, valueField,
                          unitField = "variableUnit", 
                          to = c("kg", "pcs"))
{
    ## Purpose: Conversion
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 28 Oct 2025, 14:47

    ## library(units)
    ## units::valid_udunits() %>% filter(name_singular %in% c("kilogram", "metric_ton", "gram"))
    ## units::valid_udunits() %>% filter(name_singular %in% c(""))
    ## units::valid_udunits()$name_singular
    ## ?units::make_units
    ## 
    ## Could be more robust to define units with the appropriate package, but going for the quick
    ## fix now!
    
    ## unit2ratio(from = "kg", to = "t")
    ## unit2ratio(from = "1000_pcs", to = "t")
    ## unit2ratio(from = "1000_pcs", to = "pcs")

    ## unit2ratio(from = c("kg", "1000_pcs", "1000_pcs"),
    ##            to = c("t", "t", "pcs"))

    ## unit2ratio(from = c("kg", "1000_pcs", "1000_pcs"),
    ##            to = c("t"))
    
    for (unit in to)
    {
        convU <- unit2ratio(from = data %>% pull(unitField) %>% unique(),
                            to = unit) %>%
            dplyr::rename_with(~unitField, .cols = c(from)) %>%
            dplyr::rename_with(~paste0(.x, "_convSuffix"), .cols = c(to:new))

        data <- data %>%
            left_join(convU,
                      by = "variableUnit",
                      suffix = c("_dummySuffix", "")) %>% # In the unlikely event of similar names
                                        # in  data
            mutate(across(all_of(valueField),
                          ~ .x * ratio_convSuffix),
                   across(all_of(unitField),
                          ~ new_convSuffix)) %>%
            select(-ends_with("_convSuffix")) %>%
            rename_with(~sub("_dummySuffix$", "", .x)) # In the unlikely event of similar names
                                        # in  data
    }
    
    return(data)
}


fieldsToStrata <- function(.data, ...)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 Oct 2025, 15:08
    require(dplyr)
    
    key <- .data %>%
        dplyr::select(...) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(.key = paste(dplyr::c_across(dplyr::everything()),
                                   collapse = "+")) %>%
        dplyr::pull(.key)

    return(sapply(unique(key),
                  function(k){key %in% k},
                  simplify = FALSE))
}





### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
