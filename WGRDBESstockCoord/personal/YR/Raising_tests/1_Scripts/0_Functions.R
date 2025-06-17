#-*- coding: utf-8 -*-

### File: 0_Functions.R
### Time-stamp: <2025-06-16 14:45:21 a23579>
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
##' @param census_data census (or assembled census+estimated+raised) data.
##' @param condition_list A (named) list of conditions, with one condition per group. Supported formats are logical
##'     (same length as number of rows in census data), expressions, quosures, calls. A character strings describing the
##'     condition (as in filter(...), but with quotes) will be tentatively converted to quosure.
##' @param conditionType raising/allocation "strata" or "matched_data" (the latter is allowed to have overlap among
##'     groups).
##' @param dataType The data type, used to match the domain + info.
##' @param variableType Variable type (for info in the log-file only).
##' @param logFile Log-file path.
##' @param append Whether to append to the log file (FALSE: override it).
##' @return Group conditions as a named list of logicals (1 per group).
##' @author Yves Reecht
check_group_conditions <- function(census_data,
                                   condition_list,
                                   conditionType = c("strata", "matched_data"),
                                   dataType = "discards", variableType = "unspecified",
                                   logFile = NULL, append = FALSE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 Oct 2024, 11:41
    library(rlang)

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

    cond2logical <- function(x, census_data = census_data)
    {
        ## Condition provided as a logical
        ## (has to match the dimensions in census_data):
        if (is.logical(x))
        {
            if (length(x) == nrow(census_data))
            {
                return(replace(x, is.na(x), FALSE))
            }else{
                return(paste0("Condition does not match the number of rows: the census table has ",
                              nrow(census_data),
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
            res <- tryCatch(seq_len(nrow(census_data)) %in%
                            (census_data %>%
                             mutate(idxTmp = 1:n()) %>%
                             filter(!!x) %>%
                             pull(idxTmp)),
                            error = function(e) return(e))
            return(res)
        }

        ## Condition provided as expression:
        if ("expression" %in% class(x))
        {
            res <- tryCatch(seq_len(nrow(census_data)) %in%
                            (census_data %>%
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
                      cond2logical, census_data = census_data,
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
                   "\n## dataType = \"", dataType,
                   "\"; variableType = \"", variableType, "\"\n"))


    warn <- FALSE
    errorList <- NULL

    cond_mat <- sapply(cond_st, cbind)

    notIncluded <- (! as.logical(apply(cond_mat, 1, sum)))

    replicated <- apply(cond_mat, 1, sum) > 1

    if (conditionType == "strata")
    {

        notIncludedS <- notIncluded &
            census_data$catchCategory %in% "LAN" &
            is.na(census_data[ , switch(dataType,
                                        "discards" = "domainCatchDis",
                                        "BMS" = "domainCatchBMS")][[1]]) # Make generic with pull.
        if (any(notIncludedS))
        {
            warn <- TRUE

            warning(sum(notIncludedS), " records without estimates are not included in any raising stratum")
            message("Warning message:\n",
                    sum(notIncludedS), " records without estimates are not included in any raising stratum:\n")
            if (! is.null(logFile))
            {
                oO <- options("width")
                options("width" = 300)
                capture.output(print(as.data.frame(census_data[notIncludedS, ]),
                                     max = 50 * ncol(census_data[notIncludedS, ])),
                               file = logFile)
                options(oO)
            }else{
                print(as.data.frame(census_data[notIncludedS, ]),
                      max = 10 * ncol(census_data[notIncludedS, ]))
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
            census_data$catchCategory %in% "LAN" &
            ! is.na(census_data[ , switch(dataType,
                                          "discards" = "domainCatchDis",
                                          "BMS" = "domainCatchBMS")][[1]]) # Make generic with pull
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
                capture.output(print(as.data.frame(census_data[notIncludedM, ]),
                                     max = 50 * ncol(census_data[notIncludedM, ])),
                               file = logFile)
                options(oO)
            }else{
                print(as.data.frame(census_data[notIncludedM, ]),
                      max = 10 * ncol(census_data[notIncludedM, ]))
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







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
