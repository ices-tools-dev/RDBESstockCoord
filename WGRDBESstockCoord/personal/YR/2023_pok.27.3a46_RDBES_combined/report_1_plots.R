                                        #-*- coding: utf-8 -*-

### File: report_1_plots.R
### Time-stamp: <2023-10-06 13:03:12 a23579>
###
### Created: 06/10/2023	09:29:29
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################


## Comparisons to IC outputs (without BMS allocation):

## ###########################################################################
## Data importation and wrangling:

## Total catch (tons):
catonF <- dir(dataICDir, pattern = paste0(dataYear, "_caton"), full.names = FALSE)

ICcaton <- sapply(catonF,
                  function(x)
           {
               as.numeric(gsub("[, ]", "", tail(readLines(con = file.path(dataICDir, x)), 1)))
           }) %>%
    enframe() %>% pivot_wider() %>%
    rename_with(~ sub("(.).*", "\\1", .x)) %>%
    mutate(Origin = "IC")

Est_caton <- catonS1 %>%
    as.data.frame() %>%
    mutate(Origin = "Script")

catonSummary <- ICcaton %>%
    bind_rows(Est_caton) %>%
    pivot_longer(B:R, names_to = "catchCateg", values_to = "catchTons")

catonSummaryW <- catonSummary %>%
    pivot_wider(names_from = Origin, values_from = catchTons) %>%
    mutate(ratio = IC/Script,
           Rdiff = (ratio - 1),
           PC = paste0(sprintf("%.2f", 100 * ratio), "%"))

## Catch numbers at age:
canumF <- dir(dataICDir, pattern = paste0(dataYear, "_canum"), full.names = FALSE)

ICcanum <- sapply(canumF,
                  function(x)
           {
               ## browser()
               n <- length(readLines(con = file.path(dataICDir, x)))
               canum <-
                   gsub(" $", "",
                        gsub(",", "",
                             gsub("[ ]+", " ",
                                  readLines(con = file.path(dataICDir, x))[c(n-3, n)]))) %>%
                   str_split(" ")

               cn <- as.numeric(canum[[2]])
               names(cn) <- paste0("age", as.numeric(canum[[1]][1]):as.numeric(canum[[1]][2]))
               cn %>% enframe() %>% pivot_wider() %>%
                   bind_cols(catchCateg = sub("(.).*", "\\1", x), .)
           }) %>%
    bind_rows() %>%
    mutate(Origin = "IC")

Est_canum <-
    catchCat.long.plus %>%
    select(-meanW) %>%
    pivot_wider(names_from = age, names_prefix = "age", values_from = N) %>%
    {replace(., . == 0, NA)} %>%
    mutate(Origin = "Script")

canumSummary <- ICcanum %>%
    pivot_longer(starts_with("age"), names_to = "age", values_to = "N") %>%
    mutate(age = gsub("age", "", age),
           age = if_else(as.numeric(age) < plusGroup,
                         age, paste0(age, "+"))) %>%
    bind_rows(catchCat.long.plus %>%
              select(-meanW) %>%
              mutate(Origin = "Script") %>%
              rename(catchCateg = Catch.Cat.))

canumSummaryW <- canumSummary  %>%
    select(-sex) %>%
    pivot_wider(names_from = Origin, values_from = N) %>%
    mutate(ratio = IC/Script,
           Rdiff = (ratio - 1))

## weight at age:
wecaF <- dir(dataICDir, pattern = paste0(dataYear, "_weca"), full.names = FALSE)

ICweca <- sapply(wecaF,
                 function(x)
          {
              ## browser()
              n <- length(readLines(con = file.path(dataICDir, x)))
              weca <-
                  gsub(" $", "",
                       gsub(",", "",
                            gsub("[ ]+", " ",
                                 readLines(con = file.path(dataICDir, x))[c(n-3, n)]))) %>%
                  str_split(" ")

              cn <- as.numeric(weca[[2]])
              names(cn) <- paste0("age", as.numeric(weca[[1]][1]):as.numeric(weca[[1]][2]))
              cn %>% enframe() %>% pivot_wider() %>%
                  bind_cols(catchCateg = sub("(.).*", "\\1", x), .)
          }) %>%
    bind_rows() %>%
    mutate(Origin = "IC")

wecaSummary <- ICweca %>%
    pivot_longer(starts_with("age"), names_to = "age", values_to = "meanW") %>%
    mutate(age = gsub("age", "", age),
           age = if_else(as.numeric(age) < plusGroup,
                         age, paste0(age, "+"))) %>%
    bind_rows(catchCat.long.plus %>%
              select(-N) %>%
              mutate(Origin = "Script") %>%
              rename(catchCateg = Catch.Cat.))


wecaSummaryW <- wecaSummary  %>%
    select(-sex) %>%
    pivot_wider(names_from = Origin, values_from = meanW) %>%
    mutate(ratio = IC/Script,
           Rdiff = (ratio - 1))

## ###########################################################################
## Graphics:

## Overall catch weights:
ggCaton <- ggplot(data = catonSummaryW %>% filter(catchCateg != "R"),
                  aes(x = catchCateg, y = Rdiff)) +
    geom_histogram(stat = "identity", colour = "grey35") +
    scale_y_continuous(labels = scales::percent_format(),
                       name = "Relative diff. (IC - Scripts)"## ,
                       ## limits = c(-0.0035, 0.0035)
                       ) +
    xlab("Catch category") +
    theme_bw()

ggsave(ggCaton,
       filename = file.path(repDir, "catonComp.png"),
       width = 6, height = 4)

taf.png("caton_comp.png")
print(ggCaton)
dev.off()

## Catch numbers at age:
ggCanum <- ggplot(data = canumSummaryW %>% filter(catchCateg != "R"),
                  aes(x = age, y = Rdiff)) +
    geom_histogram(stat = "identity", colour = "grey35") +
    scale_y_continuous(labels = scales::percent_format(),
                       name = "Relative diff. (IC - Scripts)",
                       limits = c(-0.001, 0.001)) +
    scale_x_discrete(limits = function(x){x[order(as.numeric(gsub("\\+", "", x)))]}) +
    facet_wrap(~catchCateg) +
    theme_bw()

ggsave(ggCanum,
       filename = file.path(repDir, "canumComp.png"),
       width = 9, height = 6, scale = 0.8)

## Catch weights at age:
ggWeca <- ggplot(data = wecaSummaryW %>% filter(catchCateg != "R"),
                 aes(x = age, y = Rdiff)) +
    geom_histogram(stat = "identity", colour = "grey35") +
    scale_y_continuous(labels = scales::percent_format(),
                       name = "Relative diff. (IC - Scripts)",
                       limits = c(-0.001, 0.001)) +
    scale_x_discrete(limits = function(x){x[order(as.numeric(gsub("\\+", "", x)))]}) +
    facet_wrap(~catchCateg) +
    theme_bw()

ggsave(ggWeca,
       filename = file.path(repDir, "wecaComp.png"),
       width = 9, height = 6, scale = 0.8)


if (isTRUE(getOption("displayGraphics")))
{
    X11()
    print(ggCaton + ggtitle("caton"))

    X11()
    print(ggCanum + ggtitle("canum"))

    X11()
    print(ggWeca + ggtitle("weca"))
}

if (isTRUE(getOption("allocateBMS")))
{
    ## ... comparisons of SA-formated inputs otherwise:

    dataSuffix <- paste0("_", dataYear, "_age",
                         minAge, "-", plusGroup, "+.dat")

    ageCols <- paste0("age_",
                      paste0(minAge:plusGroup,
                             c(rep("", plusGroup - minAge), "+")))

    cnIC <- read_tsv(file = file.path(dataICDir,
                                      paste0("cn", dataSuffix)),
                     col_names = ageCols) %>%
        mutate(file = "cn", origin = "IC.pp")

    cwIC <- read_tsv(file = file.path(dataICDir,
                                      paste0("cw", dataSuffix)),
                     col_names = ageCols) %>%
        mutate(file = "cw", origin = "IC.pp")

    dwIC <- read_tsv(file = file.path(dataICDir,
                                      paste0("dw", dataSuffix)),
                     col_names = ageCols) %>%
        mutate(file = "dw", origin = "IC.pp")

    lfIC <- read_tsv(file = file.path(dataICDir,
                                      paste0("lf", dataSuffix)),
                     col_names = ageCols) %>%
        mutate(file = "lf", origin = "IC.pp")

    lwIC <- read_tsv(file = file.path(dataICDir,
                                      paste0("lw", dataSuffix)),
                     col_names = ageCols) %>%
        mutate(file = "lw", origin = "IC.pp")

    SAinput.comp <- rbind(cn, cw, dw, lf, lw) %>%
        as.data.frame() %>%
        rename_at(vars(everything()), ~ ageCols) %>%
        {mutate(., file = row.names(.),
                origin = "TAF")} %>%
        bind_rows(cnIC, cwIC, dwIC, lfIC, lwIC) %>%
        pivot_longer(cols = "age_3":"age_10+", names_to = "Age", names_prefix = "age_",
                     values_to = "Value") %>%
        pivot_wider(names_from = "origin", names_prefix = "Value_",
                    values_from = "Value") %>%
        mutate(ratio = Value_IC.pp / Value_TAF,
               Rdiff = (ratio - 1))

    ggSAinputs <- ggplot(data = SAinput.comp,
                         aes(x = Age, y = Rdiff)) +
        geom_histogram(stat = "identity", colour = "grey35") +
        scale_y_continuous(labels = scales::percent_format(),
                           name = "Relative diff. (IC - Scripts)",
                           limits = c(-0.001, 0.001)) +
        scale_x_discrete(limits = function(x){x[order(as.numeric(gsub("\\+", "", x)))]}) +
        facet_wrap(~file) +
        theme_bw()

    ggsave(ggSAinputs,
           filename = file.path(repDir, "SA_inputs_Comp.png"),
           width = 10, height = 6, scale = 0.8)

    if (isTRUE(getOption("displayGraphics")))
    {
        X11()
        print(ggSAinputs)
    }

}





### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
