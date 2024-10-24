library(dplyr)
library(tidyr)
library(ggplot2)
library(rlang) # for curly curly but i think .data$ would also work

# this is just some example data to test the functions
census <- read.csv('caton_raw.csv') %>% subset(Discards.Imported.Or.Raised=='Imported')

# first longhand way of doing this (without helper functions)
census %>% group_by(x=Fleet1,y=Country,wrap=Catch.Cat.) %>%
  summarise(fill= sum(Catch..kg*1e-6), groups='drop') %>% 
  mutate(fill=na_if(fill,0)) %>% 
  ggplot(aes(x,y,fill=fill)) + 
    geom_raster() +
    scale_fill_viridis_c(option='B',direction=-1,name='Catch\n(kt)') +
    facet_wrap(~wrap) +
    xlab('Fleet') + ylab('Country') + ggtitle('Black angler in 78abd')


#' A function to aggregate a dataframe to prepare it for tile_plot
#' 
#' @param data a data frame
#' @param x x aestetic to be passed to ggplot
#' @param y y aestetic to be passed to ggplot
#' @param fill fill aestetic to be passed to ggplot (numeric)
#' @param wrap optional parameter be passed to factet_wrap
#' 
#' @details 
#' This is just a simple wrapper function, you may prefer to explore your data 
#' directly using dplyr and ggplot. It also calculates a field called p
#' which is the proportion of catch (or whatever 'fill' is) in each facet.
#' This can be used to plot these as text, see examples

tile_agg <- function(data,x,y,fill,wrap=NA) {
  # aggregate the data by x, y, fill and wrap (which can be NA but not missing)
  # set any (fill) value of zero to NA so they stand out in the plot later on
  agg <- data %>% 
    group_by(x={{x}},y={{y}},wrap={{wrap}}) %>%
    summarise(fill=sum({{fill}}),.groups='drop') %>%
    group_by(wrap) %>%
    mutate(p=na_if(fill/sum(fill),0),fill=na_if(fill,0))
    return(agg)
}

# example
tile_agg(data=census,x=Fleet1,y=Country,fill=Catch..kg*1e-6) %>% head()
  
#' A function to make tile plots of whatever
#' 
#' @param agg a data frame with x, y, fill and wrap as produced by tile_agg()
#' @param xname optional name for x axis
#' @param yname optional name for y axis
#' @param fillname optional name for colour scale
#' @param title optional title for the plot
#' @param scale logical - do you want each facet to be scaled to it sums to 1
#' 
#' @details 
#' This is just a simple wrapper function, you may prefer to explore your data 
#' directly using dplyr and ggplot

tile_plot <- function(agg,xname=NULL,yname=NULL,fillname=NULL,title=NULL,scale=F) {
  if(scale) agg$fill <- agg$p
    g <- ggplot(agg,aes(x,y=y,fill=fill)) +
    geom_raster() +
    scale_fill_viridis_c(option='B',direction=-1,name=fillname) +
    xlab(xname) + ylab(yname) + ggtitle(title)
  if(!all(is.na(agg$wrap))) g <- g + facet_wrap(~wrap)
  return(g)
}


# simple example
tile_agg(data=census,x=Fleet1,y=Country,fill=Catch..kg*1e-6) %>%
  tile_plot()

# add facets and specify axis names, scale name and title
tile_agg(data=census,x=Fleet1,y=Country,fill=Catch..kg*1e-6,wrap=Catch.Cat.) %>%
  tile_plot(xname='Fleet',yname='Country',fillname='Catch\n(kt)',title='Anglerfish in 7,8abd')

# scale both facets so they sum to one
tile_agg(data=census,x=Fleet1,y=Country,fill=Catch..kg*1e-6,wrap=Catch.Cat.) %>%
  tile_plot(scale=T,fillname='prop',xname='Fleet',yname='Country',title='')

# add other ggplot elements
tile_agg(data=census,x=Fleet1,y=Country,fill=Catch..kg*1e-6,wrap=Catch.Cat.) %>%
  tile_plot() + 
  geom_text(aes(label=paste0(round(p*100),'%')),col=4,size=5) # p is calculated by tile_agg()

# you can still overwrite things like colour scale and various ways of naming scales
# but at that stage you may as well just use the ggplot directly
subset(census,Catch.Cat.=='Landings') %>% 
  tile_agg(x=Year,y=Area,fill=Catch..kg*1e-6,wrap=Country) %>%
  tile_plot(title='black anglerfish in 78abd') +
  scale_x_continuous(breaks=seq(2002,2024,by=2),name='Anno Domini') +
  ylab('New ylab') +
  scale_fill_distiller(direction=1) +
  labs(fill='Landings\n(kt)') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


##### some ways of visualising the data used to fill in missing discards

alldat <- read.csv('caton_raw.csv') %>% subset(Year==2023)

agg <- alldat %>% group_by(Country,Year,Fleet1,Source,Catch.Cat.) %>% 
  summarise(Catch..kg=sum(Catch..kg)) %>%
  pivot_wider(names_from=Catch.Cat.,values_from=Catch..kg)

ggplot(subset(agg,Landings>0 & Discards>0),aes(paste(Country,Fleet1),Landings,fill=Source=='Imported')) + geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


library(treemapify) # depends on a few other libraries, probably want to avoid

ggplot(subset(agg,Landings>0 & Discards>0),aes(area=Landings,fill=Source=='Imported',subgroup=paste(Country,Fleet1,sep='\n'))) +
  geom_treemap() + geom_treemap_subgroup_border(colour=1) + geom_treemap_subgroup_text(colour=grey(0.5),place='center')
  
library(ggalluvial)

agg <- alldat %>% subset(Catch.Cat.=='Landings') %>%
  group_by(Country,Year,Fleet1,Source) %>% 
  summarise(Catch..kg=sum(Catch..kg))

ggplot(agg,aes(y=Catch..kg,axis1=paste(Country,Fleet1),axis2=paste(Country,Source,Fleet1))) +
         geom_alluvium(aes(fill=Country,alpha=Source)) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) 


agg1 <- agg %>% mutate(Source=ordered(ifelse(Source=='Imported','Sampled','Imputed'),c('Sampled','Imputed')),
                       stratum=factor(paste(Country,Fleet1)))
ggplot(agg1,aes(x=Source,y=Catch..kg,stratum=stratum,alluvium=stratum,fill=stratum,label=stratum)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  scale_x_discrete(expand = c(.1, .1)) +
  theme(legend.position = "none") 
  



