# export the data through ic menu 8.Extract and view imported stock/year data
# click: Check catch
# click: Export CATONs; save as StockOverview_[YEAR].text
# click: Export numbers; save as Numbers_at_age_and_mean_weights_at_age_[YEAR].zip

library(dplyr)
library(tidyr)
library(ggplot2)

setwd('./data')

# read in some ad-hoc functions
source('../aggregate_IC_functions.R')


###################################################################################
# STEP 1 - Import the data
###################################################################################

# year range
years <- 2002:2023

# import data, first the StockOverview files
files <-  paste0('StockOverview_ank_',years,'.txt')
caton <- read.caton(files)

# then the samplingdata (Numbers_at_age_and_mean_weights_at_age.zip)
files <- paste0('Numbers_at_age_and_mean_weights_at_age_ank_',years,'.zip')
canum <- read.canum(files)
sampnum <- read.sampnum(files)

caton0 <- caton #keep for later
canum0 <- canum #keep for later

# keep track of changes in the datasets
sumdat <- data.frame(ImportedData=summarise.data(caton,canum))
sumdat

# check that we do have the correct updated UK info for 2021
subset(caton,Country=='UK (England)' & Year==2021) %>% group_by(Year) %>% summarise(sum(Catch..kg)/1000) 
1321.961+286.11+0.121 #from email - should be the same (1608)

######


# quick look at the raw length data
a <- canum %>% group_by(Year,Catch.Cat.,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
ggplot(subset(a,Year>=2002&l<100),aes(l,n*1e-6,col=Catch.Cat.)) + geom_line() + facet_wrap(~Year,scales='free_y') + xlab('length (cm)') + ylab('Raw length freq')
# last 3 years by country
a <- subset(canum,Year>max(Year)-3) %>% group_by(Year,Country,Catch.Cat.,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
ggplot(subset(a,l<100),aes(l,n*1e-6,col=Catch.Cat.)) + geom_line() + facet_grid(Year~Country) + xlab('length (cm)') + ylab('Raw length freq')


########################'###########################################################
# STEP 2 - Some initial checks
###################################################################################

# we should only have data for one stock
# its ok, the stock name changed in 2018
table(caton$Stock)
table(canum$Stock)

# should not be any records in 7a though; they are all assumed to be piscatorius
table(caton$Area) 
subset(caton,Area=='27.7.a') %>% group_by(Year,Country,Catch.Cat.) %>% summarise(Catch=sum(Catch..kg)/1000) 
# only UK reported 40kg of landings in 7a in 2017 and again in later years
# just delete these data; it doesnt amount to enough to make it worth while adding back in to white angler
caton <- subset(caton,!Area=='27.7.a')
canum <- subset(canum,!Area=='27.7.a')

# which countries submitted data in which years?
a <- caton %>% group_by(Year,Country,Catch.Cat.) %>% summarise(Tonnes=sum(Catch..kg)/1000)
a <- a %>% group_by(Catch.Cat.) %>% mutate(Tot=sum(Tonnes))
#just checking
subset(a,Catch.Cat.=='Landings' & Year==max(Year))

pal <- colorRampPalette(c("lightyellow", "yellow", "orange", "red", "brown4"))(12)
g <- ggplot(subset(a,Catch.Cat.%in%c('Landings','Discards')), aes(x=Year,y=Country)) + 
  geom_tile(aes(fill=Tonnes/Tot)) + 
  scale_fill_gradientn(colours=pal) + 
  theme(legend.position = "none") + 
  scale_y_discrete(drop=FALSE) + 
  facet_wrap(~Catch.Cat.)
g
png('../CountryYear.png',10,4,'in',res=600); g; dev.off()


# only UK submitted discards for 2002, best just remove so we dont extrapolate extravagantly later
caton <- subset(caton,!(Year==2002 & Catch.Cat.=='Discards'))
canum <- subset(canum,!(Year==2002 & Catch.Cat.=='Discards'))

## add the missing belgian and other countries' landings 
# see spreadsheet Official_landings.xlsx
# in recent years this is only German landings. No GER data for 2017. It is only 1% so ignore for now - no need to update
caton_ <- data.frame(Country='Others'
                     ,Season=2002:2017
                     ,Season.type='Year'
                     ,Year=2002:2017
                     ,Stock='ank.27.78ab'
                     ,Area='27.7'
                     ,Fleets='MIS_MIS'
                     ,Effort=NA
                     ,UnitEffort=NA
                     ,Catch..kg=c(118751,119459,157239,167390,70882,161866,204933,244349,316448,381992,52617,68302,74333,69201,94269,0)
                     ,Catch.Cat.='Landings'
                     ,Report.cat.='R-Reported'
                     ,Discards.Imported.Or.Raised='Imported')

caton <- rbind(caton,caton_) #only run this line once!
# keep track of changes in the datasets
sumdat <- data.frame(sumdat,MissingIcLandings=summarise.data(caton,canum))
sumdat
sumdat[2,2]/sumdat[2,1]
# adds 1% to total weight

# areas
a <- with(caton,tapply(Catch..kg/1000,list(Catch.Cat.,Area),sum,na.rm=T))
a[is.na(a)] <- 0
barplot(a,las=2)

a <- caton %>% group_by(Year,Area,Catch.Cat.) %>% summarise(Tonnes=sum(Catch..kg)/1000)
a <- a %>% group_by(Catch.Cat.) %>% mutate(Tot=sum(Tonnes))
pal <- colorRampPalette(c("lightyellow", "yellow", "orange", "red", "brown4"))(12)
g <- ggplot(subset(a,Catch.Cat.%in%c('Landings','Discards')), aes(x=Year,y=Area)) + 
  geom_tile(aes(fill=Tonnes/Tot)) + 
  scale_fill_gradientn(colours=pal) + 
  theme(legend.position = "none") + 
  scale_y_discrete(drop=FALSE) + 
  facet_wrap(~Catch.Cat.)
g
png('../AreaYear.png',10,4,'in',res=600); g; dev.off()

#the area 7 landings are those added in 'manually'


# are all catches submitted by quarter?
with(caton,tapply(Catch..kg/1000,list(Season.type),sum)) 
# some catches reported by year - need to deal with this

# any non-reported landings etc? - Yes
with(caton,tapply(Catch..kg/1000,list(Report.cat.),sum)) 

# fleets
round(with(caton,tapply(Catch..kg/1000,list(Fleets),sum))) # inconsistent use of _all suffix
caton <- caton %>% mutate(Fleets = gsub('_all|_All','',Fleets))
# check that this did not create duplicates
i <- duplicated(select(caton,Country,Season,Season.type,Year,Area,Fleets,Catch.Cat.,Report.cat.))
sum(i) # should be zero
# same for canum
canum <- canum %>% mutate(Fleets = gsub('_all|_All','',Fleets))
i <- duplicated(canum[,1:10])
sum(i) # should be zero
# and sampnum
sampnum <- sampnum %>% mutate(Fleets = gsub('_all|_All','',Fleets))

# simplified fleets
with(caton,tapply(Catch..kg/1000000,list(Fleets),sum)) # still too many by far
png('../Metiers1.png',6,10,'in',res=600)
par(mar=c(3,12,1,1))
barplot(with(caton,tapply(Catch..kg/1000000,list(Fleets),sum)),horiz=T,las=1)
dev.off()

# quite a bit OTB/OTT_DEF_>=70, so not possible to split into <100 and >100mm
fleetlut <- data.frame(Fleets=sort(unique(caton$Fleets)))
fleetlut$Fleet1 <- 'MIS_MIS'
fleetlut[substring(fleetlut$Fleets,1,1)=='G','Fleet1'] <- 'GNS_DEF'
fleetlut[substring(fleetlut$Fleets,1,7)=='OTB_CRU','Fleet1'] <- 'OTB_CRU'
fleetlut[substring(fleetlut$Fleets,1,7)=='OTT-CRU','Fleet1'] <- 'OTB_CRU'
fleetlut[substring(fleetlut$Fleets,1,7)=='OTT_CRU','Fleet1'] <- 'OTB_CRU'
fleetlut[substring(fleetlut$Fleets,1,7)=='OTB_DEF','Fleet1'] <- 'OTB_DEF'
fleetlut[substring(fleetlut$Fleets,1,7)=='OTT-DEF','Fleet1'] <- 'OTB_DEF'
fleetlut[substring(fleetlut$Fleets,1,7)=='OTT_DEF','Fleet1'] <- 'OTB_DEF'
fleetlut[substring(fleetlut$Fleets,1,7)=='TBB_DEF','Fleet1'] <- 'TBB_DEF'
with(inner_join(caton,fleetlut),tapply(Catch..kg/1000000,list(Fleet1),sum)) 
caton <- inner_join(caton,fleetlut)
canum <- inner_join(canum,fleetlut)

a <- caton %>% group_by(Year,Fleet1,Catch.Cat.) %>% summarise(Tonnes=sum(Catch..kg)/1000)
a <- a %>% group_by(Catch.Cat.) %>% mutate(Tot=sum(Tonnes))
g <- ggplot(subset(a,Catch.Cat.%in%c('Landings','Discards')), aes(x=Year,y=Fleet1)) + 
  geom_tile(aes(fill=Tonnes/Tot)) + 
  scale_fill_gradientn(colours=pal) + 
  theme(legend.position = "none") + 
  scale_y_discrete(drop=FALSE) +
  facet_wrap(~Catch.Cat.)
g
png('../FleetYear.png',10,4,'in',res=600); g; dev.off()

# a lot of mis_mis discards in 2022 - France
subset(caton,Year==2022 & Catch.Cat.=='Discards') %>% group_by(Country,Fleet1) %>% summarise(Tonnes=sum(Catch..kg)/1000)


# sample numbers
a <- left_join(caton,sampnum)
a <- full_join(a,canum %>% group_by(Country,Season,Year,Stock,Area,Fleets,Catch.Cat.,Report.cat.) %>% summarise(SampleData=T))
a$NumSamplesLength[!is.na(a$SampleData) & is.na(a$NumSamplesLength)] <- -9
a$MissingNumSamp <- ifelse(a$NumSamplesLength==-9,T,F)
b <- a %>% group_by(MissingNumSamp,Year,Catch.Cat.) %>% summarise(AnnualCatch=sum(Catch..kg))
a <- a %>% group_by(Year,Catch.Cat.) %>% mutate(AnnualCatch=sum(Catch..kg))
a$NumSamplesLength[is.na(a$NumSamplesLength)]<- 0

g <- ggplot(subset(a,Catch.Cat.=='Landings'),aes(Catch..kg/AnnualCatch,NumSamplesLength+0.1,col=Country)) + geom_point() + facet_wrap(~Year) + scale_y_log10() + ggtitle('Landings')
g

ggplot(b,aes(Year,AnnualCatch,col=Catch.Cat.)) +geom_point() + geom_line() + facet_wrap(~MissingNumSamp)


# added the 'irish' minimum sample nos line
g <- ggplot() + 
  geom_point(aes(Catch..kg/AnnualCatch,NumSamplesLength+0.1,col=Country),subset(a,Catch.Cat.=='Landings')) + 
  geom_line(aes(x,y),data.frame(x=0:25/100,y=0:25/4)) + facet_wrap(~Year) + scale_y_log10() + ggtitle('Landings')
g
png('../NumSamLan.png',10,6,'in',res=600); g; dev.off()

# different:
b <- a %>% group_by(Year,Catch.Cat.) %>% mutate(MaxSam=max(NumSamplesLength),MaxCaton=max(Catch..kg),i=percent_rank(-Catch..kg)) %>% arrange(Year,-MaxCaton)
ggplot(subset(b,Catch.Cat.=='Landings')) + 
  geom_point(aes(i,NumSamplesLength,col=Country)) +
  geom_line(aes(i,MaxSam*Catch..kg/MaxCaton)) + 
  scale_x_discrete(name ="Stratum") +
  scale_y_continuous(name ="Number of samples") +
  facet_wrap(~Year) + ggtitle('Landings')


# some strata that make up >5% of the annual landings, but <10 samples, also plenty of strata with only 1 sample
g <- ggplot(subset(a,Catch.Cat.=='Discards'),aes(Catch..kg/AnnualCatch,NumSamplesLength,col=Country)) + geom_point() + facet_wrap(~Year) + scale_y_log10() + ggtitle('Discards')
g <- ggplot() + 
  geom_point(aes(Catch..kg/AnnualCatch,NumSamplesLength+0.1,col=Country),subset(a,Catch.Cat.=='Discards' & NumSamplesLength>0)) + 
  geom_line(aes(x,y),data.frame(x=0:25/100,y=0:25/4)) + facet_wrap(~Year) + scale_y_log10() + ggtitle('Discards')
g
png('../NumSamDis.png',10,6,'in',res=600); g; dev.off()
# spain lumps most of their discards into one strata, but allegedly with very high sample numbers...

# France has only reported sample numhbers for mis_mis in 2020
select(subset(a,Country=='France' & Year==2020 & Catch.Cat.=='Discards'),Country,Season,Fleet1,Catch..kg,NumSamplesLength)
subset(caton,Country=='France' & Year==2020 & Catch.Cat.=='Discards')
subset(sampnum,Country=='France' & Year==2020 & Catch.Cat.=='Discards')


a$NumSamplesLength[is.na(a$NumSamplesLength)] <- 0
a$NumSamplesLength[a$NumSamplesLength==-9] <- 0

g <- ggplot() + 
  geom_point(aes(Catch..kg*1e-3,pmin(NumSamplesLength,20),col=Country),subset(a,Catch.Cat.=='Discards')) + 
  facet_wrap(~Year) + xlab('Catch (t)') + ylab('Number of samples (20+ combined)') + ggtitle('Discards')
g
png('../NumSamDis.png',10,6,'in',res=600); g; dev.off()

g <- ggplot() + 
geom_point(aes(Catch..kg*1e-3,pmin(NumSamplesLength,20),col=Country),subset(a,Catch.Cat.=='Landings')) + 
  facet_wrap(~Year) + xlab('Catch (t)') + ylab('Number of samples (20+ combined)') + ggtitle('Landings')
g
png('../NumSamLan.png',10,6,'in',res=600); g; dev.off()


# try to deal with duplicates, to get an estimate of actual sample size for SS
# also see folder: 2. input data\Sample numbers
b <- a %>% group_by(Year,Catch.Cat.,Country,Fleet=ifelse(substring(Fleet1,1,3)=='GNS','Gillnets','Trawls')) %>%
  summarise(NumSamplesLength=sum(unique(NumSamplesLength))) %>% #try to remove duplicates
  mutate(Country==ifelse(Country%in%c('France','Spain') & Fleet!='Gillnets',Country,'Others')) %>%
  group_by(Year,Catch.Cat.,Country,Fleet) %>%
  summarise(NumSamplesLength=sum(NumSamplesLength))
b <- subset(b,Catch.Cat.%in%c('Landings','Discards'))
g <- ggplot(b,aes(Year,NumSamplesLength,col=paste(Country,Fleet))) + geom_line() + geom_point() + facet_wrap(~Catch.Cat.)
g
png('../NumSamForSS.png',10,6,'in',res=600); g; dev.off()
write.csv(b,'../SampleNumbers.csv',row.names=F)


###################################################################################
# STEP 3 - SOP
###################################################################################

a <- sop(caton,canum,Country,Season,Year,Area,Fleets,Catch.Cat.,Report.cat.)
g <- ggplot(a, aes(x=Caton,y=SopR,col=Country)) + geom_point() + facet_wrap(~Catch.Cat.)
g
png('../Sop1.png',10,4,'in',res=600); g; dev.off()

b <- a %>% group_by(Year,Catch.Cat.) %>% summarise(Caton=sum(Caton),Sop=sum(Sop))
ggplot(b, aes(x=Year,y=Sop/Caton,col=Catch.Cat.)) + geom_point() + geom_line()

a %>% group_by(Catch.Cat.) %>% summarise(SopR=sum(Sop)/sum(Caton))

# sop correction
a <- sop(caton,canum,Country,Season,Year,Area,Fleets,Catch.Cat.,Report.cat.)
canum$FrequencyUploaded <- canum$Frequency
canum <- inner_join(canum,a)
canum$Frequency <- canum$Frequency/canum$SopR # only run this line once
canum$Caton <- NULL; canum$Sop <- NULL; canum$SopR <- NULL

# keep track of changes in the datasets
sumdat <- data.frame(sumdat,FirstSopCorr=summarise.data(caton,canum))
sumdat

###################################################################################
# STEP 4 - length-weight
###################################################################################

# LW parameters
par <- data.frame(a=0.0195,b=2.93)

# first look at length-weight
a <- canum %>% group_by(AgeLength,Weight,Country) %>% summarise()
g <- ggplot(a,aes(AgeLength,Weight,col=Country)) + geom_point()
g
png('../LW1.png',5,4,'in',res=600); g; dev.off()

# belgian lengths are in the wrong units during some years
a <- canum %>% group_by(Country,Year) %>% summarise(k=mean(AgeLength^par$b/Weight))
a[order(a$k),]
i <- which(canum$Country=='Belgium' & canum$Year%in%c(2013,2014,2016))
canum$AgeLength[i] <- canum$AgeLength[i]*10 # careful, do not run this line more than once!
a <- canum %>% group_by(AgeLength,Weight,Country) %>% summarise()
ggplot(a,aes(AgeLength,Weight,col=Country)) + geom_point()

# irish lengths>100cm were truncated
i <- which(canum$AgeLength<250 & canum$Weight>5000)
canum$AgeLength[i] <- canum$AgeLength[i]*10 # careful, do not run this line more than once!
a <- canum %>% group_by(AgeLength,Weight,Country) %>% summarise()
ggplot(a,aes(AgeLength,Weight,col=Country)) + geom_point()

# still some weird lengths (apparently not measured in whole cm)
i <- which(canum$AgeLength/10!=floor(canum$AgeLength/10))
canum[i,c(1,2,3,5,6,7,10)]
canum$AgeLength[i] <- round(canum$AgeLength[i]/10,0)*10
a <- canum %>% group_by(AgeLength,Weight,Country) %>% summarise()
ggplot(a,aes(AgeLength,Weight,col=Country)) + geom_point()

# Irish weights were based on gutted weights in some years! Replace with expected lw
i <- which(canum$Country=='Ireland')
canum$Weight[i] <- par$a*(canum$AgeLength[i]/10)^par$b
a <- canum %>% group_by(AgeLength,Weight,Country) %>% summarise()
ggplot(a,aes(AgeLength,Weight,col=Country)) + geom_point()

# Some strange french weights as well 
k <- with(canum,AgeLength^par$b/Weight)
plot(k)
canum[which(k<35000),]
i <- with(canum,which(Country=='France' & Year==2010 & Season==2 & Area=='27.7.j' & Fleets=='OTT_DEF_100-119_0_0'))
canum$Weight[i] <- par$a*(canum$AgeLength[i]/10)^par$b
a <- canum %>% group_by(AgeLength,Weight,Country) %>% summarise()
ggplot(a,aes(AgeLength,Weight,col=Country)) + geom_point()

# and some uk weights in kg instead of g
# fixed now
#i <- k>1e7 | canum$Weight==0
#canum$Weight[i] <- par$a*(canum$AgeLength[i]/10)^par$b
#a <- canum %>% group_by(AgeLength,Weight,Country) %>% summarise()
g <- ggplot(a,aes(AgeLength,Weight,col=Country)) + geom_point()
g
png('../LW2.png',5,4,'in',res=600); g; dev.off()



# sop correction (again - could have done this in one go but this is how the code happened to evolve)
a <- sop(caton,canum,Country,Season,Year,Stock,Area,Fleets,Catch.Cat.,Report.cat.)
canum <- inner_join(canum,a)
subset(canum,is.na(SopR)) # some length data with zero catch weights
# seems to be a rounding issue
canum$SopR[is.na(canum$SopR)] <- 1
canum$Frequency <- canum$Frequency/canum$SopR # only run this line once
canum$Caton <- NULL; canum$Sop <- NULL; canum$SopR <- NULL

# keep track of changes in the datasets
sumdat <- data.frame(sumdat,LWCorr=summarise.data(caton,canum))
sumdat


###################################################################################
# STEP 5 - deal with annual data
###################################################################################
with(caton,tapply(Catch..kg,list(Country,Season.type,Catch.Cat.),sum))
a <- with(caton,tapply(Catch..kg,list(Season.type,Year),sum))
a[is.na(a)]<-0
barplot(a,legend=T,args.legend=list(x='topleft',inset=0.02))
sum(a[2,])/sum(a) # percentage of catches submitted annually instead of by quarter 8.6%
max((a[2,]/colSums(a))) # max percentage in any year 31%
a[2,]/colSums(a)
plot(a[2,]/colSums(a))
# during some years, quite a lot of the catch submitted annually!

# split any annual estimates between the quarters, careful, do not run any lines twice
a <- subset(caton,Season.type=='Year')
a$Catch..kg <- a$Catch..kg/4
a$Season.type <- 'Quarter'
b <- NULL
for(i in 1:4) {a$Season=i; b<-rbind(b,a)}
caton <- subset(caton,Season.type!='Year')
caton <- rbind(caton,b)

############## also do it for canum1
a <- subset(canum,Season>4)
a$Frequency <- a$Frequency/4
a$FrequencyUploaded <- a$FrequencyUploaded/4
b <- NULL
for(i in 1:4) {a$Season=i; b<-rbind(b,a)}
canum <- subset(canum,Season<=4)
canum <- rbind(canum,b)

# check that we have not lost or gained any numbers
data.frame(sumdat,YearToQuarter=summarise.data(caton,canum))


###################################################################################
# STEP 6 - match landings and discards
###################################################################################

ld <- landis(caton,Country,Season,Year,Stock,Area,Fleets,Fleet1)
sum(subset(ld,!DisData)$Lan)/sum(ld$Lan)
ggplot(subset(ld,DisData),aes(x=Lan/1000,y=Dis/1000,col=Country)) + geom_point()


a <- ld %>% group_by(Year,Country,DisData) %>% summarise(Landings=sum(Lan)*1e-6)
g <- ggplot(a,aes(Year,Landings,fill=DisData)) + geom_col() + facet_wrap(~Country)
g
png('../LanDisData.png',10,8,'in',res=600); g; dev.off()


# problem with the spanish discard data
subset(ld,Lan==0 & Dis>1000)
subset(ld,Country=='Spain' & Area=='27.7.j.2')

a <- subset(caton,Country=='Spain') %>% group_by(Year,Area,Catch.Cat.) %>% summarise(Tonnes=sum(Catch..kg)/1000)
a <- a %>% group_by(Catch.Cat.) %>% mutate(Tot=sum(Tonnes))
pal <- colorRampPalette(c("lightyellow", "yellow", "orange", "red", "brown4"))(12)
ggplot(a, aes(x=Year,y=Area)) + 
  geom_tile(aes(fill=Tonnes/Tot)) + 
  scale_fill_gradientn(colours=pal) + 
  theme(legend.position = "none") + 
  scale_y_discrete(drop=FALSE) + 
  facet_wrap(~Catch.Cat.)

# no discard data for 2002, 2016
# up to 2012 all discards assigned to 27.7.j.2
# up to 2010 all landings assigned to 27.7.j.2
# that leaves 2011 and 2012 with a problem

# just re-assign the landings in those 2 years to 27.7.j.2 (easiest option)
i <- which(caton$Year%in%c(2011,2012) & caton$Country=='Spain' & substring(caton$Area,1,4)=='27.7')
caton$Area[i] <- '27.7.j.2'
i <- which(canum$Year%in%c(2011,2012) & canum$Country=='Spain' & substring(canum$Area,1,4)=='27.7')
canum$Area[i] <- '27.7.j.2'


ld <- landis(caton,Country,Season,Year,Stock,Area,Fleets,Fleet1)
sum(subset(ld,!DisData)$Lan)/sum(ld$Lan)

ggplot(subset(ld,DisData),aes(x=Lan/1000,y=Dis/1000,col=Country)) + geom_point()
ggplot(subset(ld,DisData & Year==2020),aes(x=Lan/1000,y=Dis/1000,col=Country)) + geom_point()
ggplot(subset(ld,DisData),aes(x=Lan/1000,y=Dis/1000,col=Fleet1)) + geom_point() + facet_wrap(~Year)

g <- ggplot(subset(ld,DisData),aes(x=Lan/1000,y=Dis/1000,col=Fleet1)) + geom_point()
g
png('../LanDis.png',10,8,'in',res=600); g; dev.off()

sum(subset(ld,Country=='Ireland' & Year==2020)$Dis)*1e-3 # thats right 82t after update in 2022

###################################################################################
# STEP 7 - fill in missing discards
###################################################################################

a <- subset(ld,DisData) %>% group_by(Country,Year) %>% summarise(PropDis=sum(Dis)/sum(Lan+Dis),Catch=sum(Lan+Dis))
g <- ggplot(a,aes(Year,PropDis,col=Country)) + geom_line() + geom_point() 
g
png('../DisCountry.png',5,4,'in',res=600); g; dev.off()


a <- subset(ld,DisData) %>% group_by(Fleet1,Year) %>% summarise(PropDis=sum(Dis)/sum(Lan+Dis),Catch=sum(Lan+Dis))
g <- ggplot(a,aes(Year,PropDis,col=Fleet1)) + geom_line() + geom_point() 
g
png('../DisFleet.png',5,4,'in',res=600); g; dev.off()

#a <- subset(ld,DisData) %>% group_by(Country,Fleet1,Year) %>% summarise(PropDis=sum(Dis)/sum(Lan+Dis),Catch=sum(Lan+Dis))
a <- ld %>% group_by(Country,Fleet1,Year) %>% summarise(PropDis=sum(Dis*DisData)/sum((Lan+Dis)*DisData),Catch=sum(Lan+Dis)) %>% filter(!is.na(PropDis))
g <- ggplot(a,aes(Year,PropDis,col=paste(Country))) + geom_line() + geom_point(aes(size=Catch)) + facet_wrap(~Fleet1)
g
png('../DisCountryFleet.png',10,6,'in',res=600); g; dev.off()

g <- ggplot(a,aes(Year,PropDis,col=Fleet1)) + geom_line() + geom_point(aes(size=Catch)) + facet_wrap(~Country)
g 
png('../DisFleetCountry.png',10,6,'in',res=600); g; dev.off()

#Country seems to be better predictor than Fleet
#high uk discard ratio in 2021 is probably ok, similar for other TBB fleets but wouldnt want to use it to fill in other fleets

# france in 2020 looks too high
g <- ggplot(subset(ld,Country=='France' & DisData),aes(Year,Dis/(Lan+Dis),size=Lan+Dis)) + geom_point() + facet_wrap(~Fleet1)
g
subset(ld,Country=='France' & Fleet1=='OTB_DEF' & Year==2020 & DisData)
subset(ld,Country=='France' & Fleet1=='OTB_DEF' & Year==2019 & DisData)

# ireland 2020 looks too low - fixed with re-upload
g <- ggplot(subset(ld,Country=='Ireland' & DisData),aes(Year,Dis/(Lan+Dis),size=Lan+Dis)) + geom_point() + facet_wrap(~Fleet1)
g
subset(ld,Country=='Ireland' & Fleet1=='OTB_DEF' & Year==2020 & DisData)
subset(ld,Country=='Ireland' & Fleet1=='OTB_DEF' & Year==2019 & DisData)
subset(ld,Country=='Ireland' & Fleet1=='OTB_CRU' & Year==2020 & DisData)
subset(ld,Country=='Ireland' & Fleet1=='OTB_CRU' & Year==2019 & DisData)

a <- subset(ld,Country=='Ireland' & substring(Fleet1,1,3)=='OTB' & Year==2020 & DisData) %>% mutate(p=Dis/(Lan+Dis))
#View(a)


# Fill in missing discard weights
# First calculate mean discard ratio by country, gear and year
a <- subset(ld,DisData) %>% group_by(Country,Fleet1,Year) %>% summarise(DisRatio1=sum(Dis)/sum(Lan))
ggplot(a,aes(Year,DisRatio1,col=paste(Country,Fleet1))) + geom_line() + geom_point() 
# remove outliers - tricky...
a <- subset(a,!(Fleet1=='MIS_MIS'))
a <- subset(a,!(Fleet1=='TBB_DEF' & Country%in%c('Ireland','Belgium'))) # should we leave them in???
a <- subset(a,!(Fleet1%in%c('TBB_DEF','OTB_DEF') & Country=='UK (England)' & Year==2021))
ggplot(a,aes(Year,DisRatio1,col=paste(Country,Fleet1))) + geom_line() + geom_point() 
# still dont believe France, use average previous 5 years - this only affects the fill-ins not the sumbitted data
subset(ld,DisData & Country=='France' & Fleet1=='OTB_DEF' & Year %in% c(2015:2019)) %>% group_by() %>% summarise(DisRatio1=sum(Dis)/sum(Lan))
a[a$Country=='France' & a$Year==2020 & a$Fleet1=='OTB_DEF',]$DisRatio1 <- 0.122

subset(ld,DisData & Country=='France' & Fleet1=='OTB_CRU' & Year %in% c(2016:2020)) %>% group_by() %>% summarise(DisRatio1=sum(Dis)/sum(Lan))
a[a$Country=='France' & a$Year==2021 & a$Fleet1=='OTB_CRU',]$DisRatio1 <- 0.334
a[a$Country=='France' & a$Year==2022 & a$Fleet1=='OTB_CRU',]$DisRatio1 <- 0.334

#subset(ld,DisData & Country=='France' & Fleet1=='OTB_DEF' & Year %in% c(2016:2019,2021)) %>% group_by() %>% summarise(DisRatio1=sum(Dis)/sum(Lan))
#or use all OTB_DEF except FRA to replace FRA 2022 which we dont believe
subset(ld,DisData & Country!='France' & Fleet1=='OTB_DEF' & Year ==2022) %>% group_by() %>% summarise(DisRatio1=sum(Dis)/sum(Lan))
a[a$Country=='France' & a$Year==2022 & a$Fleet1=='OTB_DEF',]$DisRatio1 <- 0.137

#subset(ld,DisData & Country!='France' & Fleet1=='OTB_CRU' & Year ==2022) %>% group_by() %>% summarise(DisRatio1=sum(Dis)/sum(Lan))
#a[a$Country=='France' & a$Year==2022 & a$Fleet1=='OTB_CRU',]$DisRatio1 <- 0.137

subset(ld,DisData & Country=='Ireland' & Fleet1%in%c('OTB_DEF','OTB_CRU') & Year %in% c(2015:2019)) %>% group_by() %>% summarise(DisRatio1=sum(Dis)/sum(Lan))
a[a$Country=='Ireland' & a$Year==2020 & a$Fleet1%in%c('OTB_DEF','OTB_CRU'),]$DisRatio1 <- 0.339

ggplot(a,aes(Year,DisRatio1,col=Fleet1)) + geom_line() + geom_point() + facet_wrap(~Country)
# uk tbb also looks out of scale
subset(ld,DisData & Country=='UK (England)' & Fleet1=='TBB_DEF' & Year %in% c(2015:2019)) %>% group_by() %>% summarise(DisRatio1=sum(Dis)/sum(Lan))
a[a$Country=='UK (England)' & a$Year==2020 & a$Fleet1=='TBB_DEF',]$DisRatio1 <- 0.0732
subset(ld,DisData & Country=='UK (England)' & Fleet1=='OTB_DEF' & Year %in% c(2016:2020)) %>% group_by() %>% summarise(DisRatio1=sum(Dis)/sum(Lan))
#a[a$Country=='UK (England)' & a$Year==2021 & a$Fleet1=='OTB_DEF',]$DisRatio1 <- 0.0307

ggplot(a,aes(Year,DisRatio1,col=Fleet1)) + geom_line() + geom_point() + facet_wrap(~Country)

# 2022 high discards make sense following huge recruitment in 2020

# use the remainder
ggplot(a,aes(Year,DisRatio1,col=Fleet1)) + geom_line() + geom_point() + facet_wrap(~Country)
ld <- left_join(ld,a)

# Next calculate mean discard ratio by gear and year
a <- subset(ld,DisData & Fleet1!='MIS_MIS' & !(Country=='France' & Year==2020)) %>% group_by(Fleet1,Year) %>% summarise(DisRatio2=sum(Dis)/sum(Lan))
ggplot(a,aes(Year,DisRatio2,col=Fleet1)) + geom_line() + geom_point() 
subset(a,Year==2020) # if i had used the other countries' discard ratio for OTB DEF it would have been 0.0201
# 2021 looks out of whack but we did have excessive recrtuitment in 2020, if i remove it, i will just end up filling it in with the overall ratio
# TBB and OTB_CRU are minor enough so it shouldnt affect the overall too much (and remember this is only for filling in unsampled discards)
ld <- left_join(ld,a)

# for fleets that have no discard data at all, use overall ratio per year
a <- subset(ld,DisData) %>% group_by(Year) %>% summarise(DisRatio3=sum(Dis)/sum(Lan))
ggplot(a,aes(Year,DisRatio3)) + geom_line() + geom_point() 
ld <- left_join(ld,a)

ld$DisRatio <- ld$DisRatio1
ld$DisRatio <- ifelse(is.na(ld$DisRatio),ld$DisRatio2,ld$DisRatio)
ld$DisRatio <- ifelse(is.na(ld$DisRatio),ld$DisRatio3,ld$DisRatio)

# so only 2002 should have missing discard ratios:
subset(ld,is.na(DisRatio)) %>% group_by(Year) %>% summarise(NumNA=length(Year))

caton1a <- caton %>% group_by(Country,Year,Season,Area,Fleets,Fleet1,Catch.Cat.,Discards.Imported.Or.Raised) %>%
  summarise(Catch..kg=sum(Catch..kg))
caton1b <- subset(ld,!DisData) %>% group_by(Country,Year,Season,Area,Fleets,Fleet1,Catch.Cat.='Discards',Discards.Imported.Or.Raised='Raised') %>%
  summarise(Catch..kg=sum(Lan*DisRatio))
caton1 <- rbind(caton1a,caton1b)

# keep track of changes in the datasets
sumdat <- data.frame(sumdat,DisFill=summarise.data(caton1,canum))
sumdat


###################################################################################
# STEP 8 - fill in missing sample data
###################################################################################

canum1a <- canum %>% select(Country,Year,Season,Area,Fleets,Fleet1,Catch.Cat.,AgeLength,FrequencyUploaded,
                           Frequency,Weight,Source)

a <- canum1a %>% group_by(Country,Year,Season,Area,Fleets,Catch.Cat.,Source) %>% summarise()
caton1 <- left_join(caton1[,1:9],a,c('Country','Year','Season','Area','Fleets','Catch.Cat.'))
table(caton1$Source,exclude=NULL)

a <- subset(caton1,Catch.Cat.=='Landings') %>% group_by(Year,Country,LanSamData=!is.na(Source)) %>% summarise(Landings=sum(Catch..kg)*1e-6)
g <- ggplot(a,aes(Year,Landings,fill=LanSamData)) + geom_col() + facet_wrap(~Country)
g
png('../LanSamData.png',10,8,'in',res=600); g; dev.off()



caton1 %>% group_by(Catch.Cat.) %>% summarise(PropUnsampled=sum(Catch..kg[is.na(Source)],na.rm=T)/sum(Catch..kg,na.rm=T))


## Assume that each country's sampling is representative of the missing data of that country
# mean lf by country, season and year
a <- canum1a %>% group_by(Year,Season,Country,Catch.Cat.,AgeLength) %>%
   summarise(n=sum(Frequency),w=sum(Frequency*Weight)/sum(Frequency))
a <- a %>% group_by(Year,Season,Country,Catch.Cat.) %>% mutate(SoP=sum(n*w/1000))
# join to caton1
b <- inner_join(subset(caton1,is.na(Source)),a)
canum1b <- with(b,data.frame(Country,Year,Season,Area,Fleets,Fleet1,Catch.Cat.,AgeLength,FrequencyUploaded=0,
                             Frequency=Catch..kg*n/SoP,Weight=w,Source='FillCountry'))

canum1 <- rbind(canum1a,canum1b)
a <- canum1 %>% group_by(Country,Year,Season,Area,Fleets,Catch.Cat.,Source) %>% summarise()
caton1 <- left_join(caton1[,1:9],a,c('Country','Year','Season','Area','Fleets','Catch.Cat.'))
table(caton1$Source,exclude=NULL)

# now for any countries that have no lf data
# mean lf by season and year
a <- canum1a %>% group_by(Year,Season,Catch.Cat.,AgeLength) %>%
   summarise(n=sum(Frequency),w=sum(Frequency*Weight)/sum(Frequency))
a <- a %>% group_by(Year,Season,Catch.Cat.) %>% mutate(SoP=sum(n*w/1000))
# join to caton1
b <- inner_join(subset(caton1,is.na(Source)),a)
canum1c <- with(b,data.frame(Country,Year,Season,Area,Fleets,Fleet1,Catch.Cat.,AgeLength,FrequencyUploaded=0,
                             Frequency=Catch..kg*n/SoP,Weight=w,Source='FillSeason'))

canum1 <- rbind(canum1a,canum1b,canum1c)

a <- canum1 %>% group_by(Country,Year,Season,Area,Fleets,Catch.Cat.,Source) %>% summarise()
caton1 <- left_join(caton1[,1:9],a,c('Country','Year','Season','Area','Fleets','Catch.Cat.'))
table(caton1$Source,exclude=NULL)
subset(canum,Year==2002 & Catch.Cat.=='Discards' & Season==1)
# no discard data at all for Q1 2002 - thats ok, we dont use discards until 2003

# finally mean lf by year
a <- canum1a %>% group_by(Year,Catch.Cat.,AgeLength) %>%
   summarise(n=sum(Frequency),w=sum(Frequency*Weight)/sum(Frequency))
a <- a %>% group_by(Year,Catch.Cat.) %>% mutate(SoP=sum(n*w/1000))
# join to caton1
b <- inner_join(subset(caton1,is.na(Source)),a)
#canum1d <- with(b,data.frame(Country,Year,Season,Area,Fleets,Fleet1,Catch.Cat.,AgeLength,FrequencyUploaded=0,
#                             Frequency=Catch..kg*n/SoP,Weight=w,Source='FillYear'))
# empty

canum1 <- rbind(canum1a,canum1b,canum1c)#,canum1d)

a <- canum1 %>% group_by(Country,Year,Season,Area,Fleets,Catch.Cat.,Source) %>% summarise()
caton1 <- left_join(caton1[,1:9],a,c('Country','Year','Season','Area','Fleets','Catch.Cat.'))
table(caton1$Source,exclude=NULL)


table(caton1$Source,exclude=NULL) # should have no NAs left
table(subset(caton1,is.na(Source))$Year) # they are nearly all in 2002
caton1 <- subset(caton1,!(Year==2002 & Catch.Cat.=='Discards'))
table(caton1$Source,exclude=NULL) # should have no NAs left, but we still have a few...
canum1 <- subset(canum1,Frequency>0) # because zero catches have also been included
subset(caton1,is.na(Source))
# a few logbook discards remain
caton1 <- subset(caton1,Catch.Cat.!='Logbook Registered Discard')
caton1 <- subset(caton1,Catch.Cat.!='BMS landing')
table(caton1$Source,exclude=NULL) # should definitely have no NAs left now


# have a look at how many fill-ins we have

a <- caton1 %>% group_by(Year) %>% summarise(
  LanSamp=sum(Catch..kg[Catch.Cat.=='Landings' & Source=='Imported'])
  ,LanVolume=sum(Catch..kg[Catch.Cat.=='Landings' & Source!='Imported'])
  ,DisSamp=sum(Catch..kg[Catch.Cat.=='Discards' & Source=='Imported'])
  ,DisVolume=sum(Catch..kg[Catch.Cat.=='Discards' & Source!='Imported' & Discards.Imported.Or.Raised=='Imported'])
  ,DisFill=sum(Catch..kg[Catch.Cat.=='Discards' & Source!='Imported' & Discards.Imported.Or.Raised!='Imported'])
  )
png('../Allocations1.png',10,7,'in',res=600)
barplot(t(a[,-1]/1000),legend=T,args.legend=list(x='topleft',inset=c(0.02,-0.15),ncol=2),col=c('blue','#0000FF50','red','#FF000090','#FF000050'),names=a$Year)
dev.off()

a <- caton1 %>% group_by(Country) %>% summarise(
  LanSamp=sum(Catch..kg[Catch.Cat.=='Landings' & Source=='Imported'])
  ,LanVolume=sum(Catch..kg[Catch.Cat.=='Landings' & Source!='Imported'])
  ,DisSamp=sum(Catch..kg[Catch.Cat.=='Discards' & Source=='Imported'],na.rm=T)
  ,DisVolume=sum(Catch..kg[Catch.Cat.=='Discards' & Source!='Imported' & Discards.Imported.Or.Raised=='Imported'],na.rm=T)
  ,DisFill=sum(Catch..kg[Catch.Cat.=='Discards' & Source!='Imported' & Discards.Imported.Or.Raised!='Imported'],na.rm=T)
  )
png('../Allocations2.png',10,7,'in',res=600)
b <- barplot(t(a[,-1]/1000),legend=T,args.legend=list(x='topright',inset=c(0.02,-0.15),ncol=2),col=c('blue','#0000FF50','red','#FF000090','#FF000050'))
text(x=b, y=0, a$Country, srt=45, xpd=T, adj=1.2)
dev.off()


a <- caton1 %>% group_by(Year,Country) %>% summarise(
  LanSamp=sum(Catch..kg[Catch.Cat.=='Landings' & Source=='Imported'])
  ,LanVolume=sum(Catch..kg[Catch.Cat.=='Landings' & Source!='Imported'])
  ,DisSamp=sum(Catch..kg[Catch.Cat.=='Discards' & Source=='Imported'],na.rm=T)
  ,DisVolume=sum(Catch..kg[Catch.Cat.=='Discards' & Source!='Imported' & Discards.Imported.Or.Raised=='Imported'],na.rm=T)
  ,DisFill=sum(Catch..kg[Catch.Cat.=='Discards' & Source!='Imported' & Discards.Imported.Or.Raised!='Imported'],na.rm=T)
  ) %>% pivot_longer(3:7)
a$name <- ordered(a$name,c('LanSamp','LanVolume','DisSamp','DisVolume','DisFill'))

g <- ggplot(a,aes(Country,value/1000,fill=name)) + 
  geom_col(position=position_stack(reverse=T)) + 
  facet_wrap(~Year) + 
  scale_fill_manual(values=c('blue','#0000FF50','red','#FF000090','#FF000050')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g
png('../Allocations3.png',10,7,'in',res=600)
plot(g)
dev.off()


# keep track of changes in the datasets
sumdat <- data.frame(sumdat,SampFill=summarise.data(caton1,canum1))
sumdat


###################################################################################
# STEP 9 - unreported landings
###################################################################################

#spain already taken care of, just need sop correction

# irish area misreporting
# since 2017 this was included in IC so no need to correct after 2016
misrep <- data.frame(Country='Ireland',Catch.Cat.='Landings',Year=2003:2016,p=c(1.058,1.038,1.048,1.046,1.058,1.032,1.042,1.052,1.083,1.063,1.076,1.065,1.038,1.047))
plot(misrep[,3:4],type='b')
caton1a <- left_join(caton1,misrep)
caton1a$p <- ifelse(is.na(caton1a$p),1,caton1a$p)
caton1a$Catch..kgOld <- caton1a$Catch..kg
caton1a$Catch..kg <- caton1a$Catch..kg*caton1a$p
caton1 <- caton1a

# need to do small sop correction - due to this messing with misreporting
# also bigger one for some of the esp data - this is to do with the reporting category, i think its ok
a <- sop(caton1,canum1,Country,Season,Year,Area,Fleet1,Catch.Cat.)
ggplot(a, aes(x=Caton,y=SopR,col=Country)) + geom_point() + facet_wrap(~Catch.Cat.)
subset(a,SopR<0.8) # what is going on here???
table(subset(a,SopR<0.8)$Year)

canum1 <- inner_join(canum1,a)
canum1$Frequency <- canum1$Frequency/canum1$SopR # only run this line once
canum1$Caton <- NULL; canum1$Sop <- NULL; canum1$SopR <- NULL

# keep track of changes in the datasets
sumdat <- data.frame(sumdat,MisRep=summarise.data(caton1,canum1))
sumdat

# look at sumdat, where did each category change in the process
par(mfrow=c(3,3))
for(i in 1:nrow(sumdat)) barplot(as.matrix(sumdat[i,]),main=rownames(sumdat)[i],horiz=T,las=1)
par(mfrow=c(1,1))

###################################################################################
# STEP 10 - final plots
###################################################################################

a <- canum1 %>% group_by(Year,Catch.Cat.,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
b <- data.frame(source='filled',a)

a <- canum0 %>% group_by(Year,Catch.Cat.,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
b <- rbind(b,data.frame(source='raw',a))

g <- ggplot(subset(b,Year>=2002&l<100),aes(l,n*1e-6,col=Catch.Cat.,lty=source)) + geom_line() + facet_wrap(~Year,scales='free_y')
g
png('../LFD.png',10,7,'in',res=600);g;dev.off()

a <- canum1 %>% group_by(Year,Season,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
ggplot(a,aes(l,n,col=factor(Season))) + geom_line() + facet_wrap(~Year,scales='free_y')

a <- subset(canum1,Year==2020) %>% group_by(Year,Country,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
ggplot(a,aes(l,n,col=Country)) + geom_line() 

a <- subset(canum1,Year==2021) %>% group_by(Year,Country,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
ggplot(a,aes(l,n,col=Country)) + geom_line() 

a <- subset(canum1,Year==2022) %>% group_by(Year,Country,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
ggplot(a,aes(l,n,col=Country)) + geom_line() 

a <- subset(canum1,Year==2023) %>% group_by(Year,Country,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
ggplot(a,aes(l,n,col=Country)) + geom_line() 

# size selection differs by fleet.
a <- canum1 %>% group_by(Year,Fleet1,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
b <- a %>% group_by(Year,l) %>% mutate(sumn=sum(n))
a1 <- canum1 %>% group_by(Fleet1,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
b1 <- a1 %>% group_by(l) %>% mutate(sumn=sum(n))
g <- ggplot() + geom_point(data=b,aes(jitter(l),n/sumn,col=Fleet1),alpha=1/4) + geom_line(data=b1,aes(l,n/sumn,col=Fleet1),lwd=1) + xlim(10,100) + xlab('Length (cm)') + ylab('Proportion of catch')
g
png('../Selectivity1.png',10,4,'in',res=600); g; dev.off()


# but catches by fleet are relatively consistent over time
a <- caton1 %>% group_by(Year,Fleet1) %>% summarise(tonnes=sum(Catch..kg)/1000)
b <- a %>% group_by(Year) %>% mutate(tottonnes=sum(tonnes))
g <- ggplot(b,aes(Year,tonnes/tottonnes,col=Fleet1)) + geom_line() + ylab('Proportion of catch')
g
png('../Selectivity2.png',5,4,'in',res=600); g; dev.off()



a <- canum1 %>% group_by(Year,Catch.Cat.,l=floor(AgeLength/20)*2) %>% summarise(n=sum(Frequency))
b <- a %>% spread(Catch.Cat.,n)
b$Discards <- ifelse(is.na(b$Discards),0,b$Discards)
b$Landings <- ifelse(is.na(b$Landings),0,b$Landings)
g <- ggplot(b,aes(l,Landings/(Discards+Landings))) + geom_line() + xlim(10,100) + facet_wrap(~Year)
g
png('../Discards1.png',10,4,'in',res=600); g; dev.off()

out <- NULL
for(y in 2003:max(years)){
  glm1 <- with(subset(b,Year==y & l>15 & l<45),glm(as.matrix(cbind(Discards,Landings))~l,family=binomial))
  L50 <- - glm1$coef[1]/ glm1$coef[2]
  out <- rbind(out,data.frame(Year=y,L50))
}
g <- ggplot(out,aes(Year,L50)) + geom_line() + geom_point()
g
png('../Discards2.png',5,4,'in',res=600); g; dev.off()



###################################################################################
# STEP 11 - save results
###################################################################################

# mixfish need this:
write.csv(caton1,'../caton_raw.csv',row.names=F)
#write.csv(canum1,'../canum.csv',row.names=F)

caton2 <- caton1 %>% group_by(year=Year,country=Country,subArea=substring(Area,1,4),fleet=Fleet1) %>% 
  summarise(landings=sum(Catch..kg[Catch.Cat.=='Landings'])/1000,discards=sum(Catch..kg[Catch.Cat.=='Discards'])/1000)
write.csv(caton2,'../caton_summary.csv',row.names=F)

# For WGMIXFISH - 
a <- canum %>% mutate(Frequency=round(Frequency,2))
write.csv(a,"../ANK_canum_raw.csv",row.names=F)

canum2 <- canum1 %>% group_by(year=Year,country=Country,subArea=substring(Area,1,4),catchCat=Catch.Cat.,fleet=Fleet1,lenCm=AgeLength/10) %>% 
  summarise(frequency1000=sum(Frequency),meanWeightKg=sum(Weight*Frequency)/sum(Frequency)/1000)
write.csv(canum2,'../canum_summary.csv',row.names=F)

caton3 <- caton1 %>% group_by(year=Year) %>% 
  summarise(landings=sum(Catch..kg[Catch.Cat.=='Landings'])/1000,discards=sum(Catch..kg[Catch.Cat.=='Discards'])/1000)
write.csv(caton3,'../ank78_caton.csv',row.names=F)

### done

