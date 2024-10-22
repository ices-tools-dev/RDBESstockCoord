

# function to read the 'StockOverview.txt' file(s)
read.caton <- function(files){
  caton <- NULL
  for(f in files){
    cat(f,'\n')
    caton0 <- read.delim(f)
    caton <- rbind(caton,caton0)
  }
  caton$Report.cat. <- substring(caton$Report.cat.,1,15)
  i <- duplicated(select(caton,Country,Season,Season.type,Year,Area,Fleets,Catch.Cat.,Report.cat.))
  if(sum(i)>0) warning('It looks like you have some duplicated values')
  return(caton)
}

# function to read the 'Numbers_at_age_and_mean_weights_at_age.zip' file(s)
read.canum <- function(files,type='canum'){
  canum <- NULL
  for(f in files){
    cat(f,'\n')
    num0 <- read.delim(unz(f,'NumbersAtAgeLength.txt'),skip=2)
    wgt0 <- read.delim(unz(f,'MeanWeigthAtAgeLength.txt'),skip=1)
    i <- grep('UndeterminedLngt|MaleLngt|FemaleLngt',names(num0))
    num1 <- num0[,c(1:15,i)] %>% gather('AgeLength','Frequency',i)
    i <- grep('UndeterminedLngt|MaleLngt|FemaleLngt',names(wgt0))
    wgt1 <- wgt0[,c(1:15,i)] %>% gather('AgeLength','Weight',i)
    # Catch.Cat. in numbersatlength file is inconsistent
    num1$Catch.Cat. <- ifelse(num1$Catch.Cat.=='L','Landings',as.character(num1$Catch.Cat.))
    num1$Catch.Cat. <- ifelse(num1$Catch.Cat.=='D','Discards',as.character(num1$Catch.Cat.))
    num1$Catch.Cat. <- factor(num1$Catch.Cat.)
    canum0 <- inner_join(num1,wgt1,by=rev(names(wgt1))[-1])
    if(nrow(canum0)!=nrow(num1)) warning('Someting went wrong in the join of numbers and weights')
    canum0 <- subset(canum0,Frequency>0)
    canum <- rbind(canum,canum0)
  }
    canum$Sex <- gsub('[0-9]','',canum$AgeLength)
    canum <- canum %>% mutate(AgeLength = as.numeric(sub('UndeterminedLngt|MaleLngt|FemaleLngt','',AgeLength)))
  if(type=='canum') {
    canum.out <- select(canum,c(Country,Season,Year,Stock,Area,Fleets,Catch.Cat.,Report.cat.,Sex,AgeLength,Frequency,Weight))
    canum.out$Source='Imported'
    i <- duplicated(canum.out[,1:10])
    if(sum(i)>0) warning('It looks like you have some duplicated values')
    return(canum.out)
  }
  if(type=='sampnum') {
    sampnum <- select(canum,c(Country,Season,Year,Stock,Area,Fleets,Catch.Cat.,Report.cat.,NumAgeMeasurement,NumLengthMeasurements,NumSamplesAge,NumSamplesLength))
    i <- duplicated(sampnum)
    return(sampnum[!i,])
  }
}

read.sampnum <- function(files) read.canum(files,type='sampnum')

# summarise the available data
summarise.data <- function(caton,canum){
  c(CatWt=sum(caton$Catch..kg,na.rm=T)/1000
    ,LanWt=sum(subset(caton,Catch.Cat.=='Landings')$Catch..kg,na.rm=T)/1000
    ,DisWtImported=sum(subset(caton,Catch.Cat.=='Discards'& Discards.Imported.Or.Raised=='Imported')$Catch..kg,na.rm=T)/1000
    ,DisWtFilled=sum(subset(caton,Catch.Cat.=='Discards'& Discards.Imported.Or.Raised!='Imported')$Catch..kg,na.rm=T)/1000
    ,CaNum=sum(canum$Frequency,na.rm=T)/1000
    ,LanNumImported=sum(subset(canum,Catch.Cat.=='Landings' & Source=='Imported')$Frequency,na.rm=T)/1000
    ,LanNumFilled=sum(subset(canum,Catch.Cat.=='Landings' & Source!='Imported')$Frequency,na.rm=T)/1000
    ,DisNumImported=sum(subset(canum,Catch.Cat.=='Discards' & Source=='Imported')$Frequency,na.rm=T)/1000
    ,DisNumFilled=sum(subset(canum,Catch.Cat.=='Discards' & Source!='Imported')$Frequency,na.rm=T)/1000
    )
}

# sum of products, ... is the group_by argument
sop <- function(caton,canum,...){
  a <- caton %>% group_by(...) %>% summarise(Caton=sum(Catch..kg)*1e-3)
  b <- canum %>% group_by(...) %>% summarise(Sop=sum(Frequency*Weight)*1e-6)
  c <- inner_join(a,b)
  c$SopR <- c$Sop/c$Caton
  return(c)
}


# sum of products, ... is the group_by argument
landis <- function(caton,...){
  a <- caton %>% group_by(...) %>%
    summarise(Lan=sum(Catch..kg[Catch.Cat.=='Landings'])
            ,Dis=sum(Catch..kg[Catch.Cat.=='Discards'])
            ,DisData=sum(Catch.Cat.=='Discards')>0)
  return(a)
  }


# output functions
write.catch.helper <- function(filename,name,description,header,x) {
  write.table(name,filename,sep=',',quote=F,row.names=F,col.names=F)
  write.table(description,filename,append=T,quote=F,sep=',',row.names=F,col.names=F)
  write.table('',filename,append=T,quote=F,sep=',',row.names=F,col.names=F)
  write.table(t(header),filename,append=T,quote=F,sep=',',row.names=T,col.names=F)
  write.table('',filename,append=T,quote=F,sep=',',row.names=F,col.names=F)
  write.table(x,filename,append=T,quote=F,sep=',',row.names=F,col.names=T,na='')
}  

write.catch.length <- function(caton,canum,filenamestart,stock){
  filename <- paste0(filenamestart,'_landings_n.csv')
  name <- 'landings.n'
  description <- paste0('Landings numbers at length (thousands); ',stock)
  header <- caton1 %>% group_by(year=Year,quarter=Season) %>% summarise()
  x <- subset(canum1, Catch.Cat.=='Landings') %>% group_by(time=paste(Year,Season,sep='_'),length=AgeLength/10) %>% summarise(landings.n=sum(Frequency)) %>% spread(time,landings.n)
  x <- full_join(x,data.frame(length=1:200))
  x <- x[order(x$length),]
  x[is.na(x)] <- 0
  write.catch.helper(filename,name,description,header,x)
  
  filename <- paste0(filenamestart,'_landings_wt.csv')
  name <- 'landings.wt'
  description <- paste0('Landings weight at length (kg); ',stock)
  x <- subset(canum1, Catch.Cat.=='Landings') %>% group_by(time=paste(Year,Season,sep='_'),length=AgeLength/10) %>% summarise(landings.wt=sum(Weight*Frequency)/sum(Frequency)/1000) %>% spread(time,landings.wt)
  x <- full_join(x,data.frame(length=1:200))
  x <- x[order(x$length),]
  write.catch.helper(filename,name,description,header,x)

  filename <- paste0(filenamestart,'_discards_n.csv')
  name <- 'discards.n'
  description <- paste0('Discards numbers at length (thousands); ',stock)
  x <- subset(canum1, Catch.Cat.=='Discards') %>% group_by(time=paste(Year,Season,sep='_'),length=AgeLength/10) %>% summarise(landings.n=sum(Frequency)) %>% spread(time,landings.n)
  x <- full_join(x,data.frame(length=1:200))
  x <- x[order(x$length),]
  x[is.na(x)] <- 0
  write.catch.helper(filename,name,description,header,x)
  
  filename <- paste0(filenamestart,'_discards_wt.csv')
  name <- 'discards.wt'
  description <- paste0('Discards weight at length (kg); ',stock)
  x <- subset(canum1, Catch.Cat.=='Discards') %>% group_by(time=paste(Year,Season,sep='_'),length=AgeLength/10) %>% summarise(landings.wt=sum(Weight*Frequency)/sum(Frequency)/1000) %>% spread(time,landings.wt)
  x <- full_join(x,data.frame(length=1:200))
  x <- x[order(x$length),]
  write.catch.helper(filename,name,description,header,x)

}
