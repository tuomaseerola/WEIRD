#setwd("/Users/wxbn79/Dropbox/2023_WEIRD/data")

#d <- read.csv("WEIRD part cleaned Feb24.csv")
####read in via contents.R and run preprocess script first
##descriptive stats###################################################

length(unique(d$PaperTitle))
#sort(table(weird$PaperTitle), decreasing = TRUE)[1:30]


###number of studies per paper
t <- table(d$PaperTitle)
t2 <- as.numeric(table(d$PaperTitle))
table(t2)/1360

###articles only 
articles <- d[!duplicated(d$PaperTitle), ]

sort(table(articles$FirstAuthorCountry))/1360*100
table(articles$FirstAuthorCountry_WEOG)/1360*100

###studies with human participants
framelen <- length(d$PaperTitle)
#((table(is.na(weird$SampleSize))+2)/framelen)*100 
#humanstudies <- as.numeric((table(is.na(weird$SampleSize))+2)[1])###added 2 studies without reported sample size

#d$humansample <- FALSE
#d$humansample[!is.na(d$SampleSize)] <- TRUE
#sum(d$humansample) # 1524
#sum(is.na(d$SampleSize) & !is.na(d$SampleAgeMean)) #now zero
#ind1<-which(is.na(d$SampleSize) & !is.na(d$SampleOtherDescription))
#ind2 <- which(is.na(d$SampleSize) & !is.na(d$SamplingMethodDescription))
#ind3 <- which(is.na(d$SampleSize) & !is.na(d$SampleMusicianshipDescription))
#ind4 <- unique(c(ind1, ind2, ind3))
#d[ind4,]

#d$humansample[ind4]<-TRUE #sum is now 1532
#table(is.na(weird$CountryDataCollected))
humanstudies <- as.numeric(table(d$humansample))[2]
###country data collected
country_collected <- strsplit(d$CountryDataCollected, '; ')
c2 <- NULL
for (i in 1:length(country_collected)){
  c1 <- country_collected[i][[1]]
  c2 <- c(c2,c1)
}

sort(table(c2))/humanstudies*100

###country of origin of participants
table(is.na(d$CountryDataCollected))/humanstudies*100
table(is.na(d$SamplePrimaryCountryofOrigin))/humanstudies*100

country_origin <- strsplit(d$SamplePrimaryCountryofOrigin, '; ')
co <- NULL
for (i in 1:length(country_origin)){
  c1 <- country_origin[i][[1]]
  co <- c(co,c1)
}

sort(table(co))/humanstudies*100

####sample size
median(df$SampleSize, na.rm=TRUE)
range(df$SampleSize, na.rm=TRUE)

ss2 <- NULL
for (i in 1:length(d$SampleSize)){
  if(is.na(d$SampleSize[i])==TRUE){s1 <- NA}
  if(is.na(d$SampleSize[i])==FALSE){
    if(grepl(";", d$SampleSize[i])==TRUE){
     s <- strsplit(d$SampleSize[i], '; ')
     s1 <- as.numeric(s[[1]][1])+as.numeric(s[[1]][2])}
   if(grepl(";", d$SampleSize[i])==FALSE){
    s1 <- as.numeric(d$SampleSize[i])
    }
  }
  ss2 <- c(ss2, s1)
}

hist(ss2)
mean(ss2, na.rm=TRUE)
median(ss2, na.rm=TRUE)

table(ss2 <100)/humanstudies


####age
mean(df$sample_agemean, na.rm=TRUE)
table(df$sample_agemean >= 18 & df$sample_agemean <= 30)/sum(table(df$sample_agemean > 17 & df$sample_agemean < 30))

##sample musicianship
nas <- as.numeric(table(is.na(weird$SampleMusicianshipDescription))[2])-(framelen-humanstudies)
nas/humanstudies*100
table(weird$SampleMusicianshipDescription)/humanstudies*100


##sampling method
table(weird$SamplingMethodDescription)/humanstudies*100
nas <- as.numeric(table(is.na(weird$SamplingMethodDescription))[2])-(framelen-humanstudies)
nas/humanstudies*100
#table(is.na(weird$SamplingMethodDescription))/humanstudies*100

###sample description
sort(table(weird$SampleOtherDescription), decreasing = TRUE)/humanstudies*100
nas <- as.numeric(table(is.na(weird$SampleOtherDescription))[2])-(framelen-humanstudies)
nas/humanstudies*100
6.82+1.05+0.45+ 0.39+0.20+0.066 ###children or infants

###ethnicity and education
nas <- as.numeric(table(is.na(weird$Ethnicity))[2])-(framelen-humanstudies)
nas/humanstudies*100
#table(is.na(weird$Ethnicity))/humanstudies*100
table(weird$Ethnicity)/sum(table(weird$Ethnicity))*100

nas <- as.numeric(table(is.na(weird$MeanYearsEducation))[2])-(framelen-humanstudies)
nas/humanstudies*100
#table(is.na(weird$MeanYearsEducation))/humanstudies*100

ed <- strsplit(weird$MeanYearsEducation, '; ')
e2 <- NULL
for (i in 1:length(ed)){
  e1 <- ed[i][[1]]
  e2 <- c(e2,e1)
}

e2 <- as.numeric(e2)
mean(e2, na.rm=TRUE)


###music stimuli

table(is.na(weird$MusicSource))/framelen*100
num <- as.numeric(table(is.na(weird$MusicSource))[1])
table(weird$MusicSource)/num*100

table(is.na(weird$MusicSource))
sort(table(weird$MusicOriginCountry))/num*100

weird_westmusic <- weird[which(grepl("Western", weird$MusicOriginCountry)==TRUE),]

west.genres <- strsplit(weird_westmusic$MusicPrimaryGenre, '; ')
c2 <- NULL
for (i in 1:length(west.genres)){
  c1 <- west.genres[i][[1]]
  c2 <- c(c2,c1)
}
table(c2)
sort(table(c2), decreasing = TRUE)/(length(weird_westmusic[,1]))*100

all.genres <- strsplit(weird$MusicPrimaryGenre, '; ')
c3 <- NULL
for (i in 1:length(all.genres)){
  c1 <- all.genres[i][[1]]
  c3 <- c(c3,c1)
}
table(c3)
sort(table(c3), decreasing = TRUE)/num*100


