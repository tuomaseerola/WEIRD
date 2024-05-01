# preprocess.R
# WEIRD article
# T. Eerola, 23/3/2024
# Status: Requires more work

cat("Preprocessing.....")

cat(paste('\nNumber of coded studies:',nrow(d)))
cat(paste('\nUnique first authors:',length(unique(d$FirstAuthorName_cleaned))))
t<-table(d$FirstAuthorName_cleaned)
t<-dplyr::arrange(data.frame(t),-Freq)
#head(t,15)


#### 1. Create Human studies index -------------
cat("\n1. Studies with samples (human studies)")

d$humansample <- FALSE
d$humansample[!is.na(d$SampleSize)  ] <- TRUE
sum(d$humansample)

ind1<-which(is.na(d$SampleSize) & !is.na(d$SampleOtherDescription))
ind2 <- which(is.na(d$SampleSize) & !is.na(d$SamplingMethodDescription))
ind3 <- which(is.na(d$SampleSize) & !is.na(d$SampleMusicianshipDescription))
ind4 <- unique(c(ind1, ind2, ind3))
d[ind4,]

d$humansample[ind4]<-TRUE #sum is now 1532
sum(d$humansample) # indeed

#### 2. Create index for studies with music: 1047, MusicSource column (not NA) -------
cat("\n2. Studies with music (music studies)")
d$musicstudies<-FALSE
d$musicstudies[!is.na(d$MusicSource)]<-TRUE
sum(d$musicstudies)

#### 3. Create paper id from author+year --------
cat("\n3. Create paper_ids \n")

tmp <- str_replace(d$FirstAuthorName_cleaned,',.*','')
tmp <- str_replace(tmp,' ','')
tmp <- str_replace(tmp,'-.*','')
tmp <- str_replace(tmp,'_.*','')
tmp <- str_replace(tmp,' .*','')
tmp <- str_replace(tmp,'\\..*','')
tmp <- paste0(tolower(stringi::stri_trans_general(tmp,"latin-ascii")),d$Year,tolower(stringr::str_extract(stringi::stri_trans_general(d$PaperTitle,"latin-ascii"), "^.{2}")))
d$paper_id <-tmp

# 2 special cases of the same author, same year, same title beginning: granot2011mu_study1_sample0
d$paper_id[d$paper_id == 'granot2011mu'] <- paste0(d$paper_id[d$paper_id=='granot2011mu'],c('a','b'))
d$paper_id[d$paper_id == 'yeoh2010th'] <- paste0(d$paper_id[d$paper_id=='yeoh2010th'],c('a','b','c'))

cat(paste('...Unique studies:',length(unique(d$paper_id))))
t<-table(d$paper_id)
t<-dplyr::arrange(data.frame(t),-Freq)
head(t,15)

#### 4. Add study ids--------
cat("\n4. Add study ids")

library(dplyr)
d <- d %>%
  group_by(paper_id) %>%
  mutate(study_id = paste0("study", row_number()))

d$paper_id_study_id <- paste0(d$paper_id,'_',d$study_id)
length(unique(d$paper_id_study_id)) # 1622

#### 4. Recode some of the studies with multiple sample qualities (musicians/non-musicians)


#### 5. Expand study_ids into separate study samples -----------
cat("\n5. Expand into samples")
cat('....Loop across papers with multiple studies to get multiple samples (;)\n')

#pb = txtProgressBar(min = 0, max = nrow(d), initial = 0, style = 3) 

SAMPLE <- NULL
for (k in 1:nrow(d)) {
#  setTxtProgressBar(pb,k)
  new_row <- d[k,]
  new_row$sample_id<-'sample0'
  new_row$sample_size<-NA
  new_row$sample_agemean<-NA
  new_row$sample_agesd<-NA
  new_row$sample_female<-NA
  new_row$sample_male<-NA
  new_row$country_data_collected<-NA
  new_row$sample_ethnicity<-NA
  new_row$sample_musicianship_description<-NA
  new_row$sample_sampling_method_description<-NA
  new_row$sample_other_desciption<-NA  
  new_row$sample_primary_country<-NA  
  
  # expand more: Ethnicity, SampleMusicianshipDescription, SamplingMethodDescription, SampleOtherDesciption
  
  tmp<-d$SampleSize[k]
  tmpAGEM<-d$SampleAgeMean[k]
  tmpAGESD<-d$SampleAgeSD[k]
  tmpFEMALE<-d$FemaleParticipantsNumber[k]
  tmpMALE<-d$MaleParticipantsNumber[k]
  tmpCOUNTRY<-d$CountryDataCollected[k]
  tmpCOUNTRYORIGIN<-d$SamplePrimaryCountryofOrigin[k] # new
  
  tmpETHNICITY<-d$Ethnicity[k]
  tmpMUSICIANSHIP<-d$SampleMusicianshipDescription[k]
  tmpSAMPLINGMETHOD<-d$SamplingMethodDescription[k]
  tmpOTHER<-d$SampleOtherDescription[k]
  
  # instead of tmp with only sample size, combine all relevant columns
  
  if(!is.na(tmp)){ # Skip missing data
    
    if(str_detect(tmp,';')){
      #if(str_detect(paste(tmp,tmpCOUNTRYORIGIN),';')){
      #print("multiple samples detected")
      tmp_split <- str_split(tmp,';'); tmp_split<-tmp_split[[1]]
      tmpAGEM_split <- str_split(tmpAGEM,';'); tmpAGEM_split<-tmpAGEM_split[[1]]
      tmpAGESD_split <- str_split(tmpAGESD,';'); tmpAGESD_split<-tmpAGESD_split[[1]]
      tmpFEMALE_split <- str_split(tmpFEMALE,';'); tmpFEMALE_split<-tmpFEMALE_split[[1]]
      tmpMALE_split <- str_split(tmpMALE,';'); tmpMALE_split<-tmpMALE_split[[1]]
      tmpCOUNTRY_split <- str_split(tmpCOUNTRY,';'); tmpCOUNTRY_split<-tmpCOUNTRY_split[[1]]
      tmpCOUNTRYORIGIN_split <- str_split(tmpCOUNTRYORIGIN,';'); tmpCOUNTRYORIGIN_split<-tmpCOUNTRYORIGIN_split[[1]]
      
      tmpETHNICITY_split <- str_split(tmpETHNICITY,';'); tmpETHNICITY_split<-tmpETHNICITY_split[[1]]
      tmpMUSICIANSHIP_split <- str_split(tmpMUSICIANSHIP,';'); tmpMUSICIANSHIP_split<-tmpMUSICIANSHIP_split[[1]]
      tmpSAMPLINGMETHOD_split <- str_split(tmpSAMPLINGMETHOD,';'); tmpSAMPLINGMETHOD_split<-tmpSAMPLINGMETHOD_split[[1]]
      tmpOTHER_split <- str_split(tmpOTHER,';'); tmpOTHER_split<-tmpOTHER_split[[1]]
      
      for (l in 1:length(tmp_split)) {
        sample_id = paste0("sample", l) #
        #      new_row<-d[k,]
        new_row$sample_id<-sample_id
        new_row$sample_size<-tmp_split[l]
        new_row$sample_agemean<-tmpAGEM_split[l]
        new_row$sample_agesd<-tmpAGESD_split[l]
        new_row$sample_female<-tmpFEMALE_split[l]
        new_row$sample_male<-tmpMALE_split[l]
        new_row$sample_country_data_collected<-tmpCOUNTRY_split[l]
        new_row$sample_primary_country_of_origin<-tmpCOUNTRYORIGIN_split[l]
        new_row$sample_ethnicity<-tmpETHNICITY_split[l]
        new_row$sample_musicianship_description<-tmpMUSICIANSHIP_split[l]
        #      new_row$sample_sampling_method_description<-tmpSAMPLINGMETHOD_split[l]
        #     new_row$sample_other_desciption<-tmpOTHER_split[l]
        #new_row[is.na(new_row)] <- NA # Cannot be assumed if other columns have variable lengths
        SAMPLE<-rbind(SAMPLE,new_row)
      }
    }
    # else{
    #   SAMPLE<-rbind(SAMPLE,new_row)
    # }
  }
}
#close(pb)

#### 6. Clean variables --------
cat("6. Clean variables \n")

# clean strings: Sample size
tmp <- str_replace_all(SAMPLE$sample_size,' ','')
SAMPLE$sample_size <- as.numeric(tmp)

# clean strings: Age (NOTE THIS IS WAY TOO MESSY TO RESOLVE)
# age in months needs to be converted
tmp<-str_replace_all(SAMPLE$sample_agemean,' ','')
SAMPLE$sample_agemean <- suppressWarnings(as.numeric(tmp))
SAMPLE$sample_agemean
# clean strings: AgeSD
tmp<-str_replace_all(SAMPLE$sample_agesd,' ','')
SAMPLE$sample_agesd <- suppressWarnings(as.numeric(tmp))

# clean strings: sample_female
tmp <- str_replace_all(SAMPLE$sample_female,' ','')
SAMPLE$sample_female <- suppressWarnings(as.numeric(tmp))

# clean strings: sample_male
tmp <- str_replace_all(SAMPLE$sample_male,' ','')
SAMPLE$sample_male <- suppressWarnings(as.numeric(tmp)) # Not clean yet

# clean strings: COUNTRY
tmp<-str_replace_all(SAMPLE$sample_country_data_collected,'^ ','')
SAMPLE$sample_country_data_collected <- tmp

# clean strings: SAMPLING METHODS
#tmp<-str_replace_all(SAMPLE$sample_sampling_method_description,'^ ','')
#SAMPLE$sample_sampling_method_description <- tmp

#### Treat singular samples also similarly (prune and clean)
# SampleSize
tmp<-str_replace_all(d$SampleSize,' ','')
tmp<-str_replace_all(tmp,'^.*;.*$','')
tmp<-str_replace_all(tmp,'[A-Za-z]','')
tmp<-str_replace_all(tmp,'\\(','')
tmp<-str_replace_all(tmp,'\\)','')
tmp<-str_replace_all(tmp,' ','')
tmp<-str_replace_all(tmp,',','')
d$SampleSize<-as.numeric(tmp)

#### Treat singular samples also similarly (prune and clean)
# SampleAgeMean
tmp<-str_replace_all(d$SampleAgeMean,' ','')
tmp<-str_replace_all(tmp,'^.*;.*$','NA')
tmp<-str_replace_all(tmp,'[A-Za-z]','')
tmp<-str_replace_all(tmp,'\\(','')
tmp<-str_replace_all(tmp,'\\)','')
tmp<-str_replace_all(tmp,' ','')
tmp<-str_replace_all(tmp,',','')
tmp<-str_replace_all(tmp,'-.*^','') # months into years
d$SampleAgeMean<-as.numeric(tmp)

#### Treat singular samples also similarly (prune and clean)
# SampleAgeSD
tmp<-str_replace_all(d$SampleAgeSD,' ','')
tmp<-str_replace_all(tmp,'^.*;.*$','NA')
tmp<-str_replace_all(tmp,'[A-Za-z]','')
tmp<-str_replace_all(tmp,'\\(','')
tmp<-str_replace_all(tmp,'\\)','')
tmp<-str_replace_all(tmp,' ','')
#tmp<-str_replace_all(tmp,',','')
tmp<-str_replace_all(tmp,'-.*^','') #
d$SampleAgeSD<-suppressWarnings(as.numeric(tmp))

#### Treat singular samples also similarly (prune and clean)
# FemaleParticipantsNumber
tmp<-str_replace_all(d$FemaleParticipantsNumber,' ','')
tmp<-str_replace_all(tmp,'^.*;.*$','NA')
tmp<-str_replace_all(tmp,'[A-Za-z]','')
tmp<-str_replace_all(tmp,'\\(','')
tmp<-str_replace_all(tmp,'\\)','')
tmp<-str_replace_all(tmp,' ','')
d$FemaleParticipantsNumber<-as.numeric(tmp)

#### Treat singular samples also similarly (prune and clean)
# MaleParticipantsNumber
tmp<-str_replace_all(d$MaleParticipantsNumber,' ','')
tmp<-str_replace_all(tmp,'^.*;.*$','NA')
tmp<-str_replace_all(tmp,'[A-Za-z]','')
tmp<-str_replace_all(tmp,'\\(','')
tmp<-str_replace_all(tmp,'\\)','')
tmp<-str_replace_all(tmp,' ','')
d$MaleParticipantsNumber<-as.numeric(tmp)

#### Treat singular samples also similarly (prune and clean)
# CountryDataCollected
tmp<-str_replace_all(d$CountryDataCollected,'^ ','')
tmp<-str_replace_all(tmp,'^.*;.*$','NA')
d$CountryDataCollected<-tmp
# SamplePrimaryCountryofOrigin
tmp<-str_replace_all(d$SamplePrimaryCountryofOrigin,'^ ','')
#tmp<-str_replace_all(tmp,'^.*;.*$','NA') # critical link here
d$SamplePrimaryCountryofOrigin<-tmp


d$sample_id<-'sample0'
d$sample_size<-d$SampleSize
d$sample_agemean<-d$SampleAgeMean
d$sample_agesd<-d$SampleAgeSD
d$sample_female<-d$FemaleParticipantsNumber
d$sample_male<-d$MaleParticipantsNumber
d$sample_country_data_collected<-d$CountryDataCollected
d$sample_sampling_method_description<-d$SamplingMethodDescription #ERROR?

# Blank the entries for subsampling so they are not counted
SAMPLE$SampleSize<-NA
SAMPLE$SampleAgeMean<-NA
SAMPLE$SampleAgeSD<-NA
SAMPLE$FemaleParticipantsNumber<-NA
SAMPLE$MaleParticipantsNumber<-NA
SAMPLE$CountryDataCollected<-NA
#SAMPLE$SamplingMethodDescription<-'x4'

tmp<-str_replace_all(SAMPLE$SamplePrimaryCountryofOrigin,'^ ','')
tmp<-str_replace_all(tmp,'^.*;.*$','')
SAMPLE$SamplePrimaryCountryofOrigin<-tmp
table(SAMPLE$SamplePrimaryCountryofOrigin)
SAMPLE$SamplePrimaryCountryofOrigin

#### 7. Combine and sort actual and expanded --------
cat("7. Combine expanded and original data \n")

d2 <- rbind(SAMPLE,d)
#table(d2$sample_id)

#table(d2$SamplePrimaryCountryofOrigin)

#table(d2$sample_country_data_collected)

df <- dplyr::arrange(d2,paper_id,study_id,sample_id)
#names(df)
#head(df[,c(24:28,33)],15)

#### 8. If online, use the primary data collected -------
cat("8. Convert online studies based on SamplePrimaryCountryofOrigin")
online_convert<-TRUE
if(online_convert==TRUE){
  df$SamplePrimaryCountryofOrigin_online_inferred<-df$SamplePrimaryCountryofOrigin
  cat("\n6. Handle online origin\n")
  ind <- df$sample_country_data_collected == 'online' & !is.na(df$sample_country_data_collected) # 148 cases
  table(df$SamplePrimaryCountryofOrigin[ind])
  tmp<-df$SamplePrimaryCountryofOrigin[ind]
  table(tmp)
  # for WEOG purposes, simplify the ones that are clearly from two WEOG countries
  tmp <- str_replace_all(tmp,'North America; Europe','US')
  tmp <- str_replace_all(tmp,'Finland; UK','Finland')
  tmp <- str_replace_all(tmp,'Australia; China','')
  tmp <- str_replace_all(tmp,'Germany; Taiwan','')
  tmp <- str_replace_all(tmp,'UK; Singapore','')
  tmp <- str_replace_all(tmp,'US; Jamaica','')
  tmp <- str_replace_all(tmp,'online','')
  tmp <- str_replace_all(tmp,'Europe','UK')
  tmp <- str_replace_all(tmp,'Europe','UK')
  tmp <- str_replace_all(tmp,'Armenia; Azerbaijan','Armenia')
  table(tmp)
  df$SamplePrimaryCountryofOrigin_online_inferred[ind] <- tmp
  
  # same for papers
  d$SamplePrimaryCountryofOrigin_online_inferred <- d$SamplePrimaryCountryofOrigin
  ind <- d$sample_country_data_collected == 'online' # 139 cases
  ind[is.na(ind)]<-FALSE
  table(d$SamplePrimaryCountryofOrigin[ind])
  tmp<-d$SamplePrimaryCountryofOrigin[ind]
  table(tmp)
  # for WEOG purposes, simplify the ones that are clearly from two WEOG countries
  tmp <- str_replace_all(tmp,'North America; Europe','US')
  tmp <- str_replace_all(tmp,'Finland; UK','Finland')
  tmp <- str_replace_all(tmp,'Australia; China','')
  tmp <- str_replace_all(tmp,'Germany; Taiwan','')
  tmp <- str_replace_all(tmp,'UK; Singapore','')
  tmp <- str_replace_all(tmp,'US; Jamaica','')
  tmp <- str_replace_all(tmp,'online','')
  tmp <- str_replace_all(tmp,'Europe','UK')
  tmp <- str_replace_all(tmp,'Europe','UK')
  tmp <- str_replace_all(tmp,'Armenia; Azerbaijan','Armenia')
  table(tmp)
  d$SamplePrimaryCountryofOrigin_online_inferred[ind] <- tmp
  table(d$SamplePrimaryCountryofOrigin_online_inferred)
  table(df$SamplePrimaryCountryofOrigin,df$SamplePrimaryCountryofOrigin_online_inferred)
}

# 9. remove sample0 from those which have sub samples
#cat("9. Trim duplicate sample0\n")

#print(paste('Unique samples:',sum((!is.na(df$sample_size)))))

# remove those rows that have sample0 AND sample_size==NA but 
#dim(df)
#df2 <- df %>% filter(!is.na(sample_size) | !sample_id=='sample0')
#dim(df2)
#head(df[,24:28],10)
#head(df2[,24:28],10)

#df2$sample_size

#### 9. Create Unique Identifier --------------
cat("9. Create UIDs\n")

df$UID <- paste0(df$paper_id,'_',df$study_id,'_',df$sample_id)

length(unique(df$paper_id))  # 1360
length(unique(df$study_id))  # 8
length(unique(df$sample_id)) # 6
length(unique(df$UID))       # 1984

# check for duplicates?
t<-table(df$UID)
t<-dplyr::arrange(data.frame(t),-Freq)
head(t,5)


#### 10. Gender balance  --------------------
cat("10. Gender balance indicators\n")

df$sample_gender_balance <- df$sample_female / (df$sample_female+df$sample_male)
d$gender_balance <- d$FemaleParticipantsNumber / (d$FemaleParticipantsNumber+d$MaleParticipantsNumber)
d$SampleSizeAggregated <- d$FemaleParticipantsNumber+d$MaleParticipantsNumber

#### 11. WEIRD index  --------------------
cat("11. WERID indicator for countries\n")

WEIRD_country_index <- "Krys"
# options are: "Krys", "WEOG" or "Muthukhrisna"

# Put in some fixes
d$CountryDataCollected[d$CountryDataCollected=='North America']<-'US'
#d$CountryDataCollected[d$CountryDataCollected=='Europe']<-'UK'

cat(paste0('Using WEIRD_country_index: ',WEIRD_country_index))
if (WEIRD_country_index=='WEOG'){
  #### WEOG country variable 
  WEOG_countries<-c("Andorra","Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany","Greece","Iceland","Ireland","Israel","Italy","Liechtenstein","Luxembourg","Malta","Monaco","Netherlands","New Zealand","Norway","Portugal","San Marino","Spain","Sweden","Switzerland","Turkey","UK","US","Vatican City","Europe","North America")
  # Note that Hungary and Poland, Romania, Serbia are not part of the WEOS!
  
  # which countries do not match!
  table(df$CountryDataCollected[!df$CountryDataCollected %in% WEOG_countries])
  # Czech Republic Estonia Europe Latvia North America Poland Romania Serbia online (139 of them)
  # Fix North America and online
  df$CountryDataCollected[df$CountryDataCollected=='North America']<-'US'
  sum(df$CountryDataCollected=='online',na.rm = T)
  index<-df$CountryDataCollected=='online'
  index[is.na(index)]<-FALSE
  df$CountryDataCollected[index]<-df$SamplePrimaryCountryofOrigin_online_inferred[index]
  df$sample_country_data_collected[index]<-df$SamplePrimaryCountryofOrigin_online_inferred[index]
  
  table(df$CountryDataCollected)
  sum(is.na(df$CountryDataCollected))
  df$CountryDataCollected_WEOG <- NA
  df$CountryDataCollected_WEOG[!is.na(df$CountryDataCollected)] <- 'Non-WEOG'
  df$CountryDataCollected_WEOG[df$CountryDataCollected %in% WEOG_countries]<- 'WEOG'
  sum(is.na(df$CountryDataCollected_WEOG))
  table(df$CountryDataCollected_WEOG)
  table(df$CountryDataCollected,df$CountryDataCollected_WEOG)
  df$CountryDataCollected[df$CountryDataCollected_WEOG!='WEOG']
  
  # clean some anomalies, missing countries to actual NAs
  df$CountryDataCollected[df$CountryDataCollected=="NA"]<-NA
  df$CountryDataCollected[df$CountryDataCollected==""]<-NA
  
  ## Same for sample_country_data_collected

  # clean some anomalies, missing countries to actual NAs
  df$sample_country_data_collected[df$sample_country_data_collected=="NA"]<-NA
  df$sample_country_data_collected[df$sample_country_data_collected==""]<-NA
  df$sample_country_data_collected[df$sample_country_data_collected=="online"]<-NA
  
  df$sample_country_data_collected_WEOG <- NA
  df$sample_country_data_collected_WEOG[!is.na(df$sample_country_data_collected)] <- 'Non-WEOG'
  df$sample_country_data_collected_WEOG[df$sample_country_data_collected %in% WEOG_countries]<- 'WEOG'
  sum(is.na(df$sample_country_data_collected_WEOG))
  table(df$sample_country_data_collected_WEOG)
  table(df$sample_country_data_collected,df$sample_country_data_collected_WEOG)
  df$sample_country_data_collected[df$sample_country_data_collected_WEOG!='WEOG']
  
  
    
  ## same for FirstAuthorCountry
  
  table(df$FirstAuthorCountry)
  sum(is.na(df$FirstAuthorCountry))
  df$FirstAuthorCountry_WEOG <- 'Non-WEOG'
  df$FirstAuthorCountry_WEOG[df$FirstAuthorCountry %in% WEOG_countries]<- 'WEOG'
  sum(is.na(df$FirstAuthorCountry_WEOG))
  table(df$FirstAuthorCountry_WEOG)
  table(df$FirstAuthorCountry,df$FirstAuthorCountry_WEOG)
  
  df$CountryDataCollected_WEOG<-factor(df$CountryDataCollected_WEOG)
  df$FirstAuthorCountry_WEOG<-factor(df$FirstAuthorCountry_WEOG)
  
  ## same for papers
  d$CountryDataCollected[d$CountryDataCollected=='North America']<-'US'
  #d$CountryDataCollected[d$CountryDataCollected=='Europe']<-'UK'
  
  sum(d$CountryDataCollected=='online',na.rm = T) # 139
  index<-d$CountryDataCollected=='online'
  index[is.na(index)]<-FALSE
  d$CountryDataCollected[index]<-d$SamplePrimaryCountryofOrigin_online_inferred[index]
  table(d$SamplePrimaryCountryofOrigin_online_inferred)
  table(d$CountryDataCollected)
  sum(is.na(d$CountryDataCollected))
  d$CountryDataCollected_WEOG <- NA
  d$CountryDataCollected_WEOG[!is.na(d$CountryDataCollected)] <- 'Non-WEOG'
  d$CountryDataCollected_WEOG[d$CountryDataCollected %in% WEOG_countries]<- 'WEOG'
  sum(is.na(d$CountryDataCollected_WEOG))
  table(d$CountryDataCollected_WEOG)
  sum(table(d$CountryDataCollected_WEOG))+sum(is.na(d$CountryDataCollected_WEOG))
  table(d$CountryDataCollected,d$CountryDataCollected_WEOG)
  d$CountryDataCollected[d$CountryDataCollected_WEOG!='WEOG']
  
  d$CountryDataCollected[d$CountryDataCollected=="NA"]<-NA
  d$CountryDataCollected[d$CountryDataCollected==""]<-NA
  
  
  d$FirstAuthorCountry_WEOG <- 'Non-WEOG'
  d$FirstAuthorCountry_WEOG[d$FirstAuthorCountry %in% WEOG_countries]<-'WEOG'
  table(d$FirstAuthorCountry,d$FirstAuthorCountry_WEOG)
  sum(table(d$FirstAuthorCountry,d$FirstAuthorCountry_WEOG))
}

#### Distance to US variable
if (WEIRD_country_index=='Muthukhrisna'){
  USD <- read.csv('data/weird_sino.csv')
  head(USD)
  table(d$FirstAuthorCountry[!d$FirstAuthorCountry %in% USD$country])
  table(USD$country)
  # list of fixes
  tmp<-USD[USD$country=='Germany',]; tmp$country<-'Austria'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='France',]; tmp$country<-'Beligum'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Slovenia',]; tmp$country<-'Croatia'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Poland',]; tmp$country<-'Czech Republic'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Sweden',]; tmp$country<-'Denmark'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Great Britain',]; tmp$country<-'Ireland'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Spain',]; tmp$country<-'Portugal'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Slovenia',]; tmp$country<-'Slovakia'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='United States',]; tmp$country<-'US'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Great Britain',]; tmp$country<-'UK'; USD<-rbind(USD,tmp)
  
  tmp<-USD[USD$country=='France',]; tmp$country<-'Belgium'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Mali',]; tmp$country<-'Central African Republic'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Armenia',]; tmp$country<-'Greece'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Norway',]; tmp$country<-'Iceland'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Japan',]; tmp$country<-'Israel'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Ethiopia',]; tmp$country<-'Kenya'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Estonia',]; tmp$country<-'Latvia'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Slovenia',]; tmp$country<-'Serbia'; USD<-rbind(USD,tmp)
  tmp<-USD[USD$country=='Malaysia',]; tmp$country<-'UAE'; USD<-rbind(USD,tmp)
  
  
  df$USD <- NA
  d$USD <- NA
  df$USDC <- NA
  d$USDC <- NA
  for (k in 1:nrow(USD)) {
    df$USD[df$FirstAuthorCountry %in% USD$country[k]]<-USD$United.States[k]
    d$USD[d$FirstAuthorCountry %in% USD$country[k]]<-USD$United.States[k]
    df$USDC[df$CountryDataCollected %in% USD$country[k]]<-USD$United.States[k]
    d$USDC[d$CountryDataCollected %in% USD$country[k]]<-USD$United.States[k]
  }
  SPLIT <- mean(unique(d$USDC),na.rm = T)+0.02 # calculate this from the unique index, rather than from our data 
  SPLIT
  d$FirstAuthorCountry_USD_Md <- cut(d$USD,breaks = c(-1,SPLIT,1),labels = c('Western','Non-Western'))
  d$CountryDataCollected_USD_Md <- cut(d$USDC,breaks = c(-1,SPLIT,1),labels = c('Western','Non-Western'))
  df$FirstAuthorCountry_USD_Md <- cut(df$USD,breaks = c(-1,SPLIT,1),labels = c('Western','Non-Western'))
  df$CountryDataCollected_USD_Md <- cut(df$USDC,breaks = c(-1,SPLIT,1),labels = c('Western','Non-Western'))
  
  d
  
  table(d$CountryDataCollected_USD_Md)
  rm(USD,SPLIT,tmp)
}

#### WEIRD countries
if (WEIRD_country_index=='Krys'){
  
  #@article{krys2024weird,
  #  title={WEIRD--Confucian comparisons: Ongoing cultural biases in psychology’s evidence base and some recommendations for improving global representation.},
  #  author={Krys, Kuba and de Almeida, Igor and Wasiel, Arkadiusz and Vignoles, Vivian L},
  #  journal={American Psychologist},
  #  year={2024},
  #  publisher={American Psychological Association}
  #}
  
  # WEIRD_countries = EU; EFTA; Australia; Canada; New Zealand; UK; USA; Israel
  WEIRD_countries <- c("Andorra","Australia","Austria","Belgium","Bulgaria","Canada","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Finland","France","Germany","Gibraltar","Greece","Greenland","Hungary","Iceland","Ireland","Israel","Italy","Latvia","Liechtenstein","Lithuania","Luxembourg","Malta","Monaco","Netherlands","New Zealand","Norway","Poland","Portugal","Romania","San Marino","Slovakia","Slovenia","Spain","Sweden","Switzerland","UK","US","Vatican","Europe","North America")
  nrow(df)
  table(df$CountryDataCollected)
  sum(is.na(df$CountryDataCollected))
  
  df$CountryDataCollected[df$CountryDataCollected=="NA"]<-NA
  df$CountryDataCollected[df$CountryDataCollected==""]<-NA
  #df$CountryDataCollected[df$CountryDataCollected=="online"]<-NA
  
  ##### deal with online studies, infer them to be related to the samplePrimaryCountryofOrigin
  sum(df$CountryDataCollected=='online',na.rm = T)
  index<-df$CountryDataCollected=='online'
  index[is.na(index)]<-FALSE
  df$CountryDataCollected[index]<-df$SamplePrimaryCountryofOrigin_online_inferred[index]
  df$sample_country_data_collected[index]<-df$SamplePrimaryCountryofOrigin_online_inferred[index]
  #### online studies inferred
  
  df$CountryDataCollected_WEOG <- NA
  df$CountryDataCollected_WEOG[!is.na(df$CountryDataCollected)] <- 'Non-WEOG'
  df$CountryDataCollected_WEOG[df$CountryDataCollected %in% WEIRD_countries]<- 'WEOG'
  table(df$CountryDataCollected_WEOG)
  table(df$CountryDataCollected,df$CountryDataCollected_WEOG)
  
  sum(df$CountryDataCollected=="NA",na.rm = T)
  
  d$CountryDataCollected_WEOG <- NA
  d$CountryDataCollected_WEOG[!is.na(d$CountryDataCollected)] <- 'Non-WEOG'
  d$CountryDataCollected_WEOG[d$CountryDataCollected %in% WEIRD_countries]<- 'WEOG'
  
  df$FirstAuthorCountry_WEOG <- NA
  df$FirstAuthorCountry_WEOG[!is.na(df$FirstAuthorCountry)] <- 'Non-WEOG'
  df$FirstAuthorCountry_WEOG[df$FirstAuthorCountry %in% WEIRD_countries]<- 'WEOG'
  table(df$FirstAuthorCountry_WEOG)
  table(df$FirstAuthorCountry,df$FirstAuthorCountry_WEOG)
  
  d$FirstAuthorCountry_WEOG <- NA
  d$FirstAuthorCountry_WEOG[!is.na(d$FirstAuthorCountry)] <- 'Non-WEOG'
  d$FirstAuthorCountry_WEOG[d$FirstAuthorCountry %in% WEIRD_countries]<- 'WEOG'
  table(d$FirstAuthorCountry,d$FirstAuthorCountry_WEOG)
  
  ##### Expanded data
    # clean some anomalies, missing countries to actual NAs
  df$sample_country_data_collected[df$sample_country_data_collected=="NA"]<-NA
  df$sample_country_data_collected[df$sample_country_data_collected==""]<-NA
  df$sample_country_data_collected[df$sample_country_data_collected=="online"]<-NA
  
  df$sample_country_data_collected_WEOG <- NA
  df$sample_country_data_collected_WEOG[!is.na(df$sample_country_data_collected)] <- 'Non-WEOG'
  df$sample_country_data_collected_WEOG[df$sample_country_data_collected %in% WEIRD_countries]<- 'WEOG'
  sum(is.na(df$sample_country_data_collected_WEOG))
  table(df$sample_country_data_collected_WEOG)
  table(df$sample_country_data_collected,df$sample_country_data_collected_WEOG)
  df$sample_country_data_collected[df$sample_country_data_collected_WEOG!='WEOG']
}

#### 11. Weighted Age ------------
cat('\n11. Calculate weighted mean and sd age \n')

Weighted_age_needed <- c(100, 107, 109, 165, 176, 177, 178, 179, 180, 181, 182, 183, 193, 199, 276, 290, 291, 305, 317, 321, 322, 336, 389, 393, 404, 416, 417, 419, 426, 468, 516, 539, 546, 551, 557, 576, 592, 606, 626, 640, 641, 669, 701, 705, 706, 738, 745, 767, 774, 781, 786, 787, 790, 795, 805, 808, 820, 821, 824, 825, 829, 838, 839, 847, 868, 881, 884, 893, 894, 896, 897, 910, 920, 938, 950, 951, 957, 962, 985, 986, 1013, 1047, 1064, 1065, 1117, 1136, 1147, 1150, 1155, 1158, 1161, 1205, 1209, 1222, 1246, 1361, 1362, 1378, 1395, 1397, 1423, 1425, 1427, 1592, 1595, 1597)
WAN <- Weighted_age_needed
df$spurious_samples <- FALSE
for (k in 1:length(WAN)) {
  id<-d$paper_id[WAN[k]-1]
  df$sample_size[df$paper_id==id]
  df$sample_agemean[df$paper_id==id]
  # weighted mean age
  tmp <- weighted.mean(df$sample_agemean[df$paper_id==id],df$sample_size[df$paper_id==id],na.rm=TRUE)
  d$SampleAgeMean[WAN[k]-1]<-tmp
  df$sample_agemean[df$paper_id==id] <- tmp
  # weighted sd of age
  tmp1 <- weighted.mean(df$sample_agesd[df$paper_id==id],df$sample_size[df$paper_id==id],na.rm=TRUE)
  d$SampleAgeSD[WAN[k]-1] <- tmp1
  df$sample_agesd[df$paper_id==id] <- tmp1
  # summed sample size
  tmp2 <- sum(df$sample_size[df$paper_id==id],na.rm=TRUE)
  d$SampleSize[WAN[k]-1]<-tmp2
  df$sample_size[df$paper_id==id] <- tmp2
  # finally, we don't want to keep these specific studies in the expanded data so remove sample1 onwards
  df$spurious_samples[which(df$sample_id!='sample0' & df$paper_id == id)] <- TRUE
}
dim(d)
dim(df)
# delete the spurious ones (241 rows)
df2<-dplyr::filter(df,spurious_samples!=TRUE)
dim(df2)
length(WAN) # 106
table(df$spurious_samples,df$sample_id)
df2$paper_id[df2$paper_id==id]
df2$sample_id[df2$paper_id==id]
df <- df2
rm(df2)
df$paper_id[df$paper_id==id]
df$sample_id[df$paper_id==id]
df$sample_size[df$paper_id==id]
df$sample_agemean[df$paper_id==id]
df$sample_agesd[df$paper_id==id]

####
# Note. This is now putting the weighted mean to d (original) and I'm presuming calculations from df (expanded data)
# Note. Also, the exceptions have been deleted from the expanded data

#### 12. Checking the size of the new data frame --------
cat("12. Check counts of the expanded data frame")

cat(paste('\n...Original:',nrow(d),'obs'))  # 1622
cat(paste('\n...Expanded:',nrow(df),'obs'))  # 1743 (was 1984) same as UID
cat('\n...Within expanded:')
#cat(paste(sum(df$sample_id=='sample0'),'studies')) # 1622 studies (1622 correct!)
cat(length(unique(df$paper_id_study_id))) # 1622 (1622 correct!)
cat('\n...Number of papers (study1 and sample0): ')
cat(sum(df$study_id=='study1' & df$sample_id=='sample0')) # 1360 articles (1360 correct!)

#### 14. TAKE ONLY humanstudies -------
cat("\nFinal dataframes:")
cat("\n D = human studies:\n")
D <- dplyr::filter(d,humansample==TRUE)
cat(nrow(D))
DF <- dplyr::filter(df,humansample==TRUE)
cat("\n DF = human studies with samples:\n")
cat(nrow(DF))
DF<-ungroup(DF)
D<-ungroup(D)

#### Take individual samples
nrow(DF) # 1653, but sample0 is not needed when there are samples 1 to 2....
cat(paste('\n Unique samples in DF:',sum((!is.na(DF$sample_size)))))
# remove those rows that have sample0 AND sample_size==NA but 
DF <- DF %>% filter(!is.na(sample_size) | !sample_id=='sample0')
cat("\n DF = human studies with distinct samples:\n")
cat(nrow(DF)) # 1589
#head(DF[,24:29],10)
#head(DF2[,24:29],10)



#### 14 Optional: Manual Quality Control --------
# compare d and df
#cat("Quality control:\n")

# print((table(d$SamplingMethodDescription)))
# print((table(df$SamplingMethodDescription)))
# print((table(df$sample_sampling_method_description))) # correct!
# 
# print(cbind(d$paper_id_study_id,d$SamplingMethodDescription))
# print(cbind(df$paper_id_study_id,df$SamplingMethodDescription))
# print(cbind(df$paper_id_study_id,df$sample_sampling_method_description,df$sample_id)) # correct

# print((table(d$FemaleParticipantsNumber)))
# print((table(df$sample_gender_balance)))
# print((table(df$sample_gender_balance))) # correct!

# print(cbind(d$paper_id_study_id,d$gender_balance,d$SampleSizeAggregated))
# print(cbind(df$paper_id_study_id,df$sample_gender_balance,df$sample_size))
# print(cbind(df$paper_id_study_id,df$sample_gender_balance,df$sample_id)) # correct
# 
# print(cbind(df$paper_id_study_id,df$sample_size,df$sample_id)) # correct

#print(cbind(df$paper_id_study_id,df$sample_agemean,df$sample_id)) # correct

# dim(df)
#df$sample_agemean[is.na(df$sample_agemean)]<-'delete'
# remove redundant sample0 rows if their sample size is also NA
#df2 <- dplyr::filter(df,sample_id!='sample0' | !is.na(sample_size))
#dim(df2)

# these are calculated on the expanded data
# df2$sample_size
# df2$sample_agemean
# df2$sample_agesd

#print(cbind(df2$paper_id_study_id,df2$sample_size,df2$sample_id))

#### 13. Clean up ------------------
cat("\n15. Clean up")
rm(d2,ind1,ind2,ind3,ind4)
rm(t,k,l,sample_id,new_row,SAMPLE,WAN,tmp,ind,id,index) # tmp variables
rm(Weighted_age_needed,online_convert) # logical indices
#df<-df2
#rm(df2)
rm(list = ls()[grep("^tmp", ls())])
rm(list = ls()[grep("^WEI", ls())])

d$Year<-as.numeric(d$Year)
df$Year<-as.numeric(df$Year)
