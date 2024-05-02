#setwd("/Users/wxbn79/Dropbox/2023_WEIRD")

weird <- read.csv("data/WEIRD coding final corr names.csv")

#unique.titles <- unique(weird$PaperTitle) ##unique titles

#weird_sub <- weird[unique(weird$PaperTitle),]


#table(weird$Year)
#table(weird$CoderName, weird$Journal)

weird$FirstAuthorCountry <- as.factor(weird$FirstAuthorCountry)

#which(weird$FirstAuthorCountry=='')

#################FirstAuthorCountry###############

library(stringr)
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Begium', 'Belgium'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Belgium ', 'Belgium'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Australia ', 'Australia'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'canada', 'Canada'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Finalnd', 'Finland'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Finland ', 'Finland'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Germany ', 'Germany'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Netherlands ', 'Netherlands'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'norway', 'Norway'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'R.R. China', 'China'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Republic of Korea', 'South Korea'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'S. Korea', 'South Korea'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Korea', 'South Korea'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Scotland', 'UK'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Sheffield', 'UK'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Singapore ', 'Singapore'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Spain*', 'Spain'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'sweeden', 'Sweden'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Sweeden', 'Sweden'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Switzerland  ', 'Switzerland'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Switzerland ', 'Switzerland'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'The Netherlands', 'Netherlands'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'United Kingdom', 'UK'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'United States of America', 'US'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'USA', 'US'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Canada; USA', 'Canada; US'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Finland, Sweden', 'Finland; Sweden'))

weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'Finland; USA', 'Finland; US'))
weird$FirstAuthorCountry = as.factor(str_replace_all(weird$FirstAuthorCountry , 'South South Korea', 'South Korea'))

#### CountryDataCollected ############

weird$CountryDataCollected <- as.factor(weird$CountryDataCollected)

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Australia ', 'Australia'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Australia; United Kingdom', 'Australia; UK'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Belgium ', 'Belgium'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Dubai', 'UAE'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'England; France', 'UK; France'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Ethiopia; USA', 'Ethiopia; US'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Finalnd', 'Finland'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Finland ', 'Finland'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Germany ', 'Germany'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Korea', 'South Korea'))


weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Australia; Europe; US', 'online'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Europe; America', 'online'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Northern Europe', 'Finland'))


weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'US; Europe', 'online'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'India; Western', 'India; US'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'India; USA', 'India; US'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Malaysia / Online Existing', 'Malaysia'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'NA; US', 'US'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'North American (USA)', 'US'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'norway', 'Norway'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Norway and Sweden', 'Norway; Sweden'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Online', 'online'))



weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Paris; Warsaw; Vienna', 'Europe'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'portugese', 'Portugal'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Scotland', 'UK'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Serbia; Roma; USA', 'Serbia; US'))


weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Singapore ', 'Singapore'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Sweeden', 'Sweden'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Sweeden; Germany', 'Sweden; Germany'))


weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Switzerland ', 'Switzerland'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Taiwan ', 'Taiwan'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Tiwan', 'Taiwan'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'The Netherlands', 'Netherlands'))


weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Turkey ', 'Turkey'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'Turkey; USA', 'Turkey; US'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'UK; USA', 'UK; US'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'United Kingdom', 'UK'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'United States of America', 'US'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'US; NA; US', 'US'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'USA', 'US'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'USA; Australis', 'US; Australia'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'USA; China', 'US; China'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'USA; Germany;', 'US; Germany'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'USA; Italy; Canada; Russia; Japan', 'US; Italy; Canada; Russia; Japan'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'USA; online', 'US; online'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'USA; Singapore', 'US; Singapore'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'USA; South Korea', 'US; South Korea'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'US; Australis', 'US; Australia'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'US; Germany;', 'US; Germany'))
weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'UK;US', 'UK; US'))

weird$CountryDataCollected = as.factor(str_replace_all(weird$CountryDataCollected , 'South South Korea', 'South Korea'))


weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'North American (US)', 'US'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'online (corpus)', 'NA'))

weird$CountryDataCollected  <-  as.factor(str_replace_all(weird$CountryDataCollected, 'US; US', 'US'))

########### SamplePrimaryCountryofOrigin##########

#weird$SamplePrimaryCountryofOrigin <- as.factor(weird$SamplePrimaryCountryofOrigin)

#weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='America; Canada')]  <- 'US; Canada'
#weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='America; Jamaica')]  <- 'US; Jamaica'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Australia; Online')]  <- 'Australia; online'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Australia: UK')]  <- 'Australia; UK'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='China; Malaysia; india')]  <- 'China; Malaysia; India'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='England; France')]  <- 'France; UK'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Finland; Online')]  <- 'Finland; online'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Finland: Online')]  <- 'Finland; online'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='India; USA')]  <- 'India; US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='NA; Online')]  <- 'online'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='NA; US')]  <- 'US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Online; USA')]  <- 'online; US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Serbia; Roma; USA')]  <- 'Serbia; US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Sweeden; Gemany')]  <- 'Sweden; Gemany'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Turkey; USA')]  <- 'Turkey; US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='United States of America; Korea')]  <- 'South Korea; US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='US; NA; US')]  <- 'US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='US; UK; Australian')]  <- 'US; UK; Australia'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='USA; China')]  <- 'US; China'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='USA; Germany')]  <- 'US; Germany'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='USA; Online')]  <- 'US; online'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='USA; Singapore')]  <- 'US; Singapore'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='USA; South Korea')]  <- 'US; South Korea'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='USA; Taiwan')]  <- 'US; Taiwan'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='USA;Australia')]  <- 'US; Australia'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Australia, Canada, Denmark, Finland, Iceland, Netherlands, Norway, Qatar, South Korea, United Kingdom, United States ')]  <- NA



weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Finland; Portugal; American; other')]  <- 'Portugal'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Finland; US; Canada; Germany; UK; Finland; Australia; Netherlands')]  <- 'US'


weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Germany; UK; Japan, Austria; Finland; Norway; Portugal; Sweden; US')]  <- 'Germany'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Germany; Switzerland; Japan; Canada; Austria; Netherlands; US ')]  <- 'Germany'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Japan; China; Poland; Germany; USA')]  <- 'Japan; China; Poland; Germany; US'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='US; Canada; UK; Hong Kong; Germany; France')]  <- 'US'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='US; India; Italy')]  <- 'US'


weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='UK, Germany, Norway, US, Austria, Australia')]  <- 'UK'


weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Paris; Warsaw; Vienna; others')]  <- NA
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='24 nationalities')]  <- NA
#weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Australia; Asia')]  <- 'Australia; China'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='USA; Ireland; Netherlands; UK; Australia; Canada; Greece; Malaysia; Belgium; others')]  <- 'US'


weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='USA; UK; Canada; Australia; India; others')]  <- 'US; UK'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Canada, USA, 11 participants from other countries in Asia, Europe, or South America')]  <- 'Canada'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Canada, USA, New Zealand, Panama, France, Germany, Denmark, Bulgaria, South Korea')]  <- 'Canada'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='German')]  <- 'Germany'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Romania ')]  <- 'Romania'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='online ')]  <- 'online'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Online ')]  <- 'online'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='UK ')]  <- 'UK'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Korea')]  <- 'South Korea'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='S. Korea')]  <- 'South Korea'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='America')]  <- 'US'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Torres Strait Islander; Africa; Europe; South East Asia ')]  <- 'Europe'
#weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Israel (40.8%); countries in the former USSR (42.8%); Western Europecountries (4.5%); US and Canada (4.0%); countries in the former Eastern Bloc (3.0%); countries in South America (2.0%); and other (3.0%).')]  <- 'Israel; Eastern Europe'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='online (Europe and North America)')]  <- 'online'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='USA')]  <- 'US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='United States of America')]  <- 'US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='United States')]  <- 'US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='American')]  <- 'US'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='United Kingdom')]  <- 'UK'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Online')]  <- 'online'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='india')]  <- 'India'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='England')]  <- 'UK'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='European')]  <- 'Europe'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Finalnd')]  <- 'Finland'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Finnish')]  <- 'Finland'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='India; Western')]  <- 'India; US'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='israel')]  <- 'Israel'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Malaysian/ international')]  <- 'Malaysia'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Nertherlands')]  <- 'Netherlands'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Norway and Sweden')]  <- 'Norway; Sweden'

weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Polish')]  <- 'Poland'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Romania; Russia; Poland; Bulgaria; Moldova; Portugal; Malta; Lithuania; Kazakhstan; Italy; Estonia; Spain; Israel')]  <- 'Romania'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Serbia; Roma')]  <- 'Serbia'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Spain/online')]  <- 'Spain; online'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='sweeden')]  <- 'Sweden'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Sweeden')]  <- 'Sweden'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='The Netherlands')]  <- 'Netherlands'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='UK;Italy')]  <- 'UK; Italy'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Australian')]  <- 'Australia'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='US;Australia')]  <- 'US; Australia'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Europe ')]  <- 'Europe'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='Europe ')]  <- 'Europe'
weird$SamplePrimaryCountryofOrigin[which(weird$SamplePrimaryCountryofOrigin=='US; Korea')]  <- 'US; South Korea'



#weird$SamplePrimaryCountryofOrigin  <-  as.factor(str_replace_all(weird$SamplePrimaryCountryofOrigin, ',', ';'))

#weird$SamplePrimaryCountryofOrigin  <-  as.factor(str_replace_all(weird$SamplePrimaryCountryofOrigin, ':', ';'))




#weird$SamplePrimaryCountryofOrigin <- droplevels(weird$SamplePrimaryCountryofOrigin)


#weird$FirstAuthorCountry <- droplevels(weird$FirstAuthorCountry)


#########Ethnicity

#weird$Ethnicity <- as.factor(weird$Ethnicity)

weird$Ethnicity[which(weird$Ethnicity=='')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='110 (51%) Asian, 2 (1%)  African, and 103 (48%) European or American. ')]  <- 'Asian'
weird$Ethnicity[which(weird$Ethnicity=='19.8% African American, 41.9% Caucasian, 15% Middle Eastern, 12.1% South Asian [e.g., Indian], 2.6% East Asian [e.g., Korean, Japanese, Chinese], 2.4% Latin American/Hispanic, 0.8% Pacific Asian [e.g., Filipino, Malaysian], 0.3% Native American, and 5.2% Mixed/Other')]  <- 'white'
#weird$Ethnicity[which(weird$Ethnicity=='African American')]  <- 'black'
weird$Ethnicity[which(weird$Ethnicity=='african american 5, 4%; Caucasian 110, 81%; hispanic american 5, 4%, native american 5, 4%, other 9 7%, no response 1 1%')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='American')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Anglo Australian')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Arab; Jew')]  <- 'other'
weird$Ethnicity[which(weird$Ethnicity=='asian; ')]  <- 'Asian'
weird$Ethnicity[which(weird$Ethnicity=='Asian; Caucasian; Hispanic; African American; Mixed; Other; Unknown')]  <- 'Asian'
weird$Ethnicity[which(weird$Ethnicity=='Austrialian')]  <- NA
#weird$Ethnicity[which(weird$Ethnicity=='Black')]  <- 'black'
#weird$Ethnicity[which(weird$Ethnicity=='Black; White')]  <- 'black; white'
weird$Ethnicity[which(weird$Ethnicity=='Brazilian')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='British')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Caucasian')]  <- 'white'

weird$Ethnicity[which(weird$Ethnicity=='caucasian; african-american; hispanic; asian; other')]  <- 'Hispanic'
weird$Ethnicity[which(weird$Ethnicity=='caucasian; asian; hispanic; african-american')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='caucasian; canadian; russian; chinese; irish; jewish')]  <- 'white'

weird$Ethnicity[which(weird$Ethnicity=='Chinese')]  <- 'Asian'
weird$Ethnicity[which(weird$Ethnicity=='Dutch')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='East Asian')]  <- 'Asian'
weird$Ethnicity[which(weird$Ethnicity=='East Asian, South Asian, Black, White')]  <- 'Asian'

#weird$Ethnicity[which(weird$Ethnicity=='ethiopian')]  <- 'black'
weird$Ethnicity[which(weird$Ethnicity=='European American')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='European; North American; Asian; Aboriginal; other')]  <- NA


weird$Ethnicity[which(weird$Ethnicity=='European/Caucasian')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='Finnish')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Finnish 35%; portugese 36%; american 12%; other 17%')]  <- NA

weird$Ethnicity[which(weird$Ethnicity=='Finnish; Vietnamese; Taiwanese; Hungarian; German')]  <- NA

weird$Ethnicity[which(weird$Ethnicity=='Finnish(?)')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='German')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='German; Taiwanese')]  <- 'Asian'

weird$Ethnicity[which(weird$Ethnicity=='Greek')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Han Chinese')]  <- 'Asian'

weird$Ethnicity[which(weird$Ethnicity=='indian; western')]  <- 'Asian; white'

weird$Ethnicity[which(weird$Ethnicity=='Iranian(?)')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Irish')]  <- NA

weird$Ethnicity[which(weird$Ethnicity=='Irish; German; Australian; American; British')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Israeli(?)')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Japanese; Chinese; Polish; German; American')]  <- 'Asian'


weird$Ethnicity[which(weird$Ethnicity=='Jewish')]  <- 'other'

weird$Ethnicity[which(weird$Ethnicity=='Jewish; Muslim Arab')]  <- 'other'

weird$Ethnicity[which(weird$Ethnicity=='Latino/Hispanic')]  <- 'Hispanic'

weird$Ethnicity[which(weird$Ethnicity=='Latino/Hispanic; other')]  <- 'Hispanic'

weird$Ethnicity[which(weird$Ethnicity=='Malian; German; Bulgarian')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Na')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='NA ')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='NA; NA')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='NA; NA; white')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='non-Hispanic White')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='Nordic and Swedish')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='One participant self-identified as British, one as Tibetan, one as Canadian, and three as Chinese; the remaining 22 were US/American')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Other')]  <- 'other'
weird$Ethnicity[which(weird$Ethnicity=='Other (Hispanic)')]  <- 'Hispanic'
weird$Ethnicity[which(weird$Ethnicity=='portugese')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Portuguese')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Swedish')]  <- NA
#weird$Ethnicity[which(weird$Ethnicity=='Traditional Africans')]  <- 'black'
weird$Ethnicity[which(weird$Ethnicity=='Turkish')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Turkish; American')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Turkish; Russian or Ukrainian; Polish')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='Twenty-three participants were Finnish; other nationalities (one each) were Turkish, Russian, Greek, Dutch, Australian, Indian, Ethiopian, Moroccan, Vietnamese, South African, Chilean, Costa Rican, and Ukrainian')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='White')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='White (70%)')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='white (80%); Asian (20%)')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='White (89%), Chinese, Indian, Other Asian')]  <- 'white'

weird$Ethnicity[which(weird$Ethnicity=='White British; Other White; Asian British; Caribbean; Middle Eastern')]  <- 'white'

weird$Ethnicity[which(weird$Ethnicity=='White Caucasian')]  <- 'white'

weird$Ethnicity[which(weird$Ethnicity=='white; african-american; asian; mixed/other')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='white; asian')]  <- 'Asian; white'
weird$Ethnicity[which(weird$Ethnicity=='white; asian; other')]  <- NA
weird$Ethnicity[which(weird$Ethnicity=='white; black')]  <- 'black'

weird$Ethnicity[which(weird$Ethnicity=='white; white')]  <- 'white'

#weird$Ethnicity[which(weird$Ethnicity=='White/Caucasian; Jamaican')]  <- 'black; white'


weird$Ethnicity[which(weird$Ethnicity=='White/Caucasians')]  <- 'white'



weird$Ethnicity[which(weird$Ethnicity=='African American')]  <- 'black'

weird$Ethnicity[which(weird$Ethnicity=='Black')]  <- 'black'
weird$Ethnicity[which(weird$Ethnicity=='ethiopian')]  <- 'black'

weird$Ethnicity[which(weird$Ethnicity=='white=28; asian=2; black=3, other=1')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='white=51; black=7; Asian=3; other=4')]  <- 'white'
weird$Ethnicity[which(weird$Ethnicity=='Traditional Africans')]  <- 'black'
weird$Ethnicity[which(weird$Ethnicity=='Indigenous (First nation)')]  <- 'other'

#weird$Ethnicity <- droplevels(weird$Ethnicity)

#######Musicianship#############################

weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='47% non-musicians; 53% had some kind of musical training')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='85 musicallly trained')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='amateur musicians')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Average experience playing 9,63 years.')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='basic; intermediat training')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='beginner - grade 8')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='budding musicians')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Congenital amusic')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='early-trained musicians, late-trained musicians, and non-musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='enrolled in infant music course')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='M/A')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='mixed')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Music education M=3,56 +-4,23')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='music students')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='music teachers')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='music teachers; music students')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicains; non-musicians')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musically involved')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musically unsophisticated')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musician')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Musician')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musician, non-musician')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Musician; non-musician')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musician; non-musician ')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musician; non-musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Musicians')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians ')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Musicians ')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians and non-musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians and non-musicians (non-music majors)')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians pianist; musicians; non musicians; ')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians, non-musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Musicians, non-musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians, non-musicians, and dancers')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians; amateurs musicians')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians; dancers')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians; dancers')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians; music students; non-musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians; music teachers')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians; music teachers')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Musicians; non-musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Musicians; non-musicians (x both ethnicities)')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians; nonmusicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Musicians; nonmusicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='musicians(?)')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Musicianship was coded into 3 groups by judges - 43,9% low; 45,3% moderate; 10,7% high')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='non-music majors')]  <- 'non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='non-musical')]  <- 'non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='non-musicans')]  <- 'non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='non-musician')]  <- 'non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='non-Musicians')]  <- 'non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Non-musicians')]  <- 'non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Non-Musicians')]  <- 'non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='non-musicians; musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='non-Musicians; Musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='non-musicians; musicians; experienced-musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='nonmusician; musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='nonmusicians')]  <- 'non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='nonmusicians; musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='not reported')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Over 5 years musical training; less than 2 years musical training')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='pianists; singers; non-musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Professional')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='professional harpsichordists ')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='professional harpsichordists; musicians; non-musicians ')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='Professional musician')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='professional musicians')]  <- 'musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='professional musicians; non-musicians')]  <- 'musicians; non-musicians'
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='students')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription=='')]  <- NA
weird$SampleMusicianshipDescription[which(weird$SampleMusicianshipDescription==' musicians')]  <- 'musicians'

#######  Sample Other Description   #############################

#weird$SampleOtherDescription_original <- weird$SampleOtherDescription

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription==' ;undergraduate students')]  <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='"mostly psychology students" ')]  <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='12 kids spoke Spanish as their first language')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='15+ year jazz adjudication experience')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='16,4% ethnic minority status')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='2 of the participants are of non-white origin')]  <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=='2 professionla percussionists, 9 undergraduates')]  <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='3-year-olds; 5-year-olds; 7-year-olds; university students; young adults')]  <- 'children; university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='327 7th grade; 184 8th grade')]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='36 musically trained participants; 4 self-taught musicians; the participants were split into 2 groups.')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='39% bilingual')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='3rd grade; preschool')]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='4-9 year olds')]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='53 classically trained, 28 Jazz trained; 24 classically trained, 5 Jazz trained')]  <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=='53,13% have had musical training')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='56,5% married; 33,9% retired')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='5th grade children')]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='8 participants from experiment 1 and two authors')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='88% were more than 55 years old')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='academicians')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='accurate singers; VPID singers')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adolescent children')]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adolescents')]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='Adolescents')]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adolescents from 10 classes in three different high schools; three major types of habitation (a big city, a development town, and several villages)')]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adoloscents')]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adult')]  <- 'adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adult choir members')]  <- 'adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='Adult family caregivers of ICU patients ')]  <- 'caregivers'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adult musicians')]  <- 'adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adult oncology patients ')]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adult patients in psychiatric hospitals in Norway')]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adult; children')]  <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adult; children')]  <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='Adults')]  <- 'adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adults across the lifespan')]  <- 'adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adults from the last study')]  <- 'adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='Adults hospitalized with cancer')]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adults on a medical oncology/hematology inpatient unit ')]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adults over age 65')]  <- 'older adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adults undergoing residential treatment for substance use disorders and matched controls')]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adults with substance use disorder')]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adults; 6th graders; kindergartenders')]  <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adults; normal hearing')]  <- 'adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='adults; older adults')]  <- 'older adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='advanced pianists')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='all advanced performers or experienced teachers, who were familiar with tonal and post-tonal western classical music and had a good grasp of music theory,')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='All had undertaken advanced musical study, either at conservatoire or university')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=='All participants were part of a band, expertise level M (between groups)= 3,73 out of 5')]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Alzheimer's patient")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="amateur musicians; university students; high-school students")]  <- 'children; university students'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Amazon Mechanical Turk recruited participants")]  <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="amusic participants; control participants")]  <- 'amusics'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Amusics and controls")]  <- 'amusics'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Anglophones; francophones")]  <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="arts intervention policy makers")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="athletes")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="attending a multi-disciplinary chronic pain clinic at the time of data collection, were experiencing complex multi-sited pain of at least two years\xd5 duration, and had been referred for non-medical pain management")]  <- 'patients'

#weird$SampleOtherDescription[which(weird$SampleOtherDescription=="attending a multi-disciplinary chronic pain clinic at the time of data collection, were experiencing complex multi-sited pain of at least two years’ duration, and had been referred for non-medical pain management")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="audience members")]  <- 'audience'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="author-participants")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="autism spectrum disorder; neurotypical")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Autistic (ASD)")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="autistic adults")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="band members")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="band sutduent")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Bassonists; hornists")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="boys with ADHD")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Bruce Springsteen fans")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="cardiac rehabilitation program participants")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="caregivers to Alzheimer\xd5s patients")]  <- 'caregivers'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="caregivers to people with Parkinson's disease")]  <- 'caregivers'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="case study subject; control participants")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Case-study of a composer")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="child in music therapy")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="child; parent' music therapist")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Children")]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children age 3-4")]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children and adults")]  <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children and an adult control group (university students studying economics or a similar major or music)")]  <- 'children; university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children and parents")]  <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Children and young people")]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children from 4 UK primary schools")]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children in school")]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children recruited from three elementary schools and one music school from the same region")]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children taking music exams")]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children who would learn music")]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children with ADHD")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children with and without Developmental coordination disorder ")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children with and without EF deficits")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children with autism and typically developing children, in dyads with cargivers")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Children, Musical training from beginner to Grade 8")]  <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="children; adults")]  <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Children; Adults")]  <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Chinese speaking (further split into Amusic, trained, untrained); control group")]  <- 'amusics'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="choir members")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Choir sample; control group (non recruited, data extrated from a database)")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Choir singers")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="CI users; bilaterally postlinugally deaf")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="clarinet students")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="clarinettists")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="classical guitarists")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="classical music preferences; popular music preferences")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="classical musicians")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Classical musicians enrolled in postgraduate classical music performance studies")]  <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Classical musicians, Musical experience M= 12,34 y; Heavy metal musicians, Musical experience M= 11,38 y")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="clinic staff")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="cochlear implant users")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="community choir members")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="composer")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="composers")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Composers")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Composers & theorists; professional musicians; non-professionals")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="composition students")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Computer Music Designers")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Concert audiences")]  <- 'audience'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Conditions - live; audio/video; audio; video")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="conductor; ensemble ")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Congenital amusia; non-amusia")]  <- 'amusics'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="conservatoire students")]  <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Conservatoire students")]  <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="conservatoire teachers; undergraduate students")]  <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="conservatoire teaching staff; experienced examiners")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Conservatoriums and Schools of music students")]  <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="conservatory students")]  <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="consumers; most of ISCED levels  2 and 3")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Control group; Experimental group")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="coronary patients")]  <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Dalcroze teachers")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Dancers")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Danish String Quartet")]  <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="deaf; hearing")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="detoxification patients")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="diagnosed with ASD")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Dyads (mothers and infants)")] <- 'adults; infants'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Elderly patients with different levels of cognitive impairment")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="electronic music artists")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="elementary music teachers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="elementary school children")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="elementary school children; immigrants")] <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Employees at Philips Research")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="english speaking")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="English-speaking students")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="enrolled in either introductory, undergraduate, or postgraduate music course")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="examiners from five major instrument families")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Expectant parents")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="experienced and novice musicians")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="experienced guitarists")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="experienced guitarists; non-guitarists")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="experiment groups: MI & DA Rochester university; MI & DA Indiana university; IL &AP Rochester university; IL & AP Indiana university")] <-  'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="expert jazz educators in the USA")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="expert pianists; intermediates, novices")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="experts in makam music")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="family members")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="film music enthusiasts")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="First performed age M=6,66(SD=1,50); practiced M=4,5h (SD=1,18); performed in public over the last month M=3,08(SD=2,01)")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="First-year psychology students")] <-  'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="first-year psychology students; music college students; social media recruits")] <-  'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="first, second and third survey of the same population")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="FirstYearUndergraduates; ")] <-  'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="five dyads (mother and infant)")] <-  'adults; infants'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Flute masterclass")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="fourth graders across China")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="from city of Kassel, Germany; people outside of university with varying degrees of education, musical training, and covering a wide age range")] <-  'adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Gamelan ensemble members")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="gay men")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="General Motors sales clerks")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="German-; French-; and Italian-speaking ")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="grade 8")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="graduate and postgraduate students; regular participants\nin synchronization and rhythm perception experiments")] <-  'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="graduate students")] <-  'university students'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="graduate students and professional musicians")] <-  'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="graduate students; undergraduate students")] <-  'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="graduate students; university students; community")] <-  'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Greek speakers; English speakers")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="group interview; individual interview.")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Groups were split by experimental condition. Employment status was reported (majority students for both groups)")] <-  'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="guitarists")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="guitarists; pianists; non-musicians")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="healthy and normal hearing adults")] <-  'adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="healthy seniors; Alzheimer patients")] <-  'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="healthy, full-term infants")] <-  'infants'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="heavy metal fance")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="heavy metal versus other music fans")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="heterogeneous sample in terms of age, gender, occupation")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="high school students")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="high school students ")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="high school students to working professionals")] <-  'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="high school; univeristy; professional")] <-  'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="High-school students")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="higher education instrumental teachers and students")] <-  'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Highly educated")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="highly trained pianists; Masters students at the local conservatoire, music students at local universities and highly practised amateurs; at least 10 years performing experience")] <-  'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="highschool students")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="holocaust survivors")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="improvisation")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="individuals who previously participated in research in 1997; just commencing learning in primary (elementary) school band programmes from eight primary schools in Sydney,")] <-  NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="internet participants; undergraduate students")] <-  'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="internet users")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Japanese and English schoolchildren")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="jazz listeners")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="jazz musician duos")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="jazz pianists ")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="jazz singers with improvisation experience")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="jazz specialists")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Jewish")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="junior conservatoire students")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="kindergarden")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="kindergarten school children")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="kindergartners; first graders")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="knee-operations patients")] <-  'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="known to authors/targeted specifically for expertise")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Last FM listeners")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Latino")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="lay choristers")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Less than 3 years of formal musical training")] <-  NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="liberal arts students")] <-  'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="light skinned; blue-collar familiaes")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="literate; non-literate")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="low socioeconomic level")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="M=12,42 years of teaching experience")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Majority Students")] <-  'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="males; females")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="MastersStudents")] <-  'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Mathematicians; Literature scholars")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Mechanical Turk")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="medical professionals")] <- 'clinicians'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="men; women")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="middle school band students")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="middle school music students")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="middle school students")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="middle school, high school, and university students, and community members")] <- 'children; university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="middle- and high-school students")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="mixed nationality")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="mixed nationality")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Mostly college/undergraduate students")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="mostly psychology students")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Mostly psychology undergraduates")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="mostly recruited via university")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="mostly students")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="mostly university students")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="mothers and adolescent offspring")] <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="mothers and preterm infants")] <- 'adults; infants'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="MTurk")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="MTurk and undergraduate students")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="MTurkers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Multicultural London English speakers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music and ballet high school students")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music class; elementary school children")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music education students")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music educators")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Music importance on a 7-point scale M=5,83; Daily engagement with music M=3,46 h")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music learners")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music majors")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music performers and music listeners")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music professionals")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Music professors; music students")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music psychologists; music therapists")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music psychology students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Music researchers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music school students")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music stimuli")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music stimuli")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Music students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music students and graduates")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music students and musicians at university")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music students and professional musicians")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music students and professors of music; drummers excluded")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music students; professional musicians")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music teachers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Music teachers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music teachers and students")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music teachers; students")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music therapists")] <- 'clinicians'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music therapists; music administrators")] <- 'clinicians'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="music undergraduates")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Musical experience: 8")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Musical experience: 9; 1; 12; 1 years")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Musical trainig, ballet dancers training, non-musicians")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Musical training in years M=10, SD=6")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="musicians")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="musicians and rhythm experts")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Musicians and sound professionals")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="musicians employed in an Italian hospital")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="musicians with experience playing in healthcare settings")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="musicians; hospitalised adolescents; hospital staff")] <- 'clinicians; patients'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="MusicMajors")] <- 'university students'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="musicology students; familiar with music and emotions research")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="NA ")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="NA; undergraduate students")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="na\x95ve; adults")] <- 'adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="native English speakers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="native English students; native Chinese students")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="native french speakers")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="native speakers of Southern British English, recruited from an introductory linguistics course")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="native speakers of Thai studying at Chulalongkorn University in Bangkok; English as a first language and were recruited from Psychology 1 at the University of Western Sydney ")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="non-music majors")] <- 'university students'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="North Indian Classical")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Oboist")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="older adults cognitive impairmanet")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="online")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="opera choristers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="opera trainees")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="opportunity sampling of English speaking participants")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="orchestra musicians")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="orchestral excerpts")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Over 50% with more than 16 years of education")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="paediatric patients")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Parental education (54,1% with neither parent holding a HE degree)")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="parents")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="parents of babies")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="participants from the Belgian Ardenne untrained to the yoik")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Participants had children with ASD who received music therapy at OC-MTC")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="participants have been diagnosed with autism")] <- 'patients'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="participants living in and around Barcelona")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Participants recruited for the study were between 14-31 years old")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Participants recruited for the study were between 30-40 years old")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="participants with Alhzeimers")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="participants with Alzheimers / dementia")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="past and current students of the researcher")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Patients")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="patients with depression and sleep disturbances")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="patients with mental health and substance abuse issues")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="patients with Parkinson's disease")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="People with Dementia; caregivers")] <- 'caregivers; patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="people with Parkinson\xd5s Disease")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="percusionists; dancers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="physically active and apparently healthy ")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="pianists")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="pianists; musicians; non-musicians  ")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="piano player")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="piano players")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="piano students")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="piano teachers")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="piano teachers; students")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Police officers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="pop musicians")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="popular musicians")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="postgraduate student")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="postgraduate students of classical singing")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="postgraduates; undergraduates")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="practicing music therapists")] <- 'clinicians'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="pregnant mothers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="preschool children")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Preschool children")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="preservice music teachers ")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Primary instrument - piano (26,8%); music performance students (52,5%); undergraduate level of study (67,9%)")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="primary school")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="prisoners")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="prisoners; music therapists; prison staff")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Profesional performers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Professional dancers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="professional musicians")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="professional musicians, amateur musicians, music therapists, and music teachers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="professional musicians; amateur musicians")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="professional orchestra musicians")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="professional organists")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="professional performers of classical music with at least 10 years of orchestral conducting/performing experience; conductors with 10-31 years of professional orchestra experience and degrees from conservatory")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="professional pianists")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="professional singers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="professional string quartet")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Professionals recruited from music educators, physiotherapists, and Aleander Technique teachers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Prolific")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="psychiatric patients")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="psychology and music students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="psychology students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Psychology students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="psychology students at the University of Bologna")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="psychology students; music students without absolute pitch; music students with absolute pitch")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Psychology undergraduates")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="public high school students in Athens")] <- 'children'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="public primary school children")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="public school; private school")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="radio listeners")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="random audience members")] <- 'audience'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="range of professional experience on stage/academic degrees")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="recruited via university")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="representative sample of the italian population")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Right handed, normal hearing")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="right-handed pianists")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="runners")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="sample is that of studies 2 and 3 combined")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="School grade and school with/without musical programmes")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="school students")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="school students; music teachers")] <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="school students; staff")] <- 'adults; children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="schoolchildren")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Second Generation to Holocaust survivors")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="second group of participants had Alzheimers")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="secondary school students")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="secondary-level and university students")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="self-reported normal hearing")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="senior adults that begun singing lessons after 40")] <- 'older adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="senior citizens")] <- 'older adults'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="seventh and ninth graders")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="sighted and blind 10 year olds attending primary school")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="sighted and blind 10 year olds attending primary school")] <-  'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Significantly visually impaired")] <-  'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Sine-phase condition; Random condition")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="singer-pianist duos; professional musicians; students")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="singers")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="singers; expert listeners")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Singers; non-singers")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Smokers")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="some experienced chronic mental illness, physical, or intelliectual disabilities")] <-  'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="songwriters")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="sophomore music students and speech and hearing students")] <-  'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="sound engineers")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="sound-colour synaesthetes")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="spanish-speaking elementary pupils")] <-  'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="string players, undergraduate students")] <-  'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="string players, undergraduate students")] <-  'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="student")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Student (Below 60yo); teacher (below 60yo); Student (above 60yo); Teacher (above 60yo) ")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Student musicians and professional musicians")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students")] <-  'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Students")] <-  'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students ")] <-  'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students attending high schols in Istanbul, schools of middle-class socioeconomic status")] <-  'children'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students in psychology courses at university")] <-  'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students of 3 cello teachers")] <-  NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students of biology, chemistry, psychology, and biochemistry")] <-   'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students of University of Leipzig; 5 subjects played musical instrument, others no musical training")] <-   'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students under 18")] <-   'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students with absolute pitch; quasi-absoulte pitch; relative pitch")] <-   NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students, colaborators")] <-   NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students, researchers")] <-   NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Students; NA")] <-   'university students'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students; wind and string instrument learners")] <-NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="students;musicians")] <-NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="study authors")] <-NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="studying in a Finnish university to become primary school teachers")] <-'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Swedish athletes")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="teachers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Teachers")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="teachers and parents of nursery children")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="teritary music students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Third- and fourth-grade children")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Traumatic Brain Injury Patients")] <- 'patients'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Tribe")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="trombonists, non-trombonists, non-musicians")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Turkish musicians; turkish non-musicians; Western listeners")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="twins; interview population extracted from the survey population")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="twins; online survey")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Twitter users")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="UCLA community (university students)")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="UK music undergraduate students; portfolio career musicians performing or teaching in UK")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate ")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate and graduate students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate and graduate students enrolled in music classes or ensembles within a large university in the southern portion of the United States")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate and graduate students of flute at conservatory")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate and postgraduate music performance programs at conservatory")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate and postgraduate students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate music majors")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate music majors from a mid-size southeastern public university.")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate music students")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Undergraduate music students")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate music students, students in general music subject, music lovers, depression sufferers")] <- 'patients; undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate popular music students")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate psychology students")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate psychology testing pool")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate student; professor")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Undergraduate students")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students ")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students and professional musicians")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students from indtroductory psychology class")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students in psychology courses")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students of music")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students of psychology")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students or recent graduates")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students who were members of concert bands at two universities")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students; English-speaking")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students; German-speaking")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students; graduate students")] <- 'university students'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students; older control; participants with Alzeimers")] <- 'patients; undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate students; professional musicians")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate; postgraduate")] <- 'university students'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduate; postgraduate; staff")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduates")] <- 'undergraduate students'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Undergraduates")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduates ")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Undergraduates;")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="undergraduates; graduate students; community")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Undergraduates; Masters")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Undergraduates; MusicMajors")] <- 'undergraduate students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Undergradutae music majors; Undergraduate non-music majors")] <- 'undergraduate students'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university  students and staff")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university and high school students")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university and high school students")] <- 'children; university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university and music academy students")] <- 'children; university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university community")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university jazz program students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="University members")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university music students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="University of Reading students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="University students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="University Students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students ")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university  students")] <- 'university students'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="University Students & wider community members")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students and amateur musicians")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students and children")] <- 'children; university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students and graduates")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students and musicians")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students and professional musicians")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students and retirees")] <- 'older adults; university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students and staff")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students of multiple faculties")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students of the humanities")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students, most were communication majors")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="University Students, Postgraduate Students, and University staff")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students, regular iTunes users")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; adoloscents")] <- 'children; university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; community")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; indian nationals")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; local community")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; other")] <- 'university+'

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; performing musicians")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; professional musicians")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; university faculty")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; university staff")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; university teachers")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students; volunteers")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university students/graduates")] <- 'university+'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university teacher")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university-level students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="university/conservatory/music school students")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="untrained listeners; trained listeners")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="urban and rural professional drummers; university students and mature professional percussionists; university students")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Various nationalities, biggest group from Australia")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="veterans; average socio-economic status; married")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="violinist")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="violinists; professional musicians, recently graduated from a conservatoire in Finland; accomplished amateur musicians studying at a university in Finland")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Williams syndrome")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Williams Syndrome; typical development")] <- 'patients'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="wind, percussion, and string instrumentalists participating in either a symphony orchestra or wind ensemble course in a School of Music at a mid-Atlantic university.")] <- 'university students'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Year 7 and 8 students in a high school ")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="young adults")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Young adults and older adults")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="young adults; non-musicians, amateur musicians, and expert musicians")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="young children")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="young children (toddlers)")] <- 'children'
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="young musicians")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="young people")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="Young people aged 18-23")] <- NA

weird$SampleOtherDescription[which(weird$SampleOtherDescription=="young-adults; older-adults")] <- NA
weird$SampleOtherDescription[which(weird$SampleOtherDescription=="younger toddlers; older toddlers")] <- 'children'





#####MusicOriginCountry

weird$MusicOriginCountry[which(weird$MusicOriginCountry=="NA")] <- NA
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="British isles")] <- 'UK'


weird$MusicOriginCountry[which(weird$MusicOriginCountry=="NA ")] <- NA
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Blues")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Wesern")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="western, Bolivia")] <- 'Western; Bolivia'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="western")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="western, eastern european")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Australia")] <- 'Australia'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Indian; Western")] <- 'India; Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Ethiopian")] <- 'Ethiopia'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Oriental")] <- 'Israel; Turkey'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="\"Oriental\"")] <- 'Israel; Turkey'

weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Romanian")] <- 'Romania'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Unclassified")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="United Kingdom")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="German")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Austrian")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Canada")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Westen; Turkey")] <- 'Western; Turkey'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Indian")] <- 'India'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Western; Turkish")] <- 'Western; Turkey'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Brazilian")] <- 'Brazil'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Western ")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Aboriginal; Balinese; African-American; Pop, Art-Music ")] <- 'Australia; Bali; Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="various")] <- 'Western; China'

weird$MusicOriginCountry[which(weird$MusicOriginCountry=="various ")] <- 'Western; China'

weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Western; Non-Western")] <- 'Liberia; Ethiopia; Burundi; Ghana; Sierra; Leone; Algeria; Uganda; Somalia; Central African Republic; Zimbabwe; Venezuela; Bolivia; Brazil; Chile; Ecuador; Peru; Mexico; Belize; Nicaragua; Dominica; Haiti; Martinique; St.; Lucia; Western; Thailand; Cambodia; Laos; Burma; Vietnam; Indonesia; Bali; Philippines; India; Guyana; Nepal; Afghanistan; Pakistan; Bangladesh; Egypt; Turkey; Iran; Yemen; Greece; Lebanon; Morocco; Israel; Saudi Arabia; Kazakhastan; Uzbekistan; China; Taiwan; Japan; Korea; Mongolia; Hungary; Macedonia; Iceland; Latvia; Belgium; Poland; Russia; Belarus; Ukraine; Romania; Bulgaria; Slovenia; Croatia; Serbia and Montenegro; Bosnia-Herzegovina; Albania; Cyprus; Fiji; Vanuatu; Micronesia; Cook Islands; Samoa; Papua New Guinea; Solomon; Tonga; Kiribati; Australia; Caledonia; Marshall; French Polynesia; New Zealand'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Portuguese")] <- 'Portugal'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Contested - Armenia, Azerbaijan, Turkey, Iran")] <- 'Armenia; Azerbaijan; Turkey; Iran'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="England; France; Germany; Italy")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="British Isles")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="non-western")] <- 'Liberia; Somalia; Venezuala; Brazil; Chile; Western; India; Turkey; Egypt; Greece; Morocco; Iran; Yemen; Japan; Korea; Sweden; Iceland; Latvia; Bosnia-Herzegovina; Tonga; Marshall Islands'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="UK")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Latin America; western")] <- 'Latin America; Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="USA")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Western; Traditional (Malian and Bulgarian Folk)")] <- 'Western; Mali; Bulgaria'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Western; Traditional; World")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="")] <- 'NA'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Western and Eastern")] <- 'Western; Japan'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Eastern (Chinese Pop Music)")] <- 'China'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Western; other")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Music from Israel")] <- 'Israel'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Columbian")] <- 'Columbia'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="African; Latin American; Asian; World")] <- 'Africa; Latin America; Asia'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Arabic Maqam Kurd")] <- 'North Africa'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="UK; Ireland")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="European; Chinese")] <- 'Western; China'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Various")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="UK, USA, Germany")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="hungary")] <- 'Hungary'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="France")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Austria")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Germany")] <- 'Western'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Czech")] <- 'Czech Republic'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Western; Chinese")] <- 'Western; China'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="western; Mali ; Bulgaria; Morocco ")] <- 'Western; Mali; Bulgaria; Morocco'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="Sami")] <- 'Norway; Finland; Russia'
weird$MusicOriginCountry[which(weird$MusicOriginCountry=="mixed")] <- 'Western'

#####MusicSource

weird$MusicSource[which(weird$MusicSource=="Othwe")]  <- 'other'
weird$MusicSource[which(weird$MusicSource=="experimenter-created; pdhymms")]<- 'semi-precomposed'
weird$MusicSource[which(weird$MusicSource=="experimenter-creater")]  <- 'experimenter-created'
weird$MusicSource[which(weird$MusicSource=="experimenter created")]  <- 'experimenter-created'
weird$MusicSource[which(weird$MusicSource=="experimenter-created*")] <- 'experimenter-created'
weird$MusicSource[which(weird$MusicSource=="experiementer-created")] <- 'experimenter-created'
weird$MusicSource[which(weird$MusicSource=="precomposed, experiementer-created")] <- 'precomposed; experimenter-created'
weird$MusicSource[which(weird$MusicSource=="Precomposed")] <- 'precomposed'
weird$MusicSource[which(weird$MusicSource=="participant")] <- 'precomposed'
weird$MusicSource[which(weird$MusicSource=="precomposed ")] <- 'precomposed'
weird$MusicSource[which(weird$MusicSource=="pre-composed")] <- 'precomposed'
weird$MusicSource[which(weird$MusicSource=="pre-composed; experimenter-created")] <- 'experimenter-created; precomposed'

weird$MusicSource[which(weird$MusicSource=="precomposed; experimenter-created")] <- 'experimenter-created; precomposed'

weird$MusicSource[which(weird$MusicSource=="")] <- NA
weird$MusicSource[which(weird$MusicSource=="NA ")] <- NA
weird$MusicSource[which(weird$MusicSource=="NA")] <- NA
weird$MusicSource[which(weird$MusicSource=="Other")] <- 'other'
weird$MusicSource[which(weird$MusicSource=="experimenter-composed")] <- 'experimenter-created'
weird$MusicSource[which(weird$MusicSource=="composed")] <- 'experimenter-created'
weird$MusicSource[which(weird$MusicSource=="qualitative study")] <- NA
weird$MusicSource[which(weird$MusicSource=="Semi-precomposed")] <- 'semi-precomposed'
weird$MusicSource[which(weird$MusicSource=="semi-composed")] <- 'semi-precomposed'
weird$MusicSource[which(weird$MusicSource=="Interview")] <- NA

##### Age

unique(weird$SampleAgeMean)
weird$SampleAgeMean[which(weird$SampleAgeMean=="")] <- NA
weird$SampleAgeMean[which(weird$SampleAgeMean=="NA")] <- NA
weird$SampleAgeMean[which(weird$SampleAgeMean=="16.78\xca")] <- "16.78"
weird$SampleAgeMean[which(weird$SampleAgeMean=="33;32.2;28.4")] <- "33; 32.2; 28.4"
weird$SampleAgeMean[which(weird$SampleAgeMean=="5 months")] <- "0.42"
weird$SampleAgeMean[which(weird$SampleAgeMean=="18-21")] <- "19.5"
weird$SampleAgeMean[which(weird$SampleAgeMean=="18-24")] <- "21"
weird$SampleAgeMean[which(weird$SampleAgeMean=="25-34")] <- "29.5"
weird$SampleAgeMean[which(weird$SampleAgeMean=="40-77")] <- "58.5"
weird$SampleAgeMean[which(weird$SampleAgeMean=="21-30")] <- "25.5"
weird$SampleAgeMean[which(weird$SampleAgeMean==">49")] <- NA
weird$SampleAgeMean[which(weird$SampleAgeMean=="20-24")] <- "22"
weird$SampleAgeMean[which(weird$SampleAgeMean=="23,10; 21,17")] <- "23.10; 21.17"
weird$SampleAgeMean[which(weird$SampleAgeMean=="32-52")] <- "42"
weird$SampleAgeMean[which(weird$SampleAgeMean=="06-Sep")] <- "7.5"
weird$SampleAgeMean[which(weird$SampleAgeMean=="42,74; 51,50; 69,52 months ")] <- "3.56; 4.29; 5.79"
weird$SampleAgeMean[which(weird$SampleAgeMean=="27,44 months")] <- "2.29"
weird$SampleAgeMean[which(weird$SampleAgeMean=="56,16 months")] <- "4.68"
weird$SampleAgeMean[which(weird$SampleAgeMean=="52,92 months")] <- "4.41"
weird$SampleAgeMean[which(weird$SampleAgeMean=="29,31; 21,81")] <- "29.31; 21.81"
weird$SampleAgeMean[which(weird$SampleAgeMean=="23,5; 24,2")] <- "23.5; 24.2"
weird$SampleAgeMean[which(weird$SampleAgeMean=="24,23; 25,34")] <- "24.23; 25.34"
weird$SampleAgeMean[which(weird$SampleAgeMean=="See comments")] <- NA
weird$SampleAgeMean[which(weird$SampleAgeMean=="19,11; 20,88")] <- "19.11; 20.88"
weird$SampleAgeMean[which(weird$SampleAgeMean=="6-7 years")] <- "6.5"
weird$SampleAgeMean[which(weird$SampleAgeMean=="30 months")] <- "2.5"
weird$SampleAgeMean[which(weird$SampleAgeMean=="Oct-16")] <- "13"

weird$SampleAgeMean[which(weird$SampleAgeMean=="30-64 y")] <- "47"
weird$SampleAgeMean[which(weird$SampleAgeMean=="4y 9m - 5y 8m")] <- "5.2"
weird$SampleAgeMean[which(weird$SampleAgeMean=="31 - 47")] <- "39"
weird$SampleAgeMean[which(weird$SampleAgeMean=="20-47")] <- "33.5"
weird$SampleAgeMean[which(weird$SampleAgeMean=="18-31")] <- "24.5"
weird$SampleAgeMean[which(weird$SampleAgeMean=="14,6 months")] <- "1.22"
weird$SampleAgeMean[which(weird$SampleAgeMean=="10; Adults")] <- "10; NA"
weird$SampleAgeMean[which(weird$SampleAgeMean=="23 (musicians); 25 (non-musicians)")] <- "23; 25"

######Sampling Method Description #########

for(i in 1:length(weird$SamplingMethodDescription)){
  weird$SamplingMethodDescription[i]<-ifelse(weird$SamplingMethodDescription[i]=="NA","NA",tolower(weird$SamplingMethodDescription[[i]]))}

weird$SamplingMethodDescription<- str_replace_all(weird$SamplingMethodDescription,"volunteers","volunteer")
weird$SamplingMethodDescription<- str_replace_all(weird$SamplingMethodDescription,"volunteered","volunteer")

weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="volunteers")]  <- 'volunteer'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="course credit or paid")]  <- 'course credit; paid'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="cinema tickets")]  <- 'other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="prize draw")]  <- 'other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="cinema ticket")]  <- 'other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="")]  <- 'volunteer'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="NA ")]  <- 'NA'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="paid (parents)")]  <- 'paid'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="course credit; raffle")]  <- 'course credit; other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="compensation")]  <- 'paid'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="compensated")]  <- 'paid'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="voluntary")]  <- 'volunteer'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="compensated; course credit")]  <- 'paid; course credit'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="voucher") ]  <- 'other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="volunteer (a prize draw was offered as incentive)")]  <- 'volunteer; other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="volunteer; (paid in sweets after each task)")]  <- 'volunteer; other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="volunteer; course credits")]  <- 'volunteer; course credit'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="na (paid?)")]  <- 'paid'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="course credit (undergraduates); volunteered")]  <- 'course credit; volunteer'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="course credit (graduate students)")]  <- 'course credit'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="volunteer; ucl pen given as incentive")]  <- 'volunteer; other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="vouchers from mothercare")]  <- 'other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="course credit (undergraduates); volunteer")]  <- 'course credit; volunteer'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="lottery for voucher")]  <- 'other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="course credit (undergraduates)")]  <- 'course credit'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="course credit; two movie tickets")]  <- 'course credit; other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="course requirment ")]  <- 'course credit'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="couse credit")]  <- 'course credit'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="na; course creit")]  <- 'NA; course credit'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="other (prize draw)")]  <- 'other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="course credit, paid")]  <- 'course credit; paid'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="course credit, volunteer")]  <- 'course credit; volunteer'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="discount on program enrollment")]  <- 'other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="mcmaster infant studies group database")]  <- 'volunteer'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="observational")]  <- 'other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="mailing list")]  <- 'volunteer'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="summer camp")]  <- 'volunteer'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="mail")]  <- 'volunteer'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="volunteer (a prize draw was offered as incentive)")]  <- 'volunteer;other'
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="volunteer; (paid in sweets after each task)")]  <- 'volunteer'
weird$SamplingMethodDescription<-str_replace_all(weird$SamplingMethodDescription,"na","NA")

weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="NA ")]  <- NA
weird$SamplingMethodDescription[which(weird$SamplingMethodDescription=="volunteer ")]  <- "volunteer"



#test <- weird[873:880,]
#test$SampleAgeMean2 <- gsub(',', '.', test$SampleAgeMean)



write.csv(weird, "WEIRD part cleaned Feb24.csv")


#write.csv(weird, "WEIRD data partially cleaned.csv")