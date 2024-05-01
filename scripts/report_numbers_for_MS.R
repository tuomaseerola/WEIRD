# report_numbers_for_MS.R

# for text
# for text, weighted mean
# DF <- dplyr::filter(df,humansample==TRUE)
# dim(DF)
# D <- dplyr::filter(d,humansample==TRUE)
# dim(D)

min(DF$sample_size,na.rm = T)
max(DF$sample_size,na.rm = T)
median(DF$sample_size,na.rm = T)

mean(D$sample_agemean,na.rm = T)

tmp<-dplyr::select(DF,sample_agemean,sample_size)
tmp<-drop_na(tmp)
wtd.avg <- weighted.mean(tmp$sample_agemean, tmp$sample_size,na.rm = T)
print('Mean weighted age:')
print(wtd.avg) # 27.06

print(sum(!is.na(D$FemaleParticipantsNumber)) / nrow(D))
D$BOTHMaleAndFemaleParticipantsNumber<-D$FemaleParticipantsNumber + D$MaleParticipantsNumber
sum(D$BOTHMaleAndFemaleParticipantsNumber,na.rm = TRUE)
sum(D$FemaleParticipantsNumber,na.rm = TRUE) / sum(D$BOTHMaleAndFemaleParticipantsNumber,na.rm = TRUE) # 0.61
print('gender % female prop. (weighted):')
print(sum(D$FemaleParticipantsNumber,na.rm = TRUE) / sum(D$BOTHMaleAndFemaleParticipantsNumber,na.rm = TRUE))
#### Other numbers
DD <- dplyr::filter(d,study_id=='study1') # 1360 articles (1360 correct!)

## countries
print("First Author Country Prop:")
print(paste('US:',round(sum(DD$FirstAuthorCountry=='US',na.rm = T)/nrow(DD)*100,0)))           # 22
print(paste('UK:',round(sum(DD$FirstAuthorCountry=='UK',na.rm = T)/nrow(DD)*100,0)))           # 15
print(paste('Aus:',round(sum(DD$FirstAuthorCountry=='Australia',na.rm = T)/nrow(DD)*100,0)))   # 11
print(paste('Ger:',round(sum(DD$FirstAuthorCountry=='Germany',na.rm = T)/nrow(DD)*100,0)))     # 9
print(paste('Can:',round(sum(DD$FirstAuthorCountry=='Canada',na.rm = T)/nrow(DD)*100,0)))      # 6
print(paste('Fin:',round(sum(DD$FirstAuthorCountry=='Finland',na.rm = T)/nrow(DD)*100,0)))     # 5
print(paste('WEIRD:',round(sum(DD$FirstAuthorCountry_WEOG=='WEOG',na.rm = T)/nrow(DD)*100,0))) # 91

D <- dplyr::filter(d,humansample==TRUE)
print('Prop of human studies:')
print(round(nrow(D)/nrow(d)*100,0))

sum(is.na(D$CountryDataCollected))

print("Country Data Collected Prop:")

print(paste('US:',round(sum(D$CountryDataCollected=='US',na.rm = T)/(sum(!is.na(D$CountryDataCollected)))*100,0)))         # 23
print(paste('UK:',round(sum(D$CountryDataCollected=='UK',na.rm = T)/(sum(!is.na(D$CountryDataCollected)))*100,0)))         # 12
print(paste('Aus:',round(sum(D$CountryDataCollected=='Australia',na.rm = T)/(sum(!is.na(D$CountryDataCollected)))*100,0))) # 8
print(paste('Ger:',round(sum(D$CountryDataCollected=='Germany',na.rm = T)/(sum(!is.na(D$CountryDataCollected)))*100,0)))   # 7
print(paste('Can:',round(sum(D$CountryDataCollected=='Canada',na.rm = T)/(sum(!is.na(D$CountryDataCollected)))*100,0)))    # 7
print(paste('Fin:',round(sum(D$CountryDataCollected=='Finland',na.rm = T)/(sum(!is.na(D$CountryDataCollected)))*100,0)))   # 3
print(paste('online:',round(sum(D$CountryDataCollected=='online',na.rm = T)/(sum(!is.na(D$CountryDataCollected)))*100,0))) # 9

rm(DD,tmp,wtd.avg)
