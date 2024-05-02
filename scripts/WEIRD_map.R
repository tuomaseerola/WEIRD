saveplots <- FALSE

country_freq_samples <- dplyr::summarise(group_by(d,CountryDataCollected,CountryDataCollected_WEOG),s = n())
head(country_freq_samples)
country_freq_samples$s<-0
country_freq_samples$s[country_freq_samples$CountryDataCollected_WEOG=='WEOG']<-1
country_freq_samples
country_freq_samples<-dplyr::select(country_freq_samples,-CountryDataCollected_WEOG)
head(country_freq_samples)

#### 2 COUNTRY DATA COLLECTED --------
# country_freq_samples <- dplyr::summarise(group_by(df,sample_country_data_collected),s=sum(sample_size,na.rm = T))
# country_freq_samples
colnames(country_freq_samples)<-c('CountryName','N')
country_freq_samples$cown <- countrycode(country_freq_samples$CountryName,"country.name",'ecb')
head(country_freq_samples)
#country_freq_samples<-dplyr::filter(country_freq_samples,N>0)
country_freq_samples<-drop_na(country_freq_samples)
# Merge WEIRD and MAP data
country_freq_samples$N<-factor(country_freq_samples$N)
map2 <- merge(map, country_freq_samples, by.x = "COWcode", by.y = "cown", all = TRUE)

target_crs <- "+proj=eqearth +wktext"
map2_eqea <- st_transform(map2, target_crs)

# constrained display window
disp_win_wgs84 <- st_sfc(st_point(c(-137, -54)),
                         st_point(c(160, 100)),
                         crs = 4326)
# full map
disp_win_wgs84 <- st_sfc(st_point(c(-180, -90)),
                         st_point(c(180, 90)),
                         crs = 4326)
disp_win_eqea <- st_transform(disp_win_wgs84, target_crs)

labs <- c("Non-WEIRD","WEIRD","No data")
g5 <- ggplot(data = map2_eqea) +
  geom_sf(aes(fill = N), 
          position = "identity") + 
    geom_sf_label(aes(label=name),label.size = 0,size=1,fill=NA)+
  #  geom_sf_label(aes(label=Freq))+
  scale_fill_manual(name="Country Status",labels=labs,values=c('tomato','cyan3','palegreen'))+
  xlab('')+
  ylab('')+
  theme_minimal(base_size = 15,base_family = 'Times')+
  theme(plot.title = element_text(hjust = 0.5))
print(g5)  
