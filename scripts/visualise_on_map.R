# visualise_on_map.R
# Visualise some properties on a global map
# WEIRD article
# T. Eerola, 23/3/2024
# Status: Complete

saveplots <- TRUE

#### get unique studies ------
D <- dplyr::filter(d,study_id=='study1') # 1360 articles (1360 correct!)
nrow(D)
#### FIGURE: FIRST AUTHOR COUNTRIES --------
# remove double country affiliations 
D$FirstAuthorCountry<-str_remove_all(D$FirstAuthorCountry,';.*$')
D$FirstAuthorCountry<-str_remove_all(D$FirstAuthorCountry,';.*$')
table(D$FirstAuthorCountry)

#### get only unique studies ------
#D <- dplyr::filter(d,study_id=='study1')
country_freq<-data.frame(table(D$FirstAuthorCountry))
colnames(country_freq)<-c('CountryName','Freq')

# Cite this
# Arel-Bundock, Vincent, Nils Enevoldsen, and CJ Yetman, (2018). countrycode: An R package to convert country names and country codes. Journal of Open Source Software, 3(28), 848, https://doi.org/10.21105/joss.00848

country_freq$cown <- countrycode(country_freq$CountryName,"country.name",'ecb')
# Merge WEIRD and MAP data
map1 <- merge(map, country_freq, by.x = "COWcode", by.y = "cown", all = TRUE)
map1$formal_en[is.na(map1$COWcode)]
table(map1$COWcode)
# remove these marginal regions
#map1 <- tidyr::drop_na(map1,COWcode)
dim(map1)

library(ggthemes)
library(viridis)

# Projection
target_crs <- "+proj=eqearth +wktext"
map1_eqea <- st_transform(map1, target_crs)
# constrained display window
disp_win_wgs84 <- st_sfc(st_point(c(-137, -54)),
                         st_point(c(160, 100)),
                         crs = 4326)
# full map
disp_win_wgs84 <- st_sfc(st_point(c(-180, -90)),
                         st_point(c(180, 90)),
                         crs = 4326)
disp_win_eqea <- st_transform(disp_win_wgs84, target_crs)

#### 0: plot empty map for checking ----------
# Projection
target_crs <- "+proj=eqearth +wktext"
map0_eqea <- st_transform(map, target_crs)

# constrained display window
disp_win_wgs84 <- st_sfc(st_point(c(-30, -24)),
                         st_point(c(60, 40)),
                         crs = 4326)

## Step 4: transform the display window in the new coordinate system.
disp_win_trans <- st_transform(disp_win_wgs84, crs = target_crs)
#### Step 5: retrieve the window coordinates in the new coordinate system
disp_win_coord <- st_coordinates(disp_win_trans)

#### 1 First author country map -----------
sum(D$FirstAuthorCountry=='US')
g1 <- ggplot(data = map1_eqea) +
  geom_sf(aes(fill = Freq), 
          position = "identity") + 
labs(fill='Frequency')  +
  scale_fill_viridis(option="plasma", na.value='grey',trans='log',breaks=c(1, 5, 20, 80, 280))+
#    ggthemes::theme_map()+
  ggtitle('Number of Articles Per Country (1st Author Affiliation)')+
  theme_minimal(base_size = 15,base_family = 'Times')+
  theme(plot.title = element_text(hjust = 0.5))
g1

if(saveplots==TRUE){
  ggsave(filename = 'articles_country_freq.pdf',g1,device = 'pdf',height = 6,width = 9)
}

#### 2 COUNTRY DATA COLLECTED Aggregated Sample sizes--------
nrow(d)
country_freq_samples <- dplyr::summarise(group_by(d,CountryDataCollected),s=sum(SampleSize,na.rm = T))
#head(country_freq_samples)
colnames(country_freq_samples)<-c('CountryName','N')
country_freq_samples$cown <- countrycode(country_freq_samples$CountryName,"country.name",'ecb')
#head(country_freq_samples)
country_freq_samples<-dplyr::filter(country_freq_samples,N>0)
country_freq_samples<-drop_na(country_freq_samples)
# Merge WEIRD and MAP data
map2 <- merge(map, country_freq_samples, by.x = "COWcode", by.y = "cown", all = TRUE)

# diagnostics
map2$formal_en[is.na(map2$COWcode)]

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

g2 <- ggplot(data = map2_eqea) +
  geom_sf(aes(fill = N), 
          position = "identity") + 
  labs(fill='Summed N')  +
  scale_fill_viridis(option="viridis", na.value='grey',trans='log',breaks=c(50,500,5000,50000))+
  ggtitle('Summed No. of Participants Per Country Data Collected')+
  theme_minimal(base_size = 15,base_family = 'Times')+
  theme(plot.title = element_text(hjust = 0.5))
g2

if(saveplots==TRUE){
  ggsave(filename = 'pooled_N_country_freq.pdf',g2,device = 'pdf',height = 6,width = 9)
}

#### 3. Articles per Year -------
DD <- dplyr::filter(d,study_id=='study1') # 1360 articles (1360 correct!)
nrow(DD)
DD$Year<-factor(DD$Year)
t<-table(DD$Year)
t<-data.frame(t)
colnames(t)<-c('Year','Count')
#t$Year<-as.numeric(levels(t$Year))[t$Year]
#t$Year<-factor(t$Year,labels = seq(10,22))
g3<-ggplot(t,aes(x=Year,y=Count))+
  geom_col(fill='grey85',colour='black')+
  scale_y_continuous(limits = c(0,200),expand = c(0.01,0.01))+
  ggtitle('Number of publications by year')+
  theme_bw(base_size = 13,base_family = 'Times')+
  theme(plot.title = element_text(hjust = 0.5))
g3

#### 4 Sample size  -------
max(as.numeric(d$SampleSize),na.rm=TRUE)
#4096*14
#hist(log(d$SampleSize))
tmp<-dplyr::select(d,SampleSize)
tmp<-drop_na(tmp)
Md<-median(tmp$SampleSize)
Md
g4a<-ggplot(tmp,aes(x=as.numeric(SampleSize)))+
  geom_histogram(bins=55,fill='grey85',colour='black')+
#  scale_x_continuous(trans = 'log',breaks = c(1,4,16,64,256,1024,4096,4096*4,4096*14),limits = c(0.5,60000),expand = c(0.01,0.01))+
  scale_x_continuous(trans = 'log',breaks = c(1,4,16,64,256,1024,4096,4096*4),expand = c(0.01,0.01))+
  scale_y_continuous(limits = c(0,120),expand = c(0.01,0.01))+
  xlab('Sample Size (N) on log scale')+
  geom_vline(xintercept = Md,linetype='dashed')+
  annotate("text", x=Md+4,y=110,label='Median (50)',hjust=0)+
  ylab('Count')+
  ggtitle('Sample size')+
  theme_bw(base_size = 14,base_family = 'Times')+
  theme(plot.title = element_text(hjust = 0.5))
g4a

# without log
g4b<-ggplot(tmp,aes(x=as.numeric(SampleSize)))+
  geom_histogram(binwidth = 10,fill='grey85',colour='black')+
  scale_x_continuous(limits = c(0.5,500),breaks = seq(0,500,by=50),expand = c(0.01,0.01))+
  scale_y_continuous(limits = c(0,NA),expand = c(0.01,0.01))+
  xlab('Sample Size (N)')+
  ylab('Count')+
  ggtitle('Sample size')+
  theme_bw(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5))
g4b

ind<-as.numeric(D$SampleSize)>1000
sum(ind==TRUE,na.rm = T)

library(cowplot)
G <- cowplot::plot_grid(g1,g3,g2,g4a,rel_widths = c(1.45,1),rel_heights = c(1,1),labels = "AUTO")
G

if(saveplots==TRUE){
  ggsave(filename = 'figure1.pdf',device = 'pdf',G,width = 13,height = 8)
}

G2 <- cowplot::plot_grid(g1,g3,g5,g4a,rel_widths = c(1.45,1),rel_heights = c(1,1),labels = "AUTO")
G2

if(saveplots==TRUE){
  ggsave(filename = 'figure1_alt.pdf',device = 'pdf',G2,width = 13,height = 8)
}

#### Clean --------------
rm(list = ls()[grep("^map", ls())])
rm(list = ls()[grep("^dis", ls())])
rm(list = ls()[grep("^coun", ls())])
rm(G2)
