# trends_WEOG.R
# WEIRD article
# T. Eerola, 23/3/2024

library(ggrepel)
saveplots <- FALSE

#### Temporal trends related WEO (diversity) -----------

#### 1 WEOG prop -----------
tmp <- d %>%
  select(Year,CountryDataCollected_WEOG) %>%
  drop_na() %>%
  group_by(Year) %>%
  summarise(WEOG = sum(CountryDataCollected_WEOG=='WEOG'), n = n(),prop=WEOG/n)
head(tmp)

g1<-ggplot(tmp,aes(Year,prop))+
#  geom_line()+
  geom_point()+
  geom_smooth(method = 'lm',fullrange=TRUE,se=TRUE,color='grey20')+
#  scale_y_continuous(breaks = seq(0,1,by=0.25),limits = c(0.3,1))+
  scale_y_continuous(limits=c(0.5,1),breaks = seq(0,1,by=.1),expand = c(0.005,0.085),labels = seq(0,1,by=.1)*100)+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  ylab('% Studies with Data from WEIRD')+
  theme_bw(base_size = 15,base_family = 'Times')
#g1

#### 2 First author country and WEIRD -------
articles <- dplyr::filter(d,study_id=='study1') # 1360 articles (1360 correct!)
nrow(articles)
tmp2 <- articles %>%
  select(Year,FirstAuthorCountry_WEOG) %>%
  drop_na() %>%
  group_by(Year) %>%
  summarise(WEOG = sum(FirstAuthorCountry_WEOG=='WEOG'), n = n(),prop=WEOG/n)
head(tmp2)

g2<-ggplot(tmp2,aes(Year,prop))+
  #geom_line()+
  geom_smooth(method = 'lm',fullrange=TRUE,se=TRUE,color='grey20')+
  geom_point()+
  scale_y_continuous(limits=c(0.7,1),breaks = seq(0,1,by=.1),expand = c(0.005,0.085),labels = seq(0,1,by=.1)*100)+
  #  scale_y_continuous(breaks = seq(0,1,by=0.25),limits = c(0.30,1))+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  ylab('% Articles with 1st Author from WEIRD')+
  theme_bw(base_size = 15,base_family = 'Times')
#g2

tmp3 <- DF %>%
  select(Year,CountryDataCollected_WEOG,sample_agesd) %>%
  drop_na() %>%
  group_by(Year,CountryDataCollected_WEOG) %>%
  summarise(AgeSD = median(sample_agesd,na.rm=TRUE),n=n())
#head(tmp3)

tmp3 <- dplyr::filter(tmp3,n > 5)

g3<-ggplot(tmp3,aes(Year,AgeSD,shape=CountryDataCollected_WEOG,color=CountryDataCollected_WEOG,label=n))+
  geom_point(show.legend = F)+
  geom_smooth(method = 'lm',fullrange=TRUE,se=F)+
  geom_text_repel(nudge_y = .02,show.legend = F,family="Times")+
  scale_color_brewer(name='Group',palette = 'Set1',labels=c("Non-WEIRD","WEIRD"))+
#  scale_y_continuous(limits=c(0,1),breaks = seq(0,1,by=.25),expand = c(0.005,0.085),labels = seq(0,1,by=.25)*100)+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  ylab('Age Variation (SD) in Samples')+
  theme_bw(base_size = 15,base_family = 'Times')+
  theme(legend.position="none")
#g3
 

#### 4 Gender by WEOG -----------

tmp4 <- DF %>%
  select(Year,CountryDataCollected_WEOG,sample_gender_balance) %>%
  drop_na() %>%
  group_by(Year,CountryDataCollected_WEOG) %>%
  summarise(genderprop = mean(sample_gender_balance,na.rm=TRUE),n=n())
#head(tmp4)

tmp4 <- dplyr::filter(tmp4,n > 5)
head(tmp4)
g4<-ggplot(tmp4,aes(Year,genderprop,shape=CountryDataCollected_WEOG,color=CountryDataCollected_WEOG,label=n))+
  geom_point(size=3,show.legend = F)+
  geom_text_repel(nudge_y = .02,show.legend = F,family = 'Times')+
  geom_smooth(method = 'lm',fullrange=TRUE,se=FALSE)+
  scale_color_brewer(name='Group',palette = 'Set1',labels=c("Non-WEIRD","WEIRD"))+
  scale_y_continuous(limits=c(0,1),breaks = seq(0,1,by=.25),expand = c(0.005,0.085),labels = seq(0,1,by=.25)*100)+
  #  scale_y_continuous(breaks = seq(0,1,by=0.25),limits = c(0.3,1))+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  ylab('% Female Participants')+
  theme_bw(base_size = 15,base_family = 'Times')+
  theme(legend.position="none")
g4

#### 5 UNI by WEOG -----------
#cat('\n University sample:\n')
#D <- dplyr::filter(d,humansample==TRUE)

DF$SampleOtherDescription[is.na(DF$SampleOtherDescription)]<-'Not specified'
DF$uni <- str_detect(DF$SampleOtherDescription,'universi|undergrad')
DF$uni <- factor(DF$uni,levels = c("FALSE",'TRUE'),labels = c("others","university"))

tmp5 <- DF %>%
  select(Year,CountryDataCollected_WEOG,uni) %>%
  drop_na() %>%
  group_by(Year,CountryDataCollected_WEOG) %>%
  summarise(Uni = sum(uni=='university'), n = n(),prop=Uni/n)
#head(tmp5)
tmp5 <- dplyr::filter(tmp5,n > 5)

g5<-ggplot(tmp5,aes(Year,prop,color=CountryDataCollected_WEOG,shape=CountryDataCollected_WEOG,label=n))+
  geom_point(show.legend = F)+
  geom_smooth(method = 'lm',fullrange=TRUE,se=F)+
  geom_text_repel(nudge_y = .02,show.legend = F,family="Times")+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  scale_y_continuous(limits=c(0,1),breaks = seq(0,1,by=.25),expand = c(0.005,0.085),labels = seq(0,1,by=.25)*100)+
  scale_color_brewer(name='Group', palette = 'Set1',labels=c("Non-WEIRD","WEIRD"))+
  ylab('% University Participants')+
  theme_bw(base_size = 15,base_family = 'Times')+
  theme(legend.position=c(.80, .82),legend.key = element_blank())+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  theme(legend.background=element_rect(fill = alpha("white", 0.1)))
  #theme(legend.position="top")
g5

#### 6 Stimulus Origin by WEOG -----------
DM <- dplyr::filter(d,musicstudies==TRUE)
dim(DM)
DM<-ungroup(DM)
table(DM$MusicOriginCountry)
DM$MusicOriginCountry[is.na(DM$MusicOriginCountry)]<-'Not specified'
table(DM$MusicOriginCountry)

# Western = Purely Western
# Check!

DM$origin<-'X'
DM$origin<-paste('X',DM$MusicOriginCountry)
DM$origin[str_detect(DM$MusicOriginCountry,'^Western$')]<- 'Western'
DM$origin[str_detect(DM$MusicOriginCountry,'Africa')]<- 'Non-Western'
DM$origin[str_detect(DM$MusicOriginCountry,'Not specified')]<- 'Not specified'
DM$origin[str_detect(DM$MusicOriginCountry,'Portugal')]<- 'Western'
DM$origin[str_detect(DM$MusicOriginCountry,'Estonia')]<- 'Western'
DM$origin[str_detect(DM$MusicOriginCountry,'Hungary')]<- 'Western'
DM$origin[str_detect(DM$MusicOriginCountry,'Spain')]<- 'Western'
DM$origin[str_detect(DM$MusicOriginCountry,'Australia')]<- 'Non-Western' # CHECK, if abo
DM$origin[str_detect(DM$origin,'X ')]<- 'Non-Western'

table(DM$origin,DM$MusicOriginCountry)
# x<-data.frame(DM$MusicOriginCountry,DM$origin)
# x
table(DM$origin)
DM$origin <- factor(DM$origin,levels = c("Non-Western","Not specified","Western"),labels = c("other","other","western"))
table(DM$origin)/nrow(DM)

tmp6 <- DM %>%
  select(Year,CountryDataCollected_WEOG,origin) %>%
  drop_na() %>%
  group_by(Year,CountryDataCollected_WEOG) %>%
  summarise(Origin = sum(origin=='western'), n = n(),prop=Origin/n)
head(tmp6)

tmp6 <- dplyr::filter(tmp6,n > 5)

g6<-ggplot(tmp6,aes(Year,prop,color=CountryDataCollected_WEOG,shape=CountryDataCollected_WEOG,label=n))+
  geom_point(show.legend = F)+
  geom_text_repel(nudge_y = .02,show.legend = F,family="Times")+
  geom_smooth(method = 'lm',fullrange=TRUE,se=F)+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  scale_y_continuous(limits=c(0,1),breaks = seq(0,1,by=.25),expand = c(0.005,0.085),labels = seq(0,1,by=.25)*100)+
  scale_color_brewer(name='Group', palette = 'Set1',labels=c("Non-WEIRD","WEIRD"))+
  ylab('% Studies with Western Music')+
  theme_bw(base_size = 15,base_family = 'Times')+
  theme(legend.position="none")
#g6

#### Combine -----------

G <- cowplot::plot_grid(g1,g2,g3,g4,g6,g5,nrow = 3)
print(G)

if(saveplots==TRUE){
  ggsave(filename = 'figure2.pdf',G,device = 'pdf',height = 12,width = 12)
}

#### Clean ---------
rm(list = ls()[grep("^tmp", ls())])
rm(list = ls()[grep("^g", ls())])
rm(ind,t,Md,G)
