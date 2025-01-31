# trends_WEOG.R
# WEIRD article
# T. Eerola, 23/3/2024

load(file='data/WEIRD_data.Rdata')

library(ggrepel)
source('scripts/printp.R')
saveplots <- TRUE

#### Temporal trends related WEO (diversity) -----------

#### 1 WEOG prop -----------
#table(d$CountryDataCollected,d$CountryDataCollected_WEOG)
#table(df$CountryDataCollected,df$CountryDataCollected_WEOG)

tmp <- df %>%
  select(Year,CountryDataCollected_WEOG) %>%
  drop_na() %>%
  group_by(Year) %>%
  summarise(WEOG = sum(CountryDataCollected_WEOG=='WEOG'), n = n(),prop=WEOG/n)
head(tmp)

s1 <- cor.test(tmp$Year,tmp$prop)
subplot1_stat <- paste0('italic(p) == ',printp(s1$p.value))

g1<-ggplot(tmp,aes(Year,prop))+
  geom_point()+
  geom_smooth(method = 'lm',fullrange=TRUE,se=TRUE,color='grey20')+
#  scale_y_continuous(breaks = seq(0,1,by=0.25),limits = c(0.3,1))+
  scale_y_continuous(limits=c(0.7,1),breaks = seq(0,1,by=.1),expand = c(0.005,0.085),labels = seq(0,1,by=.1)*100)+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  ylab('% WEIRD Participant Samples')+
  annotate("text",x=2016,y=0.96,label=subplot1_stat,parse=TRUE,family="Times",size=5)+
  theme_bw(base_size = 14,base_family = 'Times')
g1

#### 2 First author country and WEIRD -------
articles <- dplyr::filter(d,study_id=='study1') # 1360 articles (1360 correct!)
nrow(articles)
tmp2 <- articles %>%
  select(Year,FirstAuthorCountry_WEOG) %>%
  drop_na() %>%
  group_by(Year) %>%
  summarise(WEOG = sum(FirstAuthorCountry_WEOG=='WEOG'), n = n(),prop=WEOG/n)
head(tmp2)

s2 <- cor.test(tmp2$Year,tmp2$prop)
subplot2_stat <- paste0('italic(p) == ',printp(s2$p.value))
subplot2_stat
g2<-ggplot(tmp2,aes(Year,prop))+
  #geom_line()+
  geom_smooth(method = 'lm',fullrange=TRUE,se=TRUE,color='grey20')+
  geom_point()+
  scale_y_continuous(limits=c(0.7,1),breaks = seq(0,1,by=.1),expand = c(0.005,0.085),labels = seq(0,1,by=.1)*100)+
  #  scale_y_continuous(breaks = seq(0,1,by=0.25),limits = c(0.30,1))+
  annotate("text",x=2016,y=0.980,label=subplot2_stat,parse=TRUE,family="Times",size=5)+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  ylab('% Articles with 1st Author from WEIRD')+
  theme_bw(base_size = 14,base_family = 'Times')
#g2

tmp3 <- DF %>%
  select(Year,CountryDataCollected_WEOG,sample_agesd) %>%
  drop_na() %>%
  group_by(Year,CountryDataCollected_WEOG) %>%
  summarise(AgeSD = median(sample_agesd,na.rm=TRUE),n=n())
#head(tmp3)

#tmp3 <- dplyr::filter(tmp3,n > 5)

s3W <- cor.test(tmp3$Year[tmp3$CountryDataCollected_WEOG=='WEOG'],tmp3$AgeSD[tmp3$CountryDataCollected_WEOG=='WEOG'])
subplot3_statW <- paste0('italic(p) == ',printp(s3W$p.value))

s3NW <- cor.test(tmp3$Year[tmp3$CountryDataCollected_WEOG=='Non-WEOG'],tmp3$AgeSD[tmp3$CountryDataCollected_WEOG=='Non-WEOG'])
subplot3_statNW <- paste0('italic(p) == ',printp(s3NW$p.value))

g3<-ggplot(tmp3,aes(Year,AgeSD,shape=CountryDataCollected_WEOG,color=CountryDataCollected_WEOG,label=n))+
  geom_point(show.legend = F)+
  geom_smooth(mapping = aes(weight = n),method = 'lm',fullrange=TRUE,se=F)+
  geom_text_repel(nudge_y = .02,show.legend = F,family="Times")+
  scale_color_brewer(name='Group',palette = 'Set1',labels=c("Non-WEIRD","WEIRD"))+
#  scale_y_continuous(limits=c(0,1),breaks = seq(0,1,by=.25),expand = c(0.005,0.085),labels = seq(0,1,by=.25)*100)+
  annotate("text",x=2016,y=4.85,label=subplot3_statW,parse=TRUE,family="Times",size=5,color="#377EB8")+
  annotate("text",x=2016,y=2.15,label=subplot3_statNW,parse=TRUE,family="Times",size=5,color="#E41A1C")+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  ylab('Age Variation (SD) in Samples')+
  theme_bw(base_size = 14,base_family = 'Times')+
  theme(legend.position="none")
#g3

#### 4 Gender by WEOG -----------

tmp4 <- DF %>%
  select(Year,CountryDataCollected_WEOG,sample_gender_balance) %>%
  drop_na() %>%
  group_by(Year,CountryDataCollected_WEOG) %>%
  summarise(genderprop = mean(sample_gender_balance,na.rm=TRUE),n=n())
#head(tmp4)

#tmp4 <- dplyr::filter(tmp4,n > 5)

s4W <- cor.test(tmp4$Year[tmp4$CountryDataCollected_WEOG=='WEOG'],tmp4$genderprop[tmp4$CountryDataCollected_WEOG=='WEOG'])
subplot4_statW <- paste0('italic(p) == ',printp(s4W$p.value))

s4NW <- cor.test(tmp4$Year[tmp4$CountryDataCollected_WEOG=='Non-WEOG'],tmp4$genderprop[tmp4$CountryDataCollected_WEOG=='Non-WEOG'])
subplot4_statNW <- paste0('italic(p) == ',printp(s4NW$p.value))

g4<-ggplot(tmp4,aes(Year,genderprop,shape=CountryDataCollected_WEOG,color=CountryDataCollected_WEOG,label=n))+
  geom_point(size=3,show.legend = F)+
  geom_text_repel(nudge_y = .02,show.legend = F,family = 'Times')+
  geom_smooth(mapping = aes(weight = n),method = 'lm',fullrange=TRUE,se=FALSE)+
  scale_color_brewer(name='Group',palette = 'Set1',labels=c("Non-WEIRD","WEIRD"))+
  scale_y_continuous(limits=c(0,1),breaks = seq(0,1,by=.25),expand = c(0.005,0.085),labels = seq(0,1,by=.25)*100)+
  #  scale_y_continuous(breaks = seq(0,1,by=0.25),limits = c(0.3,1))+
  annotate("text",x=2016,y=0.69,label=subplot4_statW,parse=TRUE,family="Times",size=5,color="#377EB8")+
  annotate("text",x=2016,y=0.41,label=subplot4_statNW,parse=TRUE,family="Times",size=5,color="#E41A1C")+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  ylab('% Female Participants')+
  theme_bw(base_size = 14,base_family = 'Times')+
  theme(legend.position="none")
#g4


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
head(tmp5)
#tmp5 <- dplyr::filter(tmp5,n > 5)

s5W <- cor.test(tmp5$Year[tmp5$CountryDataCollected_WEOG=='WEOG'],tmp5$prop[tmp5$CountryDataCollected_WEOG=='WEOG'])
subplot5_statW <- paste0('italic(p) == ',printp(s5W$p.value))

s5NW <- cor.test(tmp5$Year[tmp5$CountryDataCollected_WEOG=='Non-WEOG'],tmp5$prop[tmp5$CountryDataCollected_WEOG=='Non-WEOG'])
subplot5_statNW <- paste0('italic(p) == ',printp(s5NW$p.value))

g5<-ggplot(tmp5,aes(Year,prop,color=CountryDataCollected_WEOG,shape=CountryDataCollected_WEOG,label=n))+
  geom_point(show.legend = F)+
  geom_smooth(mapping = aes(weight = n), method = 'lm',fullrange=TRUE,se=F)+
  geom_text_repel(nudge_y = .02,show.legend = F,family="Times")+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  scale_y_continuous(limits=c(0,1.024),breaks = seq(0,1,by=.25),expand = c(0.005,0.085),labels = seq(0,1,by=.25)*100)+
  scale_color_brewer(name='Group', palette = 'Set1',labels=c("Non-WEIRD","WEIRD"))+
  ylab('% University Participants')+
  theme_bw(base_size = 15,base_family = 'Times')+
  annotate("text",x=2016,y=0.30,label=subplot5_statW,parse=TRUE,family="Times",size=5,color="#377EB8")+
  annotate("text",x=2016,y=0.57,label=subplot5_statNW,parse=TRUE,family="Times",size=5,color="#E41A1C")+
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
#head(tmp6)

#tmp6 <- dplyr::filter(tmp6,n > 5)

s6W <- cor.test(tmp6$Year[tmp6$CountryDataCollected_WEOG=='WEOG'],tmp6$prop[tmp6$CountryDataCollected_WEOG=='WEOG'])
subplot6_statW <- paste0('italic(p) == ',printp(s6W$p.value))

s6NW <- cor.test(tmp6$Year[tmp6$CountryDataCollected_WEOG=='Non-WEOG'],tmp6$prop[tmp6$CountryDataCollected_WEOG=='Non-WEOG'])
subplot6_statNW <- paste0('italic(p) == ',printp(s6NW$p.value))

g6<-ggplot(tmp6,aes(Year,prop,color=CountryDataCollected_WEOG,shape=CountryDataCollected_WEOG,label=n))+
  geom_point(show.legend = F)+
  geom_text_repel(nudge_y = .02,show.legend = F,family="Times")+
  geom_smooth(mapping = aes(weight = n), method = 'lm',fullrange=TRUE,se=F)+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  scale_y_continuous(limits=c(0,1.02),breaks = seq(0,1,by=.25),expand = c(0.001,0.085),labels = seq(0,1,by=.25)*100)+
  scale_color_brewer(name='Group', palette = 'Set1',labels=c("Non-WEIRD","WEIRD"))+
  annotate("text",x=2015.5,y=0.82,label=subplot6_statW,parse=TRUE,family="Times",size=5,color="#377EB8")+
  annotate("text",x=2015.5,y=0.41,label=subplot6_statNW,parse=TRUE,family="Times",size=5,color="#E41A1C")+
  ylab('% Studies with Western Music')+
  theme_bw(base_size = 14,base_family = 'Times')+
  theme(legend.position="none")
g6

# new graph 7
# Just below ‘% Studies with Western Music’ subplot add
# ‘% Studies with Unspecified Music Origin Country’ subplot with ‘Not specified’ data 
# from the variable MusicOriginCountry.

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

table(DM$origin)
DM$origin <- factor(DM$origin,levels = c("Non-Western","Not specified","Western"),labels = c("Specified","Unspecified","Specified"))
table(DM$origin)/nrow(DM)

tmp7 <- DM %>%
  select(Year,CountryDataCollected_WEOG,origin) %>%
  drop_na() %>%
  group_by(Year,CountryDataCollected_WEOG) %>%
  summarise(Origin = sum(origin=='Unspecified'), n = n(),prop=Origin/n)
head(tmp7)

#tmp6 <- dplyr::filter(tmp6,n > 5)

s7W <- cor.test(tmp7$Year[tmp7$CountryDataCollected_WEOG=='WEOG'],tmp7$prop[tmp7$CountryDataCollected_WEOG=='WEOG'])
subplot7_statW <- paste0('italic(p) == ',printp(s7W$p.value))

s7NW <- cor.test(tmp7$Year[tmp7$CountryDataCollected_WEOG=='Non-WEOG'],tmp7$prop[tmp7$CountryDataCollected_WEOG=='Non-WEOG'])
subplot7_statNW <- paste0('italic(p) == ',printp(s7NW$p.value))

g7<-ggplot(tmp7,aes(Year,prop,color=CountryDataCollected_WEOG,shape=CountryDataCollected_WEOG,label=n))+
  geom_point(show.legend = F)+
  geom_text_repel(nudge_y = .02,show.legend = F,family="Times")+
  geom_smooth(mapping = aes(weight = n), method = 'lm',fullrange=TRUE,se=F)+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  scale_y_continuous(limits=c(0,1.02),breaks = seq(0,1,by=.25),expand = c(0.001,0.085),labels = seq(0,1,by=.25)*100)+
  scale_color_brewer(name='Group', palette = 'Set1',labels=c("Non-WEIRD","WEIRD"))+
  annotate("text",x=2015.5,y=0.82,label=subplot7_statW,parse=TRUE,family="Times",size=5,color="#377EB8")+
  annotate("text",x=2015.5,y=0.55,label=subplot7_statNW,parse=TRUE,family="Times",size=5,color="#E41A1C")+
  ylab('% Studies with Unspecified Music Origin')+
  theme_bw(base_size = 14,base_family = 'Times')+
  theme(legend.position="none")
#g7


# new graph 8
# Just below subplot of ‘% University Participants’ add subplot of ‘% Samples with Description Unspecified’ with ‘Not specified’ data from the variable SampleOtherDescription.

DF$SampleOtherDescription[is.na(DF$SampleOtherDescription)]<-'Not specified'

table(DF$SampleOtherDescription)
sum(table(DF$SampleOtherDescription))
DF$SampleOtherDescription[DF$SampleOtherDescription!='Not specified']<-'Defined'


table(DF$SampleOtherDescription)
table(DF$SampleOtherDescription)/nrow(DF)

tmp8 <- DF %>%
  select(Year,CountryDataCollected_WEOG,SampleOtherDescription) %>%
  drop_na() %>%
  group_by(Year,CountryDataCollected_WEOG) %>%
  summarise(Origin = sum(SampleOtherDescription=='Not specified'), n = n(),prop=Origin/n)
head(tmp8)

s8W <- cor.test(tmp8$Year[tmp8$CountryDataCollected_WEOG=='WEOG'],tmp8$prop[tmp8$CountryDataCollected_WEOG=='WEOG'])
subplot8_statW <- paste0('italic(p) == ',printp(s8W$p.value))

s8NW <- cor.test(tmp8$Year[tmp8$CountryDataCollected_WEOG=='Non-WEOG'],tmp8$prop[tmp8$CountryDataCollected_WEOG=='Non-WEOG'])
subplot8_statNW <- paste0('italic(p) == ',printp(s8NW$p.value))

g8<-ggplot(tmp8,aes(Year,prop,color=CountryDataCollected_WEOG,shape=CountryDataCollected_WEOG,label=n))+
  geom_point(show.legend = F)+
  geom_text_repel(nudge_y = .02,show.legend = F,family="Times")+
  geom_smooth(mapping = aes(weight = n), method = 'lm',fullrange=TRUE,se=F)+
  scale_x_continuous(breaks = seq(2010,2022,by=2),expand = c(0.005,0.085))+
  scale_y_continuous(limits=c(0,1.02),breaks = seq(0,1,by=.25),expand = c(0.001,0.085),labels = seq(0,1,by=.25)*100)+
  scale_color_brewer(name='Group', palette = 'Set1',labels=c("Non-WEIRD","WEIRD"))+
  annotate("text",x=2015.5,y=0.82,label=subplot8_statW,parse=TRUE,family="Times",size=5,color="#377EB8")+
  annotate("text",x=2015.5,y=0.61,label=subplot8_statNW,parse=TRUE,family="Times",size=5,color="#E41A1C")+
  ylab('% Samples with Description Unspecified')+
  theme_bw(base_size = 14,base_family = 'Times')+
  theme(legend.position="none")
#g8


#### Combine -----------

G <- cowplot::plot_grid(g1,g2,g3,g4,g6,g5,g7,g8,nrow = 4,labels = "AUTO")
print(G)
if(saveplots==TRUE){
  ggsave(filename = 'figure2_R2b.pdf',G,device = 'pdf',height = 17,width = 12,dpi = 300)
}

#### Report linear trends -------------
cat('Report linear trends for the sub-plots:\n')
cat('Sub-plot 1:\n')
cat(subplot1_stat)
cat('\nSub-plot 2:\n')
cat(subplot2_stat)
cat('\nSub-plot 3:\n')
cat(subplot3_statW)
cat('\n') 
cat(subplot3_statNW)
cat('\nSub-plot 4:\n')
cat(subplot4_statW)
cat('\n') 
cat(subplot4_statNW)
cat('\nSub-plot 5:\n') 
cat(subplot6_statW)
cat('\n') 
cat(subplot6_statNW)
cat('\nSub-plot 6:\n')
cat(subplot5_statW)
cat('\n') 
cat(subplot5_statNW)

cat('\nSub-plot 7:\n')
cat(subplot7_statW)
cat('\n') 
cat(subplot7_statNW)


cat('\nSub-plot 8:\n')
cat(subplot8_statW)
cat('\n') 
cat(subplot8_statNW)


#### Clean ---------
rm(list = ls()[grep("^tmp", ls())])
rm(list = ls()[grep("^g", ls())])
rm(ind,t,Md,G)
