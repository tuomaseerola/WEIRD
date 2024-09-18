# keyword_analysis.R
# WEIRD article
# T. Eerola, 23/3/2024
# Status: Complete

plotflag <- TRUE

## remove studies, keep articles (to avoid duplicating keywords)
D <- dplyr::filter(d,study_id=='study1') # 1360 articles (1360 correct!)

if(!dim(D)[1]==1360){
  print("incorrect number of observations! (Needs to be run at the level of studies, N=1360)")
  break
}

# eliminate those where the WEOG has not been defined
D <- dplyr::filter(D,!is.na(CountryDataCollected_WEOG))
sum(is.na(D$CountryDataCollected_WEOG))
dim(D) # 1221

#### 1. Specify Age -------------------------

#### 2. specify Uni Sample -------------------------
D$SampleOtherDescription[is.na(D$SampleOtherDescription)]<-'Not specified'
D$uni <- str_detect(D$SampleOtherDescription,'universi|undergrad')
D$uni <- factor(D$uni,levels = c("FALSE",'TRUE'),labels = c("others","university"))
table(D$uni)
#### 3. specify Musician -------------------------
D$SampleMusicianshipDescription[is.na(D$SampleMusicianshipDescription)]<-'Not specified'
table(D$SampleMusicianshipDescription)
D$SampleMusicianshipDescriptionBinary<-factor(D$SampleMusicianshipDescription,
                                                levels = c("musicians","musicians; non-musicians","non-musicians","Not specified"),
                                                labels = c("musicians","others","others","others"))


#### 4. specify Western -------------------------
D$MusicOriginCountry[is.na(D$MusicOriginCountry)]<-'Not specified'

D$origin<-'X'
D$origin<-paste('X',D$MusicOriginCountry)
D$origin[str_detect(D$MusicOriginCountry,'^Western$')]<- 'Western'
D$origin[str_detect(D$MusicOriginCountry,'Africa')]<- 'Non-Western'
D$origin[str_detect(D$MusicOriginCountry,'Not specified')]<- 'Not specified'
D$origin[str_detect(D$MusicOriginCountry,'Portugal')]<- 'Western'
D$origin[str_detect(D$MusicOriginCountry,'Estonia')]<- 'Western'
D$origin[str_detect(D$MusicOriginCountry,'Hungary')]<- 'Western'
D$origin[str_detect(D$MusicOriginCountry,'Spain')]<- 'Western'
D$origin[str_detect(D$MusicOriginCountry,'Australia')]<- 'Non-Western' # CHECK, if abo
D$origin[str_detect(D$origin,'X ')]<- 'Non-Western'

D$origin <- factor(D$origin,levels = c("Non-Western","Not specified","Western"),labels = c("other","other","western"))
table(D$origin)



#### keyword analysis ------------------
KW <- NULL
d_index <- NULL
WEOG_index <- NULL

u_index <- NULL
o_index <- NULL
m_index <- NULL
a_index <- NULL # age
g_index <- NULL # gender balance
year_index <- NULL

for (k in 1:nrow(D)) {
  tmp <- D$Keywords[k]
  WEOG <- as.character(D$CountryDataCollected_WEOG[k])
  u <- as.character(D$uni[k])
  o <- as.character(D$origin[k])
  m <- as.character(D$SampleMusicianshipDescriptionBinary[k])
  a <- as.character(D$SampleAgeMean[k])
  g <- as.character(D$gender_balance[k])
  y <- as.character(D$Year[k])
  tmp <- stringi::stri_trans_general(tmp,"latin-ascii")
  if(!is.na(tmp)){
    kw<-str_split(tmp,'[;,]', simplify = TRUE)
    KW<-c(KW,kw)
    d_index<-c(d_index,rep(k,length(kw)))
    u_index<-c(u_index,rep(u,length(kw)))
    o_index<-c(o_index,rep(o,length(kw)))
    m_index<-c(m_index,rep(m,length(kw)))
    a_index<-c(a_index,rep(a,length(kw)))
    g_index<-c(g_index,rep(g,length(kw)))
    year_index<-c(year_index,rep(y,length(kw)))
    WEOG_index<-c(WEOG_index,rep(WEOG,length(kw)))
  }
}

length(KW)
head(d_index)
head(WEOG_index)
head(a_index)
head(g_index)
head(year_index)

KW<-stringi::stri_trans_general(KW,"latin-ascii")

KW_O<-KW

source('scripts/simplify_keywords.R')
length(KW_O)
length(KW)
tmp<-data.frame(KW_O,KW)
head(tmp)
length(unique(KW_O))
length(unique(KW))
  
table(KW)
t<-data.frame(table(KW))
t<-dplyr::arrange(t,-Freq)
head(t,30)

#write.csv(table(KW),file = 'kw_tmp.csv')

data <- data.frame(KW,WEOG_index,a_index,u_index,m_index,o_index,g_index,year_index)
head(data)
data$a_index_n<-as.numeric(data$a_index)
data$year_index<-as.numeric(data$year_index)
data$a_index_nB<-cut(data$a_index_n,breaks = c(0,median(data$a_index_n,na.rm = T),100),labels = c('Young','Old'))
table(data$a_index_nB)

data$g_index_n<-as.numeric(data$g_index)
data$g_index_n
data$g_index_nB<-cut(data$g_index_n,breaks = c(-0.1,.5,1.1),labels = c('Male dom','Female dom'))
table(data$g_index_nB)
head(data)
table(data$o_index)
source('scripts/count2category.R')
x1 <- count2category(data, index="WEOG_index", str1="WEOG", str2="Non-WEOG")
head(x1,25)
x2 <- count2category(data, index="m_index", str1="musicians", str2="others")
head(x2,25)
x3 <- count2category(data, index="o_index", str1="western", str2="other")
head(x3,25)
x4 <- count2category(data, index="u_index", str1="university", str2="others")
head(x4,25)
# x5 <- count2category(data, index="a_index_nB", str1='Young', str2='Old')
# head(x5,25)
# x6 <- count2category(data, index="g_index_nB", str1='Male dom', str2='Female dom')
# head(x6,25)
# mean per KW
x6<-summarise(group_by(data,KW),prop=mean(g_index_n,na.rm=TRUE),Freq=n())
x6<-dplyr::arrange(x6,-Freq)
x6<-drop_na(x6)
head(x6,25)

x5<-summarise(group_by(data,KW),prop=mean(a_index_n,na.rm=TRUE),Freq=n())
x5<-dplyr::arrange(x5,-Freq)
x5<-drop_na(x5)
head(x5,25)


FROM<-1
TO<-25
head(x1)
g1a<-ggplot(data = x1[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = Freq)) + 
  geom_col(fill='grey70',color='grey10') +
  geom_text(nudge_y = .027,family = 'Times')+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.5,.5)) + 
  scale_fill_grey()+
  ylab("% WEIRD")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15,base_family = 'Times')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g1a

head(x2)
g2a<-ggplot(data = x2[FROM:TO,], aes(x = reorder(KW,prop),y=prop, label = Freq)) + 
  geom_col(fill='grey70',color='grey10') +
  geom_text(nudge_y = .027,family = 'Times')+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(0,1,.1),labels=(seq(0,1,.10))*100,limits = c(0,1)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("% Musicians")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15,base_family = 'Times')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g2a

head(x3)
g3a<-ggplot(data = x3[FROM:TO,], aes(x = reorder(KW,prop),y=prop, label = Freq)) + 
  geom_col(fill='grey70',color='grey10') +
  geom_text(nudge_y = .027,family = 'Times')+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(0,1,.1),labels=(seq(0,1,.10))*100,limits = c(0,1)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("% Western music")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15,base_family = 'Times')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g3a
head(x4)
g4a<-ggplot(data = x4[FROM:TO,], aes(x = reorder(KW,prop),y=prop, label = Freq)) + 
  geom_col(fill='grey70',color='grey10') +
  geom_text(nudge_y = .027,family = 'Times')+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(0,1,.1),labels=(seq(0,1,.10))*100,limits = c(0,1)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("% University samples")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15,base_family = 'Times')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g4a
head(x5)
Md<-median(x5$prop,na.rm=TRUE)
Md
g5a<-ggplot(data = x5[FROM:TO,], aes(x = reorder(KW,prop),y=prop-Md, label = Freq)) + 
  geom_col(fill='grey70',color='grey10') +
  geom_text(nudge_y = 0.70,family = 'Times')+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-15,15,5),limits = c(-18,17)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("Years from the median age")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15,base_family = 'Times')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g5a

head(x6,25)
#x6<-drop_na(x6)
#Md<-median(x6$prop,na.rm=TRUE)
#Md

g6a<-ggplot(data = x6[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = Freq)) + 
  geom_col(fill='grey70',color='grey10') +
  geom_text(nudge_y = .0033,family = 'Times')+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.05),labels=(seq(-.5,.5,.05)+.5)*100,limits = c(-.10,.12)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("% Female participants")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15,base_family = 'Times')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g6a

Ga <- cowplot::plot_grid(g1a,g2a,g3a,g4a,g5a,g6a,nrow = 3,ncol = 2)
print(Ga)

if(plotflag==TRUE){
  ggsave(filename = 'figure3.pdf',Ga,device = 'pdf',width = 14,height = 15)
}

#### Additional analyses ------------------
# keyword per year per WEIRD
#head(data)

# take the most common (N) keywords
N<-10
top <- summarise(group_by(data,KW),Freq=n())
#head(top)
top<-drop_na(top)
top<-dplyr::arrange(top,-Freq)
head(top,10)
top10KW <- top$KW[1:N]
top10KW
data_filtered <-dplyr::filter(data,KW %in% top10KW)

# create empty data frame
x10e<-2010:2022
x10e<-expand.grid(KW=top10KW,WEOG_index=c('Non-WEOG','WEOG'),year_index=x10e)
x10e$Freq<-0
x10 <- summarise(group_by(data_filtered,KW,WEOG_index,year_index),Freq=n())
x10<-rbind(x10,x10e)

x10<-dplyr::arrange(x10,-Freq)
#head(x10,25)
#x10<-drop_na(x10)
x10$KW<-factor(x10$KW,levels = top10KW)
x10$WEOG_index<-factor(x10$WEOG_index,levels = c('Non-WEOG','WEOG'),labels = c('Non-WEIRD','WEIRD'))
#head(x10)
#max(x10$year_index)

g10a<-ggplot(data = x10, aes(x = year_index,y=Freq,fill=WEOG_index)) + 
  geom_col(position = 'dodge',color='black') +
  facet_wrap(~KW,scales = 'free_y',nrow = 5)+
  theme(text = element_text(size=16)) +
  scale_fill_manual(name="Group",values = c('#E41A1C','#377EB8'))+
  scale_x_continuous(breaks=seq(2000,2022,2))+
  ylab("Number of articles")+
  xlab("Year")+
  theme_linedraw(base_size = 15,base_family = 'Times')+
  theme(panel.border = element_blank(), panel.grid.major = element_blank())
print(g10a)

#ggsave(filename = 'figure4.pdf',g10a,device = 'pdf',width = 16,height = 12)
