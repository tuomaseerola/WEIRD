# keyword_analysis_multiple.R
# WEIRD article
# T. Eerola, 23/3/2024
# Status: Complete

####### TO DO
#I've only just thought of this when I saw this note I wrote here. We currently analyse the keywords in 
# relation to Western/non-Western countries. Could we consider also analysing by other demographic factors? 
#   For instance, certain subtopics are more likely than others to be investigated across a wide age range, 
# to be investigated only in university samples, to be investigated more in musicians than non-musicians, 
# and using more Western music stimuli. Do you think we could explore these options a bit more? This also 
# relates to what we discussed about trying to make the narrative based on diversity more broadly and not just 
# 'West vs the rest'
#

plotflag <- FALSE

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

for (k in 1:nrow(D)) {
  tmp <- D$Keywords[k]
  WEOG <- as.character(D$CountryDataCollected_WEOG[k])
  u <- as.character(D$uni[k])
  o <- as.character(D$origin[k])
  m <- as.character(D$SampleMusicianshipDescriptionBinary[k])
  a <- as.character(D$SampleAgeMean[k])
  g <- as.character(D$gender_balance[k])
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
    WEOG_index<-c(WEOG_index,rep(WEOG,length(kw)))
  }
}

head(d_index)
head(WEOG_index)
head(a_index)
head(g_index)

KW<-stringi::stri_trans_general(KW,"latin-ascii")

KW<-str_remove_all(KW,'^ ') # remove leading spaces
KW<-tolower(KW)
KW<-str_remove_all(KW,'ê') # remove garbage
KW<-str_remove_all(KW,'�') # remove garbage
KW<-str_remove_all(KW,'s$') # remove plurals
KW<-str_remove_all(KW,'ology$') # Methodology to method
KW<-str_remove_all(KW,'^\n') # Methodology to method
KW<-str_replace_all(KW,'stres','stress') # remove plurals
KW<-str_replace_all(KW,'sadnes','sadness') # remove plurals
KW<-str_replace_all(KW,'analysi','analysis') # remove plurals
KW<-str_replace_all(KW,'loudnes','loudness') # remove plurals
KW<-str_replace_all(KW,'aesthetic','aesthetics') # remove plurals
KW<-str_replace_all(KW,'psychophysi','psychophysiology') # remove plurals
KW<-stringi::stri_trans_general(KW,"latin-ascii")

data <- data.frame(KW,WEOG_index,a_index,u_index,m_index,o_index,g_index)
head(data)
data$a_index_n<-as.numeric(data$a_index)
data$a_index_nB<-cut(data$a_index_n,breaks = c(0,median(data$a_index_n,na.rm = T),100),labels = c('Young','Old'))

data$g_index_n<-as.numeric(data$g_index)
data$g_index_n
data$g_index_nB<-cut(data$g_index_n,breaks = c(-0.1,.5,1.1),labels = c('Male dom','Female dom'))
table(data$g_index_nB)
head(data)
table(data$o_index)
source('scripts/count2category.R')
x1 <- count2category(data, index="WEOG_index", str1="WEOG", str2="Non-WEOG")
head(x1)
x2 <- count2category(data, index="m_index", str1="musicians", str2="others")
head(x2)
x3 <- count2category(data, index="o_index", str1="western", str2="other")
head(x3)
x4 <- count2category(data, index="u_index", str1="university", str2="others")
head(x4)
x5 <- count2category(data, index="a_index_nB", str1='Young', str2='Old')
head(x5)
x6 <- count2category(data, index="g_index_nB", str1='Male dom', str2='Female dom')
head(x6)
# mean per KW
x6<-summarise(group_by(data,KW),prop=mean(g_index_n,na.rm=TRUE),Freq=n())
x6<-dplyr::arrange(x6,-Freq)
head(x6)

x5<-summarise(group_by(data,KW),prop=mean(a_index_n,na.rm=TRUE),Freq=n())
x5<-dplyr::arrange(x5,-Freq)
head(x5)


FROM<-2
TO<-26
head(x1)
g1a<-ggplot(data = x1[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = Freq)) + 
  geom_col(fill='lightblue4',color='grey10') +
  geom_text(nudge_y = .02)+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.5,.5)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("% WEIRD")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15)
g1a

g1b<-ggplot(data = x1[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = KW,size = Freq)) + 
  geom_text(nudge_y = .02,show.legend = FALSE,hjust=0)+
#  geom_point()+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(0,.55)) + 
  scale_fill_brewer(palette="Dark2")+
  scale_size_area(max_size = 10)+
  scale_x_discrete(breaks = c(-1,100))+
  ylab("% WEIRD")+
  xlab("")+
  theme_minimal(base_size = 15)
g1b

head(x2)
g2a<-ggplot(data = x2[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = Freq)) + 
  geom_col(fill='lightblue4',color='grey10') +
  geom_text(nudge_y = .02)+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.5,.5)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("% Musicians")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15)
g2a

g2b<-ggplot(data = x2[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = KW,size = Freq)) + 
  geom_text(nudge_y = .02,show.legend = FALSE,hjust=0)+
  #  geom_point()+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.5,.67)) + 
  scale_fill_brewer(palette="Dark2")+
  scale_size_area(max_size = 10)+
  scale_x_discrete(breaks = c(-1,100))+
  ylab("% Musicians")+
  xlab("")+
  theme_minimal(base_size = 15)
g2b


head(x3)
g3a<-ggplot(data = x3[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = Freq)) + 
  geom_col(fill='lightblue4',color='grey10') +
  geom_text(nudge_y = .02)+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.5,.5)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("% Western")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15)
g3a

g3b<-ggplot(data = x3[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = KW,size = Freq)) + 
  geom_text(nudge_y = .02,show.legend = FALSE,hjust=0)+
  #  geom_point()+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.5,.67)) + 
  scale_fill_brewer(palette="Dark2")+
  scale_size_area(max_size = 10)+
  scale_x_discrete(breaks = c(-1,100))+
  ylab("% Western music examples")+
  xlab("")+
  theme_minimal(base_size = 15)
g3b


head(x4)
g4a<-ggplot(data = x4[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = Freq)) + 
  geom_col(fill='lightblue4',color='grey10') +
  geom_text(nudge_y = .02)+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.5,.5)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("% University samples")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15)
g4a

g4b<-ggplot(data = x4[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = KW,size = Freq)) + 
  geom_text(nudge_y = .02,show.legend = FALSE,hjust=0)+
  #  geom_point()+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.5,.67)) + 
  scale_fill_brewer(palette="Dark2")+
  scale_size_area(max_size = 10)+
  scale_x_discrete(breaks = c(-1,100))+
  ylab("% University samples")+
  xlab("")+
  theme_minimal(base_size = 15)
g4b

head(x5)
Md<-median(x5$prop,na.rm=TRUE)
g5a<-ggplot(data = x5[FROM:TO,], aes(x = reorder(KW,prop),y=prop-Md, label = Freq)) + 
  geom_col(fill='lightblue4',color='grey10') +
  geom_text(nudge_y = 1)+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-10,10,5),limits = c(-10,12)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("Years from the median age")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15)
g5a

g5b<-ggplot(data = x5[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = KW,size = Freq)) + 
  geom_text(nudge_y = .02,show.legend = FALSE,hjust=0)+
  #  geom_point()+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.5,.67)) + 
  scale_fill_brewer(palette="Dark2")+
  scale_size_area(max_size = 10)+
  scale_x_discrete(breaks = c(-1,100))+
  ylab("% Age above median")+
  xlab("Keyword (ranked)")+
  theme_minimal(base_size = 15)
g5b

head(x6)
#Md<-median(x6$prop,na.rm=TRUE)
#Md
g6a<-ggplot(data = x6[FROM:TO,], aes(x = reorder(KW,prop),y=prop-.5, label = Freq)) + 
  geom_col(fill='lightblue4',color='grey10') +
  geom_text(nudge_y = .02)+
  coord_flip() +
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.2,.2)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("% Females")+
  xlab("Keyword (ranked)")+
  theme_linedraw(base_size = 15)
g6a

Ga<-cowplot::plot_grid(g1a,g2a,g3a,g4a,g5a,g6a,nrow = 3,ncol = 2)
Ga
ggsave(filename = 'keyword_combination_ver1_R1.pdf',Ga,device = 'pdf',width = 14,height = 15)

Gb<-cowplot::plot_grid(g1b,g2b,g3b,g4b)
Gb
ggsave(filename = 'keyword_combination_ver2.pdf',Gb,device = 'pdf',width = 14,height = 10)

#### As a table --------
head(x1)
X1 = x1[FROM:TO,c(1,4,5)]
X1$Rank<-1:nrow(X1)
X1$Concept<-'WEIRD'
dim(X1)
head(X1)

X2 = x2[FROM:TO,c(1,4,5)]
X2$Rank<-1:nrow(X2)
X2$Concept<-'Musicians'

X3 = x3[FROM:TO,c(1,4,5)]
X3$Rank<-1:nrow(X3)
X3$Concept<-'Western musical samples'

X4 = x4[FROM:TO,c(1,4,5)]
X4$Rank<-1:nrow(X4)
X4$Concept<-'University samples'
head(X4)

X<-rbind(X1,X2,X3,X4)
head(X)
head(X1)
X
head(X)
ga<-ggplot(data = X, aes(x = reorder(KW,prop),y=prop-.5, label = Freq)) + 
  geom_col(fill='lightblue4',color='grey10') +
  geom_text(nudge_y = .02)+
  coord_flip() +
  facet_wrap(.~Concept,scales = "free_x")+
  theme(text = element_text(size=16)) +
  scale_y_continuous(breaks=seq(-.5,.5,.1),labels=(seq(-.5,.5,.10)+.5)*100,limits = c(-.5,.5)) + 
  scale_fill_brewer(palette="Dark2")+
  ylab("%")+
  xlab("")+
  theme_linedraw(base_size = 15)
ga

ggsave(filename = 'keyword_combination_ver3.pdf',ga,device = 'pdf',width = 14,height = 10)
head(X)
X$prop<-X$prop*100
Xw <- pivot_wider(X,id_cols = c(KW,Freq),values_from = c(prop),names_from = Concept)
head(Xw)
knitr::kable(Xw,digits = 2)

rm(D,data,freq_table,freq1,freq2)

