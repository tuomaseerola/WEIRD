# keyword_analysis.R
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



plotflag <- TRUE

table(d$FirstAuthorCountry,d$FirstAuthorCountry_WEOG)
sum(table(d$FirstAuthorCountry,d$FirstAuthorCountry_WEOG))

## remove studies, keep articles (to avoid duplicating keywords)
D <- dplyr::filter(d,study_id=='study1') # 1360 articles (1360 correct!)

if(!dim(D)[1]==1360){
  print("incorrect number of observations! (Needs to be run at the level of studies, N=1360)")
  break
}

# delete studies where CountryDataCollected is not defined
D<-dplyr::filter(D,!is.na(CountryDataCollected))
dim(D)

use_USD_as_WEOG <- TRUE
if(use_USD_as_WEOG==TRUE){
  D$CountryDataCollected_WEOG_orig <- D$CountryDataCollected_WEOG
  # optional: replace WEOG with USD median split
  # USDC refers to primaryCountryDataCollected
  D$CountryDataCollected_WEOG<-D$CountryDataCollected_USD_Md
  D$CountryDataCollected_WEOG<-factor(D$CountryDataCollected_WEOG,levels = c("Western","Non-Western"),labels = c("WEOG","Non-WEOG"))
  table(D$CountryDataCollected_WEOG,D$CountryDataCollected_WEOG_orig)
  
  D$FirstAuthorCountry_WEOG<-D$FirstAuthorCountry_USD_Md
  D$FirstAuthorCountry_WEOG<-factor(D$FirstAuthorCountry_WEOG,levels = c("Western","Non-Western"),labels = c("WEOG","Non-WEOG"))
}
# eliminate those where the WEOG has not been defined
D <- dplyr::filter(D,!is.na(CountryDataCollected_WEOG))
sum(is.na(D$CountryDataCollected_WEOG))
dim(D) # 1123

KW<-NULL
d_index <- NULL
WEOG_index <- NULL
for (k in 1:nrow(D)) {
  tmp <- D$Keywords[k]
  WEOG <- as.character(D$CountryDataCollected_WEOG[k])
  tmp <- stringi::stri_trans_general(tmp,"latin-ascii")
  if(!is.na(tmp)){
    kw<-str_split(tmp,'[;,]', simplify = TRUE)
    KW<-c(KW,kw)
    d_index<-c(d_index,rep(k,length(kw)))
    WEOG_index<-c(WEOG_index,rep(WEOG,length(kw)))
  }
}
head(d_index)
head(WEOG_index)
length(unique(d_index))

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

length(KW)
length(d_index)
tmp<-data.frame(KW,WEOG_index)
head(tmp)
tmp1<-dplyr::filter(tmp,WEOG_index=='WEOG')
dim(tmp1)
tmp2<-dplyr::filter(tmp,WEOG_index=='Non-WEOG')
head(tmp2)
dim(tmp2)

freq1<-data.frame(table(tmp1$KW))
head(freq1)
freq1<-dplyr::arrange(freq1,-Freq)
head(freq1)
tail(freq1)
freq2<-data.frame(table(tmp2$KW))
freq2<-dplyr::arrange(freq2,-Freq)
head(freq2)

KW_unique<-unique(KW)
which(KW_unique=='representativeness heuristic')

freq_table<-data.frame(KW=KW_unique,WEOG=rep(0,length(KW_unique)),nonWEOG=rep(0,length(KW_unique)))
head(freq_table)
rownames(freq_table)<-NULL
length(KW_unique)
for (k in 1:length(KW_unique)) {
#  print(k)
  x1 <- freq1$Freq[which(freq1$Var1==KW_unique[k])]
  if(length(x1)>0){
    freq_table$WEOG[k] <- as.integer(x1)
  }
  x2 <- freq2$Freq[which(freq2$Var1==KW_unique[k])]
  if(length(x2)>0){
    freq_table$nonWEOG[k] <- as.integer(x2)
  }
}
freq_table<-dplyr::arrange(freq_table,-nonWEOG)
#head(freq_table,20)
#tail(freq_table)

freq_table$Freq<-freq_table$WEOG+freq_table$nonWEOG
freq_table$prop<-freq_table$WEOG/freq_table$Freq
head(freq_table)
freq_table<-dplyr::arrange(freq_table,-nonWEOG)
head(freq_table)

## New: NOrmalise size within WEOG AND non-WEOG
freq_table$WEOG_R<-freq_table$WEOG/max(freq_table$WEOG)
freq_table$nonWEOG_R<-freq_table$nonWEOG/max(freq_table$nonWEOG)
freq_table$joint_norm_freq <- (freq_table$nonWEOG_R + freq_table$WEOG_R) / 2
head(freq_table)
freq_table<-dplyr::arrange(freq_table,-joint_norm_freq)
freq_table$prop_RS<-scales::rescale(freq_table$prop,to=c(0,90))
freq_table$prop_RS<- 45-freq_table$prop_RS
head(freq_table,50)

freq_table$prop_RS<-scales::rescale(freq_table$prop^3.3,from=c(0,1), to=c(-45,45))
N<-55+15
data<-freq_table[2:N,]
median(data$prop_RS)
mean(data$prop_RS)
hist(data$prop_RS)
mean(data$prop)
median(data$prop)


if(plotflag==TRUE){
  library(ggwordcloud)
  set.seed(42)

  g5<-ggplot(data, aes(label = KW,size=joint_norm_freq,color=prop_RS,angle=prop_RS)) +
    geom_text_wordcloud(area_corr = TRUE) +
    scale_size_area(max_size = 50, trans = power_trans(1/.7)) +
    scale_color_gradient2(low ="firebrick1",mid = "gray10",high = "blue",midpoint = -2.311)+
    theme_minimal()
  g5

  ggsave(filename = 'wordcloud_no_music_WEOG_R2_60.pdf',device = "pdf",g5,height = 6,width = 7.5)

}


rm(D,data,freq_table,freq1,freq2)

