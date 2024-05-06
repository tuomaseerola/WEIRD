# table1.R
# compare basic properties across WEIRD and non-WEIRD countries
# WEIRD article
# T. Eerola, 23/3/2024
# Status: Complete

#### 0. Define functions ---------
library(Hmisc)
library(DescTools)
library(weights)
library(boot)
library(papaja)

my.function = function(data,index){
  d = data[index,]  #create bootstrap sample of all columns of original data?
  return(weighted.mean(d$counts, d$weights))  #calculate weighted mean using 'counts' and 'weights' columns
}



#### ROW 2 Sample Size -------------------------
#cat('\n Sample size:\n')
tmp<-dplyr::select(DF,sample_country_data_collected_WEOG,sample_size)
table(tmp$sample_country_data_collected_WEOG)
tmp<-drop_na(tmp)
#print(table(tmp$sample_country_data_collected_WEOG))
row2a <- tmp %>% 
  nest(data = -"sample_country_data_collected_WEOG") %>%
  mutate(ci = map(data, ~ MedianCI(.x$sample_size, method = "boot", R = 1000))) %>% 
  unnest_wider(ci)
#print(row2a)
print(knitr::kable(row2a[,c(1,3:5)],digits=2,caption='Sample Size'))

row2a_p<-wilcox.test(sample_size~sample_country_data_collected_WEOG,data=tmp)
p<-apa_print(row2a_p)
print(p$statistic)

#### ROW 3 Age M -------------------------
#cat('\n Age Mean:\n')

tmp<-dplyr::select(DF,sample_country_data_collected_WEOG,sample_agemean,sample_size)
tmp<-drop_na(tmp)
colnames(tmp)<-c('cat','counts','weights')
#### Weighted by sample size
x<-tmp %>%
  group_split(cat) %>% 
  purrr::map_dfr(
    function(x){
      wtd.avg <- weighted.mean(x$counts, x$weights)
      basic <- boot.ci(boot(x, my.function, R = 1000), type = "basic")$basic
      CI.LL <- basic[4]
      CI.UL <- basic[5]
      data.frame(wtd.avg, CI.LL, CI.UL)
    }
  )
rownames(x)<-c('Non-WEIRD','WEIRD')
print(knitr::kable(x,digits=2,caption='Age Mean'))

W<-dplyr::filter(tmp,cat=='WEOG')
NW<-dplyr::filter(tmp,cat=='Non-WEOG')
weighted.mean(W$counts,W$weights)
nrow(W)
weighted.mean(NW$counts,NW$weights)
nrow(NW)
w <- wtd.t.test(x=W$counts,y=NW$counts,weight = W$weights,weighty = NW$weights,bootse = T,bootp = TRUE,bootn = 1000,samedata = FALSE)
print(paste0('t value = ',round(w$coefficients[1],2),
                  ', df = ',round(w$coefficients[2],2),
             ', p-value = ',round(w$coefficients[3],3)))

#### ROW4 Age SD --------
cat('\n Age SD:\n')
tmp<-dplyr::select(DF,sample_country_data_collected_WEOG,sample_agesd,sample_size)
tmp<-drop_na(tmp)
colnames(tmp)<-c('cat','counts','weights')
#table(tmp$cat)
#### Weighted by sample size
x<-tmp %>%
  group_split(cat) %>% 
  purrr::map_dfr(
    function(x){
      wtd.avg <- weighted.mean(x$counts, x$weights)
      basic <- boot.ci(boot(x, my.function, R = 1000), type = "basic")$basic
      CI.LL <- basic[4]
      CI.UL <- basic[5]
      data.frame(wtd.avg, CI.LL, CI.UL)
    }
  )
rownames(x)<-c('Non-WEIRD','WEIRD')
print(knitr::kable(x,digits=2,caption='Age SD'))

W<-dplyr::filter(tmp,cat=='WEOG')
NW<-dplyr::filter(tmp,cat=='Non-WEOG')
nrow(W); nrow(NW)
w <- wtd.t.test(x=W$counts,y=NW$counts,weight = W$weights,weighty = NW$weights,bootse = T,bootp = TRUE,bootn = 1000,samedata = FALSE)
print(paste0('t value = ',round(w$coefficients[1],2),
             ', df = ',round(w$coefficients[2],2),
             ', p-value = ',round(w$coefficients[3],3)))

#### ROW5 sample_gender_balance -------
cat('\n Gender balance (country data collected):\n')

tmp<-dplyr::select(DF,sample_country_data_collected_WEOG,sample_gender_balance,sample_size)
#head(tmp)
tmp<-drop_na(tmp)
#table(tmp$CountryDataCollected_WEOG)
colnames(tmp)<-c('cat','counts','weights')
#head(tmp)
#nrow(tmp)
#### Weighted by sample size
x<-tmp %>%
  group_split(cat) %>% 
  purrr::map_dfr(
    function(x){
      wtd.avg <- weighted.mean(x$counts, x$weights)
      basic <- boot.ci(boot(x, my.function, R = 1000), type = "basic")$basic
      CI.LL <- basic[4]
      CI.UL <- basic[5]
      data.frame(wtd.avg, CI.LL, CI.UL)
    }
  )
rownames(x)<-c('Non-WEIRD','WEIRD')
print(knitr::kable(x,digits=2,caption='Gender balance (Primary Country Data Collected)'))

W<-dplyr::filter(tmp,cat=='WEOG')
NW<-dplyr::filter(tmp,cat=='Non-WEOG')
nrow(W); nrow(NW)
w <- wtd.t.test(x=W$counts,y=NW$counts,weight = W$weights,weighty = NW$weights,bootse = T,bootp = TRUE,bootn = 1000,samedata = FALSE)
print(paste0('t value = ',round(w$coefficients[1],2),
             ', df = ',round(w$coefficients[2],2),
             ', p-value = ',round(w$coefficients[3],3)))

cat('\n Gender balance (based on first author country):\n')

# Same with first author's country of affiliation
tmp<-dplyr::select(DF,FirstAuthorCountry_WEOG,sample_gender_balance,sample_size)
tmp<-drop_na(tmp)
table(tmp$FirstAuthorCountry_WEOG)
colnames(tmp)<-c('cat','counts','weights')
#### Weighted by sample size
x<-tmp %>%
  group_split(cat) %>% 
  purrr::map_dfr(
    function(x){
      wtd.avg <- weighted.mean(x$counts, x$weights)
      basic <- boot.ci(boot(x, my.function, R = 1000), type = "basic")$basic
      CI.LL <- basic[4]
      CI.UL <- basic[5]
      data.frame(wtd.avg, CI.LL, CI.UL)
    }
  )

rownames(x)<-c('Non-WEIRD','WEIRD')
print(knitr::kable(x,digits=2,caption='Gender balance (Primary Author Country)'))

W<-dplyr::filter(tmp,cat=='WEOG')
NW<-dplyr::filter(tmp,cat=='Non-WEOG')
nrow(W); nrow(NW)
w <- wtd.t.test(x=W$counts,y=NW$counts,weight = W$weights,weighty = NW$weights,bootse = T,bootp = TRUE,bootn = 1000,samedata = FALSE)
print(paste0('t value = ',round(w$coefficients[1],2),
             ', df = ',round(w$coefficients[2],2),
             ', p-value = ',round(w$coefficients[3],3)))

#### ROW6 1: solely musicians --------
cat('\n Solely musicians:\n')

tmp <- dplyr::select(DF,sample_country_data_collected_WEOG,SampleMusicianshipDescription)
#dim(tmp) # 
tmp$SampleMusicianshipDescription[is.na(tmp$SampleMusicianshipDescription)]<-'Not specified'
#head(tmp,20)
#table(tmp$SampleMusicianshipDescription)

# Report overall
t<-table(tmp$SampleMusicianshipDescription)/sum(table(tmp$SampleMusicianshipDescription))*100
t<-data.frame(t); colnames(t)<-c("Expertise","%")
print(knitr::kable(t,digits=1,caption = 'Musical expertise across samples'))

tmp$SampleMusicianshipDescriptionBinary<-factor(tmp$SampleMusicianshipDescription,
                                                levels = c("musicians","musicians; non-musicians","non-musicians","Not specified"),
                                                labels = c("musicians","others","others","others"))
t<-data.frame(table(tmp$SampleMusicianshipDescriptionBinary)/sum(table(tmp$SampleMusicianshipDescriptionBinary)))
t
expertise1 <- tmp %>% 
  filter(sample_country_data_collected_WEOG=='WEOG') %>%
  count(SampleMusicianshipDescriptionBinary, .drop = F) %>%
  rename(var = SampleMusicianshipDescriptionBinary) %>%
  mutate(var = ordered(var, levels = c("musicians", "others")), label = n)
expertise1

expertise2 <- tmp %>% 
  filter(sample_country_data_collected_WEOG=='Non-WEOG') %>%
  count(SampleMusicianshipDescriptionBinary, .drop = F) %>%
  rename(var = SampleMusicianshipDescriptionBinary) %>%
  mutate(var = ordered(var, levels = c("musicians", "others")), label = n)
expertise2

MultinomCI(expertise1$n,conf.level=0.95,method="sisonglaz")
expertise1 <- cbind(expertise1, 
                MultinomCI(expertise1$n,
                           conf.level=0.95,
                           method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise1$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))

print(paste0(as.character(expertise1$var[1]),' n=',expertise1$n[1],': ',expertise1$string[1]))

MultinomCI(expertise2$n,conf.level=0.95,method="sisonglaz")
expertise2 <- cbind(expertise2, 
                    MultinomCI(expertise2$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise2$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
print(paste0(as.character(expertise2$var[1]),' n=',expertise2$n[1],': ',expertise2$string[1]))

expertise1$label<-'WEIRD'
expertise2$label<-'Non-WEIRD'
exp<-rbind(expertise1,expertise2)
print(knitr::kable(exp[,1:6],digits = 2,caption = 'Solely musicians'))

t<-table(tmp$SampleMusicianshipDescriptionBinary,tmp$sample_country_data_collected_WEOG)
t
set.seed(42)
x<-chisq.test(tmp$SampleMusicianshipDescriptionBinary,tmp$sample_country_data_collected_WEOG,simulate.p.value = TRUE)
print(paste('Chi = ', round(x$statistic,2), ', p-value = ', x$p.value))


#### ROW6 2: musicians and musicians and non-musicians --------
cat('\n musicians and non-musicians:\n')

tmp <- dplyr::select(DF,sample_country_data_collected_WEOG,SampleMusicianshipDescription)
#tmp <- dplyr::select(DF,CountryDataCollected_WEOG,SampleMusicianshipDescription)
tmp$SampleMusicianshipDescription[is.na(tmp$SampleMusicianshipDescription)]<-'Not specified'
tmp<-drop_na(tmp)

#sum(table(tmp$SampleMusicianshipDescription))
#table(tmp$SampleMusicianshipDescription)
#sum(table(tmp$SampleMusicianshipDescription))
tmp$SampleMusicianshipDescriptionBinary<-factor(tmp$SampleMusicianshipDescription,
                                                levels = c("musicians","musicians; non-musicians","non-musicians","Not specified"),
                                                labels = c("musicians","musicians","non-musicians","non-musicians"))
tmp<-drop_na(tmp)



expertise1 <- tmp %>% 
  filter(sample_country_data_collected_WEOG=='WEOG') %>%
  count(SampleMusicianshipDescriptionBinary, .drop = F) %>%
  rename(var = SampleMusicianshipDescriptionBinary) %>%
  mutate(var = ordered(var, levels = c("musicians", "non-musicians")), label = n)
expertise1

expertise2 <- tmp %>% 
  filter(sample_country_data_collected_WEOG=='Non-WEOG') %>%
  count(SampleMusicianshipDescriptionBinary, .drop = F) %>%
  rename(var = SampleMusicianshipDescriptionBinary) %>%
  mutate(var = ordered(var, levels = c("musicians", "non-musicians")), label = n)
expertise2


MultinomCI(expertise1$n,conf.level=0.95,method="sisonglaz")
expertise1 <- cbind(expertise1, 
                    MultinomCI(expertise1$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise1$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise1
print(paste0(as.character(expertise1$var[1]),' n=', expertise1$n[1],': ',expertise1$string[1]))


MultinomCI(expertise2$n,conf.level=0.95,method="sisonglaz")
expertise2 <- cbind(expertise2, 
                    MultinomCI(expertise2$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise2$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise2
print(paste0(as.character(expertise2$var[1]),' n=',expertise2$n[1],': ',expertise2$string[1]))

expertise1$label<-'WEIRD'
expertise2$label<-'Non-WEIRD'
exp<-rbind(expertise1,expertise2)
print(knitr::kable(exp[,1:6],digits = 2,caption = 'musicians and non-musicians'))

t<-table(tmp$SampleMusicianshipDescriptionBinary,tmp$sample_country_data_collected_WEOG)
#t
set.seed(42)
x<-chisq.test(tmp$SampleMusicianshipDescriptionBinary,tmp$sample_country_data_collected_WEOG,simulate.p.value = T)
print(paste('Chi = ', round(x$statistic,2), ', p-value = ', x$p.value))

#### ROW6 3: non-musicians --------
cat('\n Solely Non-musicians:\n')
tmp <- dplyr::select(DF,sample_country_data_collected_WEOG,SampleMusicianshipDescription)
tmp$SampleMusicianshipDescription[is.na(tmp$SampleMusicianshipDescription)]<-'Not specified'
tmp<-drop_na(tmp)
sum(table(tmp$SampleMusicianshipDescription))

# Diagnostics
#print(knitr::kable(table(tmp$SampleMusicianshipDescription)))
#print(knitr::kable(table(tmp$SampleMusicianshipDescription)/sum(table(tmp$SampleMusicianshipDescription))*100,digits = 1))
#knitr::kable(table(tmp$SampleMusicianshipDescription,tmp$sample_country_data_collected_WEOG),digits = 1)

tmp$SampleMusicianshipDescriptionBinary<-factor(tmp$SampleMusicianshipDescription,
                                                levels = c("musicians","musicians; non-musicians","non-musicians","Not specified"),
                                                labels = c("others","others","non-musicians","others"))
table(tmp$SampleMusicianshipDescriptionBinary)
table(tmp$SampleMusicianshipDescriptionBinary,tmp$sample_country_data_collected_WEOG)
sum(table(tmp$SampleMusicianshipDescriptionBinary,tmp$sample_country_data_collected_WEOG))
tmp<-drop_na(tmp)

expertise1 <- tmp %>% 
  filter(sample_country_data_collected_WEOG=='WEOG') %>%
  count(SampleMusicianshipDescriptionBinary, .drop = F) %>%
  rename(var = SampleMusicianshipDescriptionBinary) %>%
  mutate(var = ordered(var, levels = c("others", "non-musicians")), label = n)
expertise1

expertise2 <- tmp %>% 
  filter(sample_country_data_collected_WEOG=='Non-WEOG') %>%
  count(SampleMusicianshipDescriptionBinary, .drop = F) %>%
  rename(var = SampleMusicianshipDescriptionBinary) %>%
  mutate(var = ordered(var, levels = c("others", "non-musicians")), label = n)
expertise2


MultinomCI(expertise1$n,conf.level=0.95,method="sisonglaz")
expertise1 <- cbind(expertise1, 
                    MultinomCI(expertise1$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise1$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise1
print(paste0(as.character(expertise1$var[2]),' n=',expertise1$n[2],': ',expertise1$string[2]))

MultinomCI(expertise2$n,conf.level=0.95,method="sisonglaz")
expertise2 <- cbind(expertise2, 
                    MultinomCI(expertise2$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise2$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise2
print(paste0(as.character(expertise2$var[2]),' n=',expertise2$n[2],': ',expertise2$string[2]))

expertise1$label<-'WEIRD'
expertise2$label<-'Non-WEIRD'
exp<-rbind(expertise1,expertise2)
print(knitr::kable(exp[,1:6],digits = 2,caption = 'non-musicians'))

t<-table(tmp$SampleMusicianshipDescriptionBinary,tmp$sample_country_data_collected_WEOG)
#t
set.seed(42)
x<-chisq.test(tmp$SampleMusicianshipDescriptionBinary,tmp$sample_country_data_collected_WEOG,simulate.p.value = T)
print(paste('Chi = ', round(x$statistic,2), ', p-value = ', x$p.value))

#### ROW 7 University Sample -------
cat('\n University sample:\n')
DF$SampleOtherDescription[is.na(DF$SampleOtherDescription)]<-'Not specified'

#table(D$SampleOtherDescription)
sum(table(DF$SampleOtherDescription))
sum(table(DF$sample_country_data_collected_WEOG))
table(DF$SampleOtherDescription,DF$sample_country_data_collected_WEOG)
table(DF$sample_country_data_collected,DF$sample_country_data_collected_WEOG)

DF$uni <- str_detect(DF$SampleOtherDescription,'universi|undergrad')
DF$uni <- factor(DF$uni,levels = c("FALSE",'TRUE'),labels = c("others","university"))
table(DF$uni)
## university student
# DF$uni <- str_detect(DF$SampleOtherDescription,'universi')
# t<-table(DF$uni)
# t<-round(t/sum(t)*100)
# t
# ## university student
# DF$uni <- str_detect(DF$SampleOtherDescription,'undergra')
# t<-table(DF$uni)
# t<-round(t/sum(t)*100)
# t
# DF$uni <- str_detect(DF$SampleOtherDescription,'chil|infan')
# t<-table(DF$uni)
# t<-round(t/sum(t)*100)
# t
# DF$uni <- str_detect(DF$SampleOtherDescription,'chil|infan|undergra|univer')
# t<-table(DF$uni)
# t<-round(t/sum(t)*100)
# t

tmp <- dplyr::select(DF,sample_country_data_collected_WEOG,uni)
tmp<-drop_na(tmp)

table(tmp$sample_country_data_collected_WEOG,tmp$uni)
expertise1 <- tmp %>% 
  filter(sample_country_data_collected_WEOG=='WEOG') %>%
  count(uni, .drop = F) %>%
  rename(var = uni) %>%
  mutate(var = ordered(var, levels = c("others", "university")), label = n)
expertise1

expertise2 <- tmp %>% 
  filter(sample_country_data_collected_WEOG=='Non-WEOG') %>%
  count(uni, .drop = F) %>%
  rename(var = uni) %>%
  mutate(var = ordered(var, levels = c("others", "university")), label = n)
expertise2

expertise1 <- cbind(expertise1, 
                    MultinomCI(expertise1$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise1$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise1
print(paste0(as.character(expertise1$var[2]),' n=',expertise1$n[2],': ',expertise1$string[2]))

expertise2 <- cbind(expertise2, 
                    MultinomCI(expertise2$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise2$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise2
print(paste0(as.character(expertise2$var[2]),' n=',expertise2$n[2],': ',expertise2$string[2]))

expertise1$label<-'WEIRD'
expertise2$label<-'Non-WEIRD'
exp<-rbind(expertise1,expertise2)
print(knitr::kable(exp[,1:6],digits = 2,caption = 'University samples'))


t<-table(tmp$uni,tmp$sample_country_data_collected_WEOG)
t
set.seed(42)
x<-chisq.test(tmp$uni,tmp$sample_country_data_collected_WEOG,simulate.p.value = T)
print(paste('Chi = ', round(x$statistic,2), ', p-value = ', x$p.value))

#### ROW 8 Recruitment ----------
cat('\n Recruitment volunteer:\n')

DF$SamplingMethodDescription <- str_replace(DF$SamplingMethodDescription,' ','')
DF$SamplingMethodDescription[is.na(DF$SamplingMethodDescription)]<-'Not specified'
table(DF$SamplingMethodDescription)
DF$incentive<-''
DF$incentive[str_detect(DF$SamplingMethodDescription,'credit|paid')]<- 'Paid'
DF$incentive[str_detect(DF$SamplingMethodDescription,'^volunteer$')]<- 'Volunteer'
DF$incentive[str_detect(DF$SamplingMethodDescription,'^volunteer;other$')]<- 'Volunteer'
DF$incentive[str_detect(DF$SamplingMethodDescription,'^other$|^other;other$')]<- 'Other'
DF$incentive[str_detect(DF$SamplingMethodDescription,'Not specified')]<- 'Not specified'
round(table(DF$incentive)/nrow(DF)*100)
DF$incentive <- factor(DF$incentive,levels = c("Not specified","Other","Paid","Volunteer"),labels = c("others","others","others","volunteer"))
table(DF$incentive)
table(DF$incentive,DF$sample_country_data_collected_WEOG)

tmp<-dplyr::select(DF,sample_country_data_collected_WEOG,incentive)
tmp<-drop_na(tmp)
table(tmp$sample_country_data_collected_WEOG,tmp$incentive)
expertise1 <- tmp %>% 
  filter(sample_country_data_collected_WEOG=='WEOG') %>%
  count(incentive, .drop = F) %>%
  rename(var = incentive) %>%
  mutate(var = ordered(var, levels = c("others", "volunteer")), label = n)
expertise1

expertise2 <- tmp %>% 
  filter(sample_country_data_collected_WEOG=='Non-WEOG') %>%
  count(incentive, .drop = F) %>%
  rename(var = incentive) %>%
  mutate(var = ordered(var, levels = c("others", "volunteer")), label = n)
expertise2

expertise1 <- cbind(expertise1, 
                    MultinomCI(expertise1$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise1$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise1
print(paste0(as.character(expertise1$var[2]),' n=',expertise1$n[2],': ',expertise1$string[2]))

expertise2 <- cbind(expertise2, 
                    MultinomCI(expertise2$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise2$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise2
print(paste0(as.character(expertise2$var[2]),' n=',expertise2$n[2],': ',expertise2$string[2]))

expertise1$label<-'WEIRD'
expertise2$label<-'Non-WEIRD'
exp<-rbind(expertise1,expertise2)
print(knitr::kable(exp[,1:6],digits = 2,caption = 'Volunteer samples'))


t<-table(tmp$incentive,tmp$sample_country_data_collected_WEOG)
t
x<-chisq.test(tmp$incentive,tmp$sample_country_data_collected_WEOG)
print(paste('Chi = ', round(x$statistic,2), ', p-value = ', x$p.value))

#### ROW 9 Experimenter Created Music ----------
cat('\n Experimenter Created Music:\n')
DM <- dplyr::filter(d,musicstudies==TRUE)
dim(DM)
DM<-ungroup(DM)

DM$MusicSource[is.na(DM$MusicSource)]<-'Not specified'
table(DM$MusicSource)

DM$source<-paste('X',DM$MusicSource)
DM$source[str_detect(DM$MusicSource,'experimenter')]<- 'Experimenter'
DM$source[str_detect(DM$MusicSource,'comp')]<- 'Composed'
DM$source[str_detect(DM$MusicSource,'Not specified')]<- 'Not specified'
DM$source[str_detect(DM$MusicSource,'^other$')]<- 'Other'
table(DM$source)
DM$source <- factor(DM$source,
                   levels = c("Composed","Experimenter","Not specified","Other"),
                   labels = c("other","experimenter","other","other"))
table(DM$source)
table(DM$source,DM$CountryDataCollected_WEOG)

tmp<-dplyr::select(DM,CountryDataCollected_WEOG,source)
tmp<-drop_na(tmp)
table(tmp$CountryDataCollected_WEOG,tmp$source)
expertise1 <- tmp %>% 
  filter(CountryDataCollected_WEOG=='WEOG') %>%
  count(source, .drop = F) %>%
  rename(var = source) %>%
  mutate(var = ordered(var, levels = c("other", "experimenter")), label = n)
expertise1

expertise2 <- tmp %>% 
  filter(CountryDataCollected_WEOG=='Non-WEOG') %>%
  count(source, .drop = F) %>%
  rename(var = source) %>%
  mutate(var = ordered(var, levels = c("other", "experimenter")), label = n)
expertise2

expertise1 <- cbind(expertise1, 
                    MultinomCI(expertise1$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise1$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise1
print(paste0(as.character(expertise1$var[2]),' n=',expertise1$n[2],': ',expertise1$string[2]))

expertise2 <- cbind(expertise2, 
                    MultinomCI(expertise2$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise2$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise2
print(paste0(as.character(expertise2$var[2]),' n=',expertise2$n[2],': ',expertise2$string[2]))

expertise1$label<-'WEIRD'
expertise2$label<-'Non-WEIRD'
exp<-rbind(expertise1,expertise2)
print(knitr::kable(exp[,1:6],digits = 2,caption = 'Experimenter selected. music'))


t<-table(tmp$source,tmp$CountryDataCollected_WEOG)
t
x<-chisq.test(tmp$source,tmp$CountryDataCollected_WEOG)
print(paste('Chi = ', round(x$statistic,2), ', p-value = ', x$p.value))

#### ROW 10: Music Origin ----------
cat('\n Western music:\n')
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

#table(D$origin,D$MusicOriginCountry)
#x<-data.frame(D$MusicOriginCountry,D$origin)
#x

DM$origin <- factor(DM$origin,levels = c("Non-Western","Not specified","Western"),labels = c("other","other","western"))
table(DM$origin)
table(DM$origin,DM$CountryDataCollected_WEOG)

tmp<-dplyr::select(DM,CountryDataCollected_WEOG,origin)
tmp<-drop_na(tmp)
table(tmp$CountryDataCollected_WEOG,tmp$origin)
expertise1 <- tmp %>% 
  filter(CountryDataCollected_WEOG=='WEOG') %>%
  count(origin, .drop = F) %>%
  rename(var = origin) %>%
  mutate(var = ordered(var, levels = c("other", "western")), label = n)
expertise1

expertise2 <- tmp %>% 
  filter(CountryDataCollected_WEOG=='Non-WEOG') %>%
  count(origin, .drop = F) %>%
  rename(var = origin) %>%
  mutate(var = ordered(var, levels = c("other", "western")), label = n)
expertise2

expertise1 <- cbind(expertise1, 
                    MultinomCI(expertise1$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise1$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise1
print(paste0(as.character(expertise1$var[2]),' n=',expertise1$n[2],': ',expertise1$string[2]))

expertise2 <- cbind(expertise2, 
                    MultinomCI(expertise2$n,
                               conf.level=0.95,
                               method="sisonglaz")) %>%
  rename(prop = est) %>%
  mutate(string = paste0(round(prop*100, 0),'% [', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'),
         abs_string = paste0(n, "/", sum(expertise2$n), ", ", round(prop*100, 0),'% [95% confidence interval, ', round(lwr.ci*100, 0), '% to ', round(upr.ci*100, 0), '%]'))
expertise2
print(paste0(as.character(expertise2$var[2]),' n=',expertise2$n[2],': ',expertise2$string[2]))

expertise1$label<-'WEIRD'
expertise2$label<-'Non-WEIRD'
exp<-rbind(expertise1,expertise2)
print(knitr::kable(exp[,1:6],digits = 2,caption = 'Western music'))

t<-table(tmp$origin,tmp$CountryDataCollected_WEOG)
t
sum(t)
x<-chisq.test(tmp$origin,tmp$CountryDataCollected_WEOG)
print(paste('Chi = ', round(x$statistic,2), ', p-value = ', x$p.value))

rm(w,W,tmp,row2a,row2a_p,x,expertise1,expertise2,t,my.function,NW,DM)
