# keyword_analysis.R
# WEIRD article
# T. Eerola, 23/3/2024
# Status: Complete

#### Custom functions -------------------
source('scripts/count2category.R')
source('scripts/count2category2.R')
source('scripts/count2category3.R')
plotflag <- TRUE

## remove studies, keep articles (to avoid duplicating keywords)
D <- dplyr::filter(d, study_id == 'study1') # 1360 articles (1360 correct!)

if (!dim(D)[1] == 1360) {
  print("incorrect number of observations! (Needs to be run at the level of studies, N=1360)")
  break
}

# eliminate those where the WEOG has not been defined
D <- dplyr::filter(D, !is.na(CountryDataCollected_WEOG))
sum(is.na(D$CountryDataCollected_WEOG))
dim(D) # 1221

#### 1. Specify Age -------------------------

#### 2. specify Uni Sample -------------------------
D$SampleOtherDescription[is.na(D$SampleOtherDescription)] <- 'Not specified'
D$uni <- str_detect(D$SampleOtherDescription, 'universi|undergrad')
D$uni[D$SampleOtherDescription == 'Not specified'] <- 'Not specified'
D$uni <- factor(
  D$uni,
  levels = c("FALSE", "Not specified", "TRUE"),
  labels = c("others", "Not specified", "university")
)

#### 3. specify Musician -------------------------
D$SampleMusicianshipDescription[is.na(D$SampleMusicianshipDescription)] <-
  'Not specified'
D$SampleMusicianshipDescriptionBinary <- factor(
  D$SampleMusicianshipDescription,
  levels = c(
    "musicians",
    "musicians; non-musicians",
    "non-musicians",
    "Not specified"
  ),
  labels = c("musicians", "both", "non-musicians", "Not specified")
)
#### 4. specify Western -------------------------
D$MusicOriginCountry[is.na(D$MusicOriginCountry)] <- 'Not specified'
D$origin <- 'X'
D$origin <- paste('X', D$MusicOriginCountry)
D$origin[str_detect(D$MusicOriginCountry, '^Western$')] <- 'Western'
D$origin[str_detect(D$MusicOriginCountry, 'Africa')] <- 'Non-Western'
D$origin[str_detect(D$MusicOriginCountry, 'Not specified')] <- 'Not specified'
D$origin[str_detect(D$MusicOriginCountry, 'Portugal')] <- 'Western'
D$origin[str_detect(D$MusicOriginCountry, 'Estonia')] <- 'Western'
D$origin[str_detect(D$MusicOriginCountry, 'Hungary')] <- 'Western'
D$origin[str_detect(D$MusicOriginCountry, 'Spain')] <- 'Western'
D$origin[str_detect(D$MusicOriginCountry, 'Australia')] <- 'Non-Western' # CHECK, if abo
D$origin[str_detect(D$origin, 'X ')] <- 'Non-Western'
D$origin <- factor(
  D$origin,
  levels = c("Non-Western", "Not specified", "Western"),
  labels = c("non-western", "other", "western")
)

#### keyword analysis ------------------
KW <- NULL
d_index <- NULL
WEOG_index <- NULL

u_index <- NULL # university
o_index <- NULL # origin
m_index <- NULL # musicians
a_index <- NULL # age
g_index <- NULL # gender balance
year_index <- NULL # year

for (k in 1:nrow(D)) {
  tmp <- D$Keywords[k]
  WEOG <- as.character(D$CountryDataCollected_WEOG[k])
  u <- as.character(D$uni[k])
  o <- as.character(D$origin[k])
  m <- as.character(D$SampleMusicianshipDescriptionBinary[k])
  a <- as.character(D$SampleAgeMean[k])
  g <- as.character(D$gender_balance[k])
  y <- as.character(D$Year[k])
  tmp <- stringi::stri_trans_general(tmp, "latin-ascii")
  if (!is.na(tmp)) {
    kw <- str_split(tmp, '[;,]', simplify = TRUE)
    KW <- c(KW, kw)
    d_index <- c(d_index, rep(k, length(kw)))
    u_index <- c(u_index, rep(u, length(kw)))
    o_index <- c(o_index, rep(o, length(kw)))
    m_index <- c(m_index, rep(m, length(kw)))
    a_index <- c(a_index, rep(a, length(kw)))
    g_index <- c(g_index, rep(g, length(kw)))
    year_index <- c(year_index, rep(y, length(kw)))
    WEOG_index <- c(WEOG_index, rep(WEOG, length(kw)))
  }
}

KW <- stringi::stri_trans_general(KW, "latin-ascii")
source('scripts/simplify_keywords.R')

data <- data.frame(KW,
                   WEOG_index,
                   a_index,
                   u_index,
                   m_index,
                   o_index,
                   g_index,
                   year_index)
data$a_index_n <- as.numeric(data$a_index)
data$year_index <- as.numeric(data$year_index)
data$a_index_nB <- cut(
  data$a_index_n,
  breaks = c(0, median(data$a_index_n, na.rm = T), 100),
  labels = c('Young', 'Old')
)

table(data$m_index) # include both

#### REVISION IDEA: EXPLICIT MENTION OF NA / not specified
table(data$o_index) # This is fine, 3 categories
# REVISION % Western music % University samples
# total number studies, including NA [prop of na]


data$g_index_n <- as.numeric(data$g_index)
data$g_index_nB <- cut(
  data$g_index_n,
  breaks = c(-0.1, .5, 1.1),
  labels = c('Male dom', 'Female dom')
)
x1 <- count2category(data,
                     index = "WEOG_index",
                     str1 = "WEOG",
                     str2 = "Non-WEOG")
#x2 <- count2category(data, index="m_index", str1="musicians", str2="others")
x2b <- count2category3(
  data,
  index = "m_index",
  str1 = "musicians",
  str2 = "both",
  str3 = "non-musicians",
  str4 = "Not specified"
)
head(x2b)
#x3 <- count2category(data, index="o_index", str1="western", str2="other")
x3b <- count2category2(
  data,
  index = "o_index",
  str1 = "western",
  str2 = "non-western",
  str3 = "other"
)

x4 <- count2category2(
  data,
  index = "u_index",
  str1 = "university",
  str2 = "others",
  str3 = "Not specified"
)

# x5 <- count2category(data, index="a_index_nB", str1='Young', str2='Old')
# head(x5,25)
# x6 <- count2category(data, index="g_index_nB", str1='Male dom', str2='Female dom')
# head(x6,25)
# mean per KW
x6 <- summarise(group_by(data, KW),
                prop = mean(g_index_n, na.rm = TRUE),
                Freq = n())
x6 <- dplyr::arrange(x6, -Freq)
x6 <- drop_na(x6)

x5 <- summarise(group_by(data, KW),
                prop = mean(a_index_n, na.rm = TRUE),
                Freq = n())
x5 <- dplyr::arrange(x5, -Freq)
x5 <- drop_na(x5)

FROM <- 1
TO <- 25

#### subplot 1 ----------
g1a <- ggplot(data = x1[FROM:TO, ], aes(
  x = reorder(KW, prop),
  y = prop,
  label = Freq
)) +
  geom_col(fill = 'grey70', color = 'grey10') +
  geom_text(nudge_y = .0127, family = 'Times',size=3.0) +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(
    breaks = seq(0, 1, .1),
    labels = (seq(0, 1, .1)) * 100
  ) +
  scale_fill_grey() +
  coord_flip(ylim = c(.45,1)) +
  ylab("% WEIRD") +
  xlab("Keyword (ranked)") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  theme_linedraw(base_size = 15, base_family = 'Times') +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
g1a

### subplot 2 ----
#x2b
x2b<-x2b[FROM:TO, ]

#head(x2b)
x2b$musiciansp<-x2b$musicians/x2b$Freq
x2b$bothp<-x2b$both/x2b$Freq
x2b$nonmusiciansp<-x2b$`non-musicians`/x2b$Freq
x2b$notspecifiedp<-x2b$`Not specified`/x2b$Freq
#head(x2b)
X2b <- pivot_longer(x2b, cols = c(musiciansp, bothp, nonmusiciansp, notspecifiedp))
X2b$name <- factor(X2b$name, levels = c('musiciansp', 'bothp', 'nonmusiciansp', 'notspecifiedp'),labels = c('Musicians','Both','Non-musicians', 'Unspecified'))
#head(X2b)

X2b$name <- factor(X2b$name, levels = c('Unspecified', 'Non-musicians', 'Both','Musicians'))
#head(X2b)

g2a <- ggplot(data = X2b, aes(
  x = reorder(KW, prop),
  y = value,
  fill = name,
  label = Freq*value
)) +
  geom_col(color = 'grey10') +
  geom_text(position = position_stack(vjust = 0.5), family = 'Times',size=3.0) +
  coord_flip() +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(
    breaks = seq(0, 1, .1),
    labels = (seq(0, 1, .10)) * 100,
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
#  scale_fill_brewer(palette = "Dark2") +
  scale_fill_grey(name='Musical\nexpertise', start = 1.0,end = .4, guide = guide_legend(reverse = TRUE)) +
  ylab("% Musical expertise") +
  xlab("Keyword (ranked)") +
  theme_linedraw(base_size = 15, base_family = 'Times') +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
#  theme(legend.position="top")

#g2a
# ggsave(
#   filename = 'tmp2.pdf',
#   g2a,
#   device = 'pdf',
#   width = 9,
#   height = 7
# )
### revised ends here ----

#### subplot 3 -------
#head(x3b)
x3b<-x3b[FROM:TO, ]
x3b$westernp<-x3b$western/x3b$Freq
x3b$nonwesternp<-x3b$`non-western`/x3b$Freq
x3b$otherp<-x3b$other/x3b$Freq
names(x3b)
X3b <- pivot_longer(x3b, cols = c("westernp", "nonwesternp", "otherp"))
X3b$name <- factor(X3b$name, levels = c('otherp','nonwesternp','westernp'),labels = c('Unspecified', 'Non-Western', 'Western'))
head(X3b)

g3a <- ggplot(data = X3b, aes(
  x = reorder(KW, prop),
  y = value,
  fill = name,
  label = Freq*value
)) +
  geom_col(color = 'grey10') +
  geom_text(position = position_stack(vjust = 0.5), family = 'Times',size=3.0) +
  coord_flip() +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(
    breaks = seq(0, 1, .1),
    labels = (seq(0, 1, .10)) * 100,
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(name='Music\norigin', start = 1.0,end = .4, guide = guide_legend(reverse = TRUE)) +
  ylab("% Music origin") +
  xlab("Keyword (ranked)") +
  theme_linedraw(base_size = 15, base_family = 'Times') +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
#  theme(legend.position="bottom")
g3a

#### subplot 4 -------
x4b<-x4[FROM:TO, ]
head(x4b)
x4b$universityp<-x4b$university/x4b$Freq
x4b$othersp<-x4b$others/x4b$Freq
x4b$notspecifiedp<-x4b$`Not specified`/x4b$Freq

X4b <- pivot_longer(x4b, cols = c(universityp, othersp, notspecifiedp))
X4b$name <- factor(X4b$name, levels = c('universityp', 'othersp', 'notspecifiedp'),labels = c('University', 'Others', 'Unspecified'))
head(X4b)

X4b$name <- factor(X4b$name, levels = c('Unspecified', 'Others', 'University'))
head(X4b)

g4a <- ggplot(data = X4b, aes(
  x = reorder(KW, prop),
  y = value,
  fill = name,
  label = Freq*value
)) +
  geom_col(color = 'grey10') +
  geom_text(position = position_stack(vjust = 0.5), family = 'Times',size=3.0) +
  coord_flip() +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(
    breaks = seq(0, 1, .1),
    labels = (seq(0, 1, .10)) * 100,
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(name='Sample\ndescription', start = 1.0,end = .4, guide = guide_legend(reverse = TRUE)) +
  ylab("% Sample description") +
  xlab("Keyword (ranked)") +
  theme_linedraw(base_size = 15, base_family = 'Times') +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
#  theme(legend.position="bottom")
g4a

#### subplot 5 --------

Md <- median(x5$prop, na.rm = TRUE)
g5a <- ggplot(data = x5[FROM:TO, ], aes(
  x = reorder(KW, prop),
  y = prop - Md,
  label = Freq
)) +
  geom_col(fill = 'grey70', color = 'grey10') +
  geom_text(nudge_y = 0.70, family = 'Times',size=3.0) +
  coord_flip() +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(breaks = seq(-15, 15, 5), limits = c(-18, 17)) +
  scale_fill_brewer(palette = "Dark2") +
  ylab("Years from the median age") +
  xlab("Keyword (ranked)") +
  theme_linedraw(base_size = 15, base_family = 'Times') +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
g5a

#### subplot 6 ---------
head(x6)
g6a <- ggplot(data = x6[FROM:TO, ], aes(
  x = reorder(KW, prop),
  y = prop,
  label = Freq
)) +
  geom_col(fill = 'grey70', color = 'grey10') +
  geom_text(nudge_y = .0033, family = 'Times',size=3.0) +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(
    breaks = seq(0, 1, .05),
    labels = (seq(0, 1, .05)) * 100
#    limits = c(0, .6)
  ) +
  scale_fill_brewer(palette = "Dark2") +
  ylab("% Female participants") +
  xlab("Keyword (ranked)") +
  coord_flip(ylim = c(0.4,0.6)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  theme_linedraw(base_size = 15, base_family = 'Times') +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
g6a

Figure3 <- cowplot::plot_grid(g1a, g2a, g3a, g4a, g5a, g6a, nrow = 3, ncol = 2,labels = 'AUTO')
print(Figure3)

if (plotflag == TRUE) {
  ggsave(
    filename = 'figure3_R2.pdf',
    Figure3,
    device = 'pdf',
    width = 14,
    height = 15
  )
}

#### Additional analyses ------------------
# keyword per year per WEIRD
head(data)

# take the most common (N) keywords
N <- 10
top <- summarise(group_by(data, KW), Freq = n())
top <- drop_na(top)
top <- dplyr::arrange(top, -Freq)
head(top, 10)
top10KW <- top$KW[1:N]
data_filtered <- dplyr::filter(data, KW %in% top10KW)

# create empty data frame
x10e <- 2010:2022
x10e <- expand.grid(
  KW = top10KW,
  WEOG_index = c('Non-WEOG', 'WEOG'),
  year_index = x10e
)
x10e$Freq <- 0
x10 <- summarise(group_by(data_filtered, KW, WEOG_index, year_index), Freq =
                   n())
x10 <- rbind(x10, x10e)

x10 <- dplyr::arrange(x10, -Freq)
x10$KW <- factor(x10$KW, levels = top10KW)
x10$WEOG_index <- factor(
  x10$WEOG_index,
  levels = c('Non-WEOG', 'WEOG'),
  labels = c('Non-WEIRD', 'WEIRD')
)

Figure4 <- ggplot(data = x10, aes(x = year_index, y = Freq, fill = WEOG_index)) +
  geom_col(position = 'dodge', color = 'black') +
  facet_wrap( ~ KW, scales = 'free_y', nrow = 5) +
  theme(text = element_text(size = 16)) +
  scale_fill_manual(name = "Group", values = c('#E41A1C', '#377EB8')) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  ylab("Number of articles") +
  xlab("Year") +
  theme_linedraw(base_size = 15, base_family = 'Times') +
  theme(panel.border = element_blank(), panel.grid.major = element_blank())

print(Figure4)

if (plotflag == TRUE) {
  ggsave(
    filename = 'figure4_R2.pdf',
    Figure4,
    device = 'pdf',
    width = 16,
    height = 12
  )
}
