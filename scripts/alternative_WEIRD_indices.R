alternative_WEIRD_indices <- function(d = NULL, WEIRD_index = 'WEOG') {
  # Put in some fixes
  d$CountryDataCollected[d$CountryDataCollected == 'North America'] <- 'US'
  #d$CountryDataCollected[d$CountryDataCollected=='Europe']<-'UK'
  
  cat(paste0('Using WEIRD country index: ', WEIRD_index))
  if (WEIRD_index == 'WEOG') {
    #### WEOG country variable
    WEOG_countries <- c(
      "Andorra",
      "Australia",
      "Austria",
      "Belgium",
      "Canada",
      "Denmark",
      "Finland",
      "France",
      "Germany",
      "Greece",
      "Iceland",
      "Ireland",
      "Israel",
      "Italy",
      "Liechtenstein",
      "Luxembourg",
      "Malta",
      "Monaco",
      "Netherlands",
      "New Zealand",
      "Norway",
      "Portugal",
      "San Marino",
      "Spain",
      "Sweden",
      "Switzerland",
      "Turkey",
      "UK",
      "US",
      "Vatican City",
      "Europe",
      "North America"
    )
    # Note that Hungary and Poland, Romania, Serbia are not part of the WEOS!
    
    # which countries do not match!
    table(df$CountryDataCollected[!df$CountryDataCollected %in% WEOG_countries])
    # Czech Republic Estonia Europe Latvia North America Poland Romania Serbia online (139 of them)
    # Fix North America and online
    df$CountryDataCollected[df$CountryDataCollected == 'North America'] <-
      'US'
    sum(df$CountryDataCollected == 'online', na.rm = T)
    index <- df$CountryDataCollected == 'online'
    index[is.na(index)] <- FALSE
    df$CountryDataCollected[index] <- df$SamplePrimaryCountryofOrigin_online_inferred[index]
    df$sample_country_data_collected[index] <- df$SamplePrimaryCountryofOrigin_online_inferred[index]
    
    table(df$CountryDataCollected)
    sum(is.na(df$CountryDataCollected))
    df$CountryDataCollected_WEOG <- NA
    df$CountryDataCollected_WEOG[!is.na(df$CountryDataCollected)] <- 'Non-WEOG'
    df$CountryDataCollected_WEOG[df$CountryDataCollected %in% WEOG_countries] <- 'WEOG'
    sum(is.na(df$CountryDataCollected_WEOG))
    table(df$CountryDataCollected_WEOG)
    table(df$CountryDataCollected, df$CountryDataCollected_WEOG)
    df$CountryDataCollected[df$CountryDataCollected_WEOG != 'WEOG']
    
    # clean some anomalies, missing countries to actual NAs
    df$CountryDataCollected[df$CountryDataCollected == "NA"] <- NA
    df$CountryDataCollected[df$CountryDataCollected == ""] <- NA
    
    ## Same for sample_country_data_collected
    
    # clean some anomalies, missing countries to actual NAs
    df$sample_country_data_collected[df$sample_country_data_collected == "NA"] <-
      NA
    df$sample_country_data_collected[df$sample_country_data_collected == ""] <-
      NA
    df$sample_country_data_collected[df$sample_country_data_collected == "online"] <-
      NA
    
    df$sample_country_data_collected_WEOG <- NA
    df$sample_country_data_collected_WEOG[!is.na(df$sample_country_data_collected)] <- 'Non-WEOG'
    df$sample_country_data_collected_WEOG[df$sample_country_data_collected %in% WEOG_countries] <- 'WEOG'
    sum(is.na(df$sample_country_data_collected_WEOG))
    table(df$sample_country_data_collected_WEOG)
    table(df$sample_country_data_collected,
          df$sample_country_data_collected_WEOG)
    df$sample_country_data_collected[df$sample_country_data_collected_WEOG !=
                                       'WEOG']
    
    
    
    ## same for FirstAuthorCountry
    
    table(df$FirstAuthorCountry)
    sum(is.na(df$FirstAuthorCountry))
    df$FirstAuthorCountry_WEOG <- 'Non-WEOG'
    df$FirstAuthorCountry_WEOG[df$FirstAuthorCountry %in% WEOG_countries] <- 'WEOG'
    sum(is.na(df$FirstAuthorCountry_WEOG))
    table(df$FirstAuthorCountry_WEOG)
    table(df$FirstAuthorCountry, df$FirstAuthorCountry_WEOG)
    
    df$CountryDataCollected_WEOG <- factor(df$CountryDataCollected_WEOG)
    df$FirstAuthorCountry_WEOG <- factor(df$FirstAuthorCountry_WEOG)
    
    ## same for papers
    d$CountryDataCollected[d$CountryDataCollected == 'North America'] <- 'US'
    #d$CountryDataCollected[d$CountryDataCollected=='Europe']<-'UK'
    
    sum(d$CountryDataCollected == 'online', na.rm = T) # 139
    index <- d$CountryDataCollected == 'online'
    index[is.na(index)] <- FALSE
    d$CountryDataCollected[index] <- d$SamplePrimaryCountryofOrigin_online_inferred[index]
    table(d$SamplePrimaryCountryofOrigin_online_inferred)
    table(d$CountryDataCollected)
    sum(is.na(d$CountryDataCollected))
    d$CountryDataCollected_WEOG <- NA
    d$CountryDataCollected_WEOG[!is.na(d$CountryDataCollected)] <- 'Non-WEOG'
    d$CountryDataCollected_WEOG[d$CountryDataCollected %in% WEOG_countries] <- 'WEOG'
    sum(is.na(d$CountryDataCollected_WEOG))
    table(d$CountryDataCollected_WEOG)
    sum(table(d$CountryDataCollected_WEOG)) + sum(is.na(d$CountryDataCollected_WEOG))
    table(d$CountryDataCollected, d$CountryDataCollected_WEOG)
    d$CountryDataCollected[d$CountryDataCollected_WEOG != 'WEOG']
    
    d$CountryDataCollected[d$CountryDataCollected == "NA"] <- NA
    d$CountryDataCollected[d$CountryDataCollected == ""] <- NA
    
    
    d$FirstAuthorCountry_WEOG <- 'Non-WEOG'
    d$FirstAuthorCountry_WEOG[d$FirstAuthorCountry %in% WEOG_countries] <-
      'WEOG'
    d$FirstAuthorCountry_WEOG<-factor(d$FirstAuthorCountry_WEOG)
    table(d$FirstAuthorCountry, d$FirstAuthorCountry_WEOG)
    sum(table(d$FirstAuthorCountry, d$FirstAuthorCountry_WEOG))
  }


  #### Distance to US variable
  if (WEIRD_index == 'Muthukhrisna') {
    USD <- read.csv('data/weird_sino.csv')
    head(USD)
    table(d$FirstAuthorCountry[!d$FirstAuthorCountry %in% USD$country])
    table(USD$country)
    # list of fixes
    tmp <- USD[USD$country == 'Germany', ]
    tmp$country <- 'Austria'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'France', ]
    tmp$country <- 'Beligum'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Slovenia', ]
    tmp$country <- 'Croatia'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Poland', ]
    tmp$country <- 'Czech Republic'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Sweden', ]
    tmp$country <- 'Denmark'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Great Britain', ]
    tmp$country <- 'Ireland'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Spain', ]
    tmp$country <- 'Portugal'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Slovenia', ]
    tmp$country <- 'Slovakia'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'United States', ]
    tmp$country <- 'US'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Great Britain', ]
    tmp$country <- 'UK'
    USD <- rbind(USD, tmp)
    
    tmp <- USD[USD$country == 'France', ]
    tmp$country <- 'Belgium'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Mali', ]
    tmp$country <- 'Central African Republic'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Armenia', ]
    tmp$country <- 'Greece'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Norway', ]
    tmp$country <- 'Iceland'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Japan', ]
    tmp$country <- 'Israel'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Ethiopia', ]
    tmp$country <- 'Kenya'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Estonia', ]
    tmp$country <- 'Latvia'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Slovenia', ]
    tmp$country <- 'Serbia'
    USD <- rbind(USD, tmp)
    tmp <- USD[USD$country == 'Malaysia', ]
    tmp$country <- 'UAE'
    USD <- rbind(USD, tmp)
    
    
    df$USD <- NA
    d$USD <- NA
    df$USDC <- NA
    d$USDC <- NA
    for (k in 1:nrow(USD)) {
      df$USD[df$FirstAuthorCountry %in% USD$country[k]] <- USD$United.States[k]
      d$USD[d$FirstAuthorCountry %in% USD$country[k]] <- USD$United.States[k]
      df$USDC[df$CountryDataCollected %in% USD$country[k]] <- USD$United.States[k]
      d$USDC[d$CountryDataCollected %in% USD$country[k]] <- USD$United.States[k]
    }
    SPLIT <- mean(unique(d$USDC), na.rm = T) + 0.02 # calculate this from the unique index, rather than from our data
    SPLIT
    d$FirstAuthorCountry_USD_Md <- cut(
      d$USD,
      breaks = c(-1, SPLIT, 1),
      labels = c('Western', 'Non-Western')
    )
    d$CountryDataCollected_USD_Md <- cut(
      d$USDC,
      breaks = c(-1, SPLIT, 1),
      labels = c('Western', 'Non-Western')
    )
    df$FirstAuthorCountry_USD_Md <- cut(
      df$USD,
      breaks = c(-1, SPLIT, 1),
      labels = c('Western', 'Non-Western')
    )
    df$CountryDataCollected_USD_Md <- cut(
      df$USDC,
      breaks = c(-1, SPLIT, 1),
      labels = c('Western', 'Non-Western')
    )
    
    d$CountryDataCollected_USD_Md <- factor(d$CountryDataCollected_USD_Md, levels = c('Western', 'Non-Western'),labels = c('WEOG','Non-WEOG'))
    d$CountryDataCollected_WEOG <- d$CountryDataCollected_USD_Md    
    df$CountryDataCollected_WEOG <- factor(df$CountryDataCollected_USD_Md, levels = c('Western', 'Non-Western'),labels = c('WEOG','Non-WEOG'))
    
    # reverse the factor order to be compatible
    d$CountryDataCollected_WEOG <- factor(d$CountryDataCollected_WEOG, levels = c('Non-WEOG', 'WEOG'))
    df$CountryDataCollected_WEOG <- factor(df$CountryDataCollected_WEOG, levels = c('Non-WEOG', 'WEOG'))
    rm(USD, SPLIT, tmp)
  }
  
  
  # We need d and DF to be returned
 # cat("\nFinal dataframes:")
#  cat("\n D = human studies:\n")
  D <- dplyr::filter(d,humansample==TRUE)
#  cat(nrow(D))
  DF <- dplyr::filter(df,humansample==TRUE)
#  cat("\n DF = human studies with samples:\n")
#  cat(nrow(DF))
  DF<-ungroup(DF)
  D<-ungroup(D)
  DF <- DF %>% filter(!is.na(sample_size) | !sample_id=='sample0')
#  cat("\n DF = human studies with distinct samples:\n")
#  cat(nrow(DF)) # 1589
  
  return <- list(d=d,DF=DF)
}
