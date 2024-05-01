count2category <- function(data = NULL,
                           index = NULL,
                           str1 = NULL,
                           str2 = NULL) {
  
  INDEX <- which(names(data)==index)
  KW <- which(names(data)=='KW')
  tmp <- data[,c(KW,INDEX)]
  colnames(tmp)<-c('KW','index')
  tmp1 <- dplyr::filter(tmp, index == str1)
  tmp2 <- dplyr::filter(tmp, index == str2)
  head(tmp2)
  
  freq1 <- data.frame(table(tmp1$KW))
  head(freq1)
  freq1 <- dplyr::arrange(freq1, -Freq)
  head(freq1)
  tail(freq1)
  freq2 <- data.frame(table(tmp2$KW))
  freq2 <- dplyr::arrange(freq2, -Freq)
  head(freq2)
  
  kw<-data[,KW]
  KW_unique <- unique(kw)
  
  freq_table <-
    data.frame(
      KW = KW_unique,
      STR1 = rep(0, length(KW_unique)),
      STR2 = rep(0, length(KW_unique))
    )
  head(freq_table)
  rownames(freq_table) <- NULL
  length(KW_unique)
  for (k in 1:length(KW_unique)) {
    #  print(k)
    x1 <- freq1$Freq[which(freq1$Var1 == KW_unique[k])]
    if (length(x1) > 0) {
      freq_table$STR1[k] <- as.integer(x1)
    }
    x2 <- freq2$Freq[which(freq2$Var1 == KW_unique[k])]
    if (length(x2) > 0) {
      freq_table$STR2[k] <- as.integer(x2)
    }
  }
  freq_table <- dplyr::arrange(freq_table, -STR2)
  head(freq_table, 20)
  #tail(freq_table)
  
  freq_table$Freq <- freq_table$STR1 + freq_table$STR2
  freq_table$prop <- freq_table$STR1 / freq_table$Freq
  head(freq_table)
  freq_table <- dplyr::arrange(freq_table, -Freq)
  head(freq_table)
  s1 <- which(names(freq_table)=='STR1');colnames(freq_table)[s1] <- str1
  s2 <- which(names(freq_table)=='STR2');colnames(freq_table)[s2] <- str2
  
  return <- freq_table
}