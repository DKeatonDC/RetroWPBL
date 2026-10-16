counts <- function(evntf) {
  evntf$c00 <- TRUE
  evntf$c10<-grepl("^[BIPV]",evntf$pitches)
  evntf$c20<-grepl("^[BIPV]{2}",evntf$pitches)
  evntf$c30<-grepl("^[BIPV]{3}",evntf$pitches)
  evntf$c01<-grepl("^[CFKLMOQRST]",evntf$pitches)
  return(evntf)
}