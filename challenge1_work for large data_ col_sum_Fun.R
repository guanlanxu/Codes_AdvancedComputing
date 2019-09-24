CSV_Col_Means <- function(infile) { 
i <- 0
con <- file(infile ,"r")
while (length(oneLine <- scan(con,what="numeric",nlines=1,sep=',',skip=1,quiet=TRUE))>0) {
  i <- i+1
  myLine <- as.numeric(unlist((strsplit(oneLine,","))))
  if (i == 1) {
    sums <- myLine  
  }
  if (i > 1) {
    sums <- sums + myLine 
  }
}
close(con)
sums/i
}



