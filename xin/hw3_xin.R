#hw3
library("stringr")
info[,1] <- 1:27307

for (i in 1:27307) {
  cat(i, "\n")
  filename <- file.path("newdata2016", paste(i, ".html", sep=""))
  if (file.exists(filename)) {
    x <- readLines(filename)
    # Owner Information: the first 5 owners   
    thisline <- grep("Sale Price", x)[2]+2
    end <- grep("Building Information", x)-12
    # Number of owners
    n <- (end-thisline+2)/2
    for (j in 1:n){
      temp <- x[thisline+(j-1)*2]
      temp <- substring(temp,9)
      date <- str_match(temp, "[0-9]{2}/[0-9]{2}/[0-9]{4}")
      price <- str_match(temp, "\\$[0-9]*,*[0-9]*")
      temp <- gsub("</td>.*", "", temp)
      temp <- gsub("amp;", "", temp)
      temp <- paste(temp, price, date, sep=", ")
      # The number of column need to be changed
      info[i,4+j] <- temp
    }
    
    #Extra Features
    thisline <- grep("\tExtra Features", x)
    end<-grep("<div id=\"MainContent_panLand\">", x)
    if (end-thisline==14) info[i,25] <- NA
    else {
      # Number of extra features
      n <- (end-thisline-11)/2
      temp<-x[(thisline+4):(thisline+2+2*n)]
      temp <- str_match(temp, "\\$[0-9]*,*[0-9]*")
      temp <- as.numeric(gsub("[^0-9]", "", temp))
      # The number of column need to be changed
      info[i,25] <- sum(temp, na.rm=TRUE)
    }
  }
}  



 