# Homework 2 Xin Xu

library("stringr")
hw1 <- read.csv("hw1.csv")
housing <- data.frame(matrix(NA, nrow=27307, ncol=30))
colnames(housing) <- c("pid", "location", "totval", "address", "owner1",
                       "owner2", "owner3", "owner4", "owner5", "multiple buildings", "yearbuilt"
                       , "sqft", "replcost", "pctgood", "style", "model", 
                       "grade", "occupancy", "actype", "bedrooms", "bathrooms", 
                       "halfbaths", "bathstyle", "kstyle", "exval", "acres", "zone", 
                       "neighborhood", "landval", "garagesqft")
housing[,1:3] <- hw1[,1:3]
housing[,10] <- 0

for (i in 1:27307) {
  cat(i, "\n")
  filename <- file.path("newdata2016", paste(i, ".html", sep=""))
  if (file.exists(filename)) {
    x <- readLines(filename)
    
    thisline <- grep("Address", x)
    temp <- x[thisline]
    temp <- gsub(".*MainContent_lblAddr1\">", "", temp)
    temp <- gsub("<br>", ", ", temp)
    temp <- gsub("<[^<>]*>", "", temp)
    housing[i,4] <- temp
    
    thisline <- grep("Sale Price", x)[2]+2
    end <- grep("Building Information", x)-12
    n <- (end-thisline+2)/2
    for (j in 1:n){
      temp <- x[thisline+(j-1)*2]
      temp <- substring(temp,9)
      date <- str_match(temp, "[0-9]{2}/[0-9]{2}/[0-9]{4}")
      price <- str_match(temp, "\\$[0-9]*,[0-9]*")
      temp <- gsub(".*<td>", "", temp)
      temp <- gsub("</td>.*", "", temp)
      temp <- gsub("amp;", "", temp)
      temp <- paste(temp, price, date, sep=", ")
      housing[i,4+j] <- temp
    }
    
    thisline <- grep("Year Built", x)
    if (length(thisline)>1) housing[i,10] <- 1
    thisline <- thisline[1]
    temp <- x[thisline+1]
    housing[i,11] <- as.numeric(str_match(temp, "[0-9]{4}"))
    
    thisline <- grep("Living Area", x)[1]
    temp <- x[thisline+1]
    temp <- gsub(".*MainContent_ctl01_lblBldArea\">", "", temp)
    housing[i,12] <- as.numeric(gsub("[^0-9]", "", temp))
    
    thisline <- grep("Replacement Cost", x)[1]
    temp <- x[thisline+1]
    temp <- str_match(temp, "\\$[0-9]*,[0-9]*")
    housing[i,13] <- as.numeric(gsub("[^0-9]", "", temp))
    
    thisline <- grep("Building Percent Good", x)[1]
    temp <- x[thisline+1]
    temp <- gsub(".*MainContent_ctl01_lblPctGood\">", "", temp)
    housing[i,14] <- as.numeric(gsub("[^0-9]", "", temp))
    
    thisline <- grep("<td>Style", x, ignore.case=TRUE)[1]
    temp <- x[thisline]
    temp <- gsub(".*<td>", "", temp)
    housing[i,15] <- gsub("</td>", "", temp)
  
    thisline <- grep("<td>Model", x, ignore.case=TRUE)[1]
    temp <- x[thisline]
    temp <- gsub(".*<td>", "", temp)
    housing[i,16] <- gsub("</td>", "", temp)    

    thisline <- grep("<td>Grade", x, ignore.case=TRUE)[1]
    temp <- x[thisline]
    temp <- gsub(".*<td>", "", temp)
    housing[i,17] <- gsub("</td>", "", temp)     
    
    thisline <- grep("<td>Occupancy", x, ignore.case=TRUE)[1]
    temp <- x[thisline]
    housing[i,18] <- as.numeric(gsub("[^0-9]", "", temp))
    
    thisline <- grep("<td>AC Type", x, ignore.case=TRUE)[1]
    temp <- x[thisline]
    temp <- gsub(".*<td>", "", temp)
    housing[i,19] <- gsub("</td>", "", temp) 
    
    thisline <- grep("T.*t.*l Bedr.*ms", x)[1]
    temp <- x[thisline]
    housing[i,20] <- as.numeric(gsub("[^0-9]", "", temp))
  
    thisline <- grep("T.*t.*l B.*th.*s", x)[1]
    temp <- x[thisline]
    housing[i,21] <- as.numeric(gsub("[^0-9]", "", temp))

    thisline <- grep("T.*t.*l Half B.*th.*s", x)[1]
    temp <- x[thisline]
    housing[i,22] <- as.numeric(gsub("[^0-9]", "", temp))
    
    thisline <- grep("<td>Bath Style", x, ignore.case=TRUE)[1]
    temp <- x[thisline]
    temp <- gsub(".*<td>", "", temp)
    housing[i,23] <- gsub("</td>", "", temp) 
    
    thisline <- grep("<td>Kitchen Style", x, ignore.case=TRUE)[1]
    temp <- x[thisline]
    temp <- gsub(".*<td>", "", temp)
    housing[i,24] <- gsub("</td>", "", temp) 
    
    thisline <- grep("\tExtra Features", x)
    temp <- x[thisline+4]
    temp <- str_match(temp, "\\$[0-9]*,[0-9]*")
    housing[i,25] <- as.numeric(gsub("[^0-9]", "", temp))
    
    thisline <- grep("Acres", x)
    temp <- x[thisline]
    temp <- gsub(".*MainContent_lblLndAcres\">", "", temp)
    temp <- gsub("</span></td>", "", temp)
    housing[i,26] <- as.numeric(temp)

    thisline <- grep("Zone</td>", x)
    temp <- x[thisline]
    temp <- gsub(".*MainContent_lblZone\">", "", temp)
    housing[i,27] <- gsub("</span></td>", "", temp)
    
    
    thisline <- grep("Neighborhood</td>", x)
    temp <- x[thisline]
    temp <- gsub(".*MainContent_lblNbhd\">", "", temp)
    housing[i,28] <- gsub("</span></td>", "", temp)
  
    thisline <- grep("Appraised Value", x)
    temp <- x[thisline]
    temp <- gsub("[^0-9]", "", temp)
    housing[i,29] <- as.numeric(temp)
    
    thisline <- grep("Appraised Value", x)
    temp <- x[thisline]
    temp <- gsub("[^0-9]", "", temp)
    housing[i,29] <- as.numeric(temp)
    }
}
write.csv(housing, file="hw2.csv", row.names=FALSE)
