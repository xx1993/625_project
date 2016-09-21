#STAT625 
#Real estate information extraction
#Written by Dingjue Ji, 09/12/2016

#Important Note:
#For those with several buildings, the number of rooms
#will be counted only for the first one.
#Besides, the room numbers will be counted as 0 only if 
#they exist and are 0. 
#The others without information will be recorded as NA.
#'Always first' policy if several terms exist for one 
#specific keyword.
#'FCP', 'FGR', 'OPA', 'UGR' are the labels for garages

#First 24 columns with their names, patterns, extracting keywords
col.names<-c('pid', 'location','totval', 'address', 
             'yearbuilt', 'sqft', 'replcost', 'pctgood', 
             'style', 'model', 'grade', 'occupancy', 
             'actype', 'bedrooms', 'bathrooms', 'halfbaths', 
             'bathstyle', 'kstyle', 'exval', 'acres', 'zone', 
             'neighborhood', 'landval', 'garagesqft')
saleinfo<-as.vector(sapply(1:5, function(i) sapply(c('buyer','price','date'),
                                                   function(x) paste(x, i, sep = ''))))
col.names[25:41]<-c(saleinfo, 'multibuilding', 'nineplus')

col.patterns<-c('', '"MainContent_lblLocation"', 
                '"MainContent_lblGenAppraisal"', 
                '"MainContent_lblAddr1"', 
                '"MainContent_ctl01_lblYearBuilt"', 
                '"MainContent_ctl01_lblBldArea"', 
                '"MainContent_ctl01_lblRcn"', 
                '"MainContent_ctl01_lblPctGood"', 
                '<td>[Ss][Tt][Yy][Ll][Ee][^<>]*</td>', 
                '<td>[Mm][Oo][Dd][Ee][Ll][^<>]*</td>', 
                '<td>[Gg][Rr][Aa][Dd][Ee][^<>]*</td>', 
                '<td>[Oo][Cc][Cc][Uu][Pp][Aa][Nn][Cc][Yy][^<>]*</td>', 
                'AC Type[^<>]*', 
                'To*ta*l Be*dro*ms', 
                'To*ta*l Ba*thr*o*m*s', 
                'To*ta*l Half Ba*ths', 
                'Bath Style[^<>]*', 
                'Kitchen Style[^<>]*', 
                '', 
                '"MainContent_lblLndAcres"', 
                '"MainContent_lblZone"', 
                '"MainContent_lblNbhd"', 
                '"MainContent_lblLndAppr"', 
                '')
#Why not use rep? Because I want to have a close look at
#each term.
col.repat<-c('', '<span[^<>]*>[^<>]*</span>', 
             '<span[^<>]*>[^<>]*</span>', 
             '<span[^<>]*>[^<>]*</span>', 
             '<span[^<>]*>[^<>]*</span>', 
             '<span[^<>]*>[^<>]*</span>', 
             '<span[^<>]*>[^<>]*</span>', 
             '<span[^<>]*>[^<>]*</span>', 
             '<td>[^<>]*</td>$', 
             '<td>[^<>]*</td>$', 
             '<td>[^<>]*</td>$', 
             '<td>[^<>]*</td>$', 
             '<td>[^<>]*</td>$', 
             '<td>[^<>]*</td>$', 
             '<td>[^<>]*</td>$', 
             '<td>[^<>]*</td>$', 
             '<td>[^<>]*</td>$', 
             '<td>[^<>]*</td>$', 
             '', 
             '<span[^<>]*>[^<>]*</span>', 
             '<span[^<>]*>[^<>]*</span>', 
             '<span[^<>]*>[^<>]*</span>', 
             '<span[^<>]*>[^<>]*</span>', 
             '')

#Tranform the format of numeric values
tonum<-function(x, trans){
  #Funtion to transform 'trans' columns of x
  #Usage tonum(DATA, COLUMNS)
  for(i in trans){
    x[, i]<-as.numeric(x[, i]) 
  }
  return(x)
}

getsale<-function(tab){
  #Function to retrieve information in table of sale history
  #Including first 5 'Owner','Sale Price','Sale Date'
  tab[, 'Owner']<-gsub('&amp[;]*', '&', tab[, 'Owner'])
  tab[, 'Sale Price']<-money2num(tab[, 'Sale Price'])
  sales<-as.vector(t(as.matrix(tab[1:5,c('Owner', 'Sale Price', 'Sale Date')])))
  return(trimws(sales))
}

#There are columns which need to be numeric
col.num<-rep(FALSE, 41)
names(col.num)<-col.names
col.num[c("totval", "sqft", "replcost", "pctgood", 
          "halfbaths", "acres", "landval", "garagesqft", "price1",
          "price2", "price3" ,"price4", "price5") ]<-TRUE

#There are many keywords sharing same pattern so we can use
#the same method
usearch<-which(col.patterns != '')

myreg<-function(pat, x, regpat, numonly=FALSE, usefirst=TRUE, nextline=0){
  #Function to retrieve string from a list of strings in html
  #Only for pid location totval
  #Usage: myreg(KEYWORDS, LIST, REGPATTERN, NUMERIC)
  #Get the line with the string and extract it
  ln<-grep(pat, x)
  if(length(ln) == 0){
    return(NA)
  }
  ln<-ln+nextline
  x[ln]<-gsub('<br>', ' ,', x[ln])
  reg<-regmatches(x[ln], regexpr(regpat, x[ln]))
  if(usefirst == TRUE){
    reg<-reg[1]
  }
  #Remove useless characters
  o<-gsub('<[^<>]*>', '', reg)
  o<-trimws(o)
  if(is.na(o) || o == ''){
    return(NA)
  }
  if(numonly == TRUE){
    o<-as.numeric(gsub('[^0-9\\.]', '', o))
  }
  return(o)
}


catchtab<-function(x, tabname){
  #Function to catch the only table with the name
  ln<-grep('^\\s+<table.*>', x)
  cap<-which(trimws(x[ln+2]) == tabname)
  cap<-ln[cap]+2
  if(length(cap) == 1){
    term<-grep('</table>', x[cap:length(x)])[1]+cap-2
    content<-x[cap:term]
    if(length(grep('No Data for', content)) > 0){
      return(NA)
    }
    ti<-gsub('<th[^<>]+>', '', trimws(x[cap+2]))
    ti<-gsub('&amp;', '&', ti)
    ti<-gsub('</th>', ',', ti)
    ti<-gsub('<br>', ' ', ti)
    ti<-strsplit(ti, split = ',')[[1]]
    content[grep('<[^<>]*tr[^<>]*>', content)]<-'SEPARATOR'
    content<-strsplit(paste(content, collapse = ''), split = 'SEPARATOR')[[1]]
    content<-content[grep('<td>.+</td>', content)]
    content<-gsub('&amp;', '&', trimws(content))
    content<-gsub('<td[^<>]*>', '', content)
    content<-gsub('</td>$', '', content)
    content<-gsub('</td>', '\t', content)
    tab<-as.data.frame(t(sapply(content,function(x) strsplit(x, split = '\t')[[1]])), stringsAsFactors=FALSE)
    colnames(tab)<-ti
    rownames(tab)<-1:nrow(tab)
    tab[tab == '&nbsp;']<-NA
    tab[tab == '']<-NA
    return(tab)
  }
  else{
    return(NA)
  }
}

money2num<-function(x){
  #Function to change character into numeric value
  as.numeric(gsub('[^0-9\\.]', '', x))
}

getgsize<-function(x){
  #Function to get the size of first garage we find
  if(length(x) > 0){
    return(sum(as.numeric(gsub('[^0-9]', '', x))))
  }
  else{
    return(NA)
  }
}


#single test section###################
fname='25904.html'
x<-scan(file = fname, what = '', sep = '\n', quiet = TRUE)
gsize<-c()
myinfo<-rep(NA, 41)
getit<-function(i){
  myreg(col.patterns[i], x, col.repat[i], numonly = col.num[i])
}
myinfo[usearch]<-sapply(usearch, getit)
myinfo[24]<-myreg('<td>Garage</td>', x, '\\S+',numonly = TRUE, nextline = 1)
subtab<-catchtab(x, 'Building Sub-Areas (sq ft)')
extab<-catchtab(x, 'Extra Features')
saletab<-catchtab(x, 'Ownership History')
outtab<-catchtab(x, 'Outbuildings')
if(class(extab) == 'data.frame'){
  myinfo[19]<- sum(money2num(extab[,grep('[Vv][Aa][Ll][Uu][Ee]', colnames(extab))]))
  grs<-unlist(sapply(c('FCP', 'FGR', 'OPA', 'UGR'), function(i) grep(i, extab[,1])))
  gsize<-c(gsize, extab[grs, 'Size'])
}
if(class(outtab) == 'data.frame'){
  grs<-unlist(sapply(c('FCP', 'FGR', 'OPA', 'UGR'), function(i) grep(i, outtab[,1])))
  gsize<-c(gsize, outtab[grs, 'Size'])
}
if(class(subtab) == 'data.frame'){
  grs<-unlist(sapply(c('FCP', 'FGR', 'OPA', 'UGR'), function(i) grep(i, subtab[,1])))
  gsize<-c(gsize, subtab[grs, 'Gross Area'])
}
if(class(extab) == 'data.frame'){
  myinfo[19]<- sum(money2num(extab[, grep('[Vv][Aa][Ll][Uu][Ee]', colnames(extab))]))
}
myinfo[24]<-getgsize(gsize)
if(class(saletab) == 'data.frame'){
  myinfo[25:39]<-getsale(saletab)
}

#single test section###################


myparse<-function(i){
  #Function to parse htmls and retrieve information
  #Only for sapply
  gsize<-c()
  fname<-paste(i, '.html', sep = '')
  #Read files when they exist
  myinfo<-rep(NA, 41)
  names(myinfo)<-col.names
  myinfo[c('multibuilding', 'nineplus')]<-0
  myinfo[1]<-i
  if(file.exists(fname)){
    x<-scan(file=fname, what = '', sep = '\n', quiet = TRUE)
    getit<-function(i){
      myreg(col.patterns[i], x, col.repat[i], numonly = col.num[i])
    }
    myinfo[usearch]<-sapply(usearch, getit) 
    #Get tables
    subtab<-catchtab(x, 'Building Sub-Areas (sq ft)')
    extab<-catchtab(x, 'Extra Features')
    saletab<-catchtab(x, 'Ownership History')
    outtab<-catchtab(x, 'Outbuildings')
    if(class(extab) == 'data.frame'){
      myinfo['exval']<- sum(money2num(extab[,grep('[Vv][Aa][Ll][Uu][Ee]', colnames(extab))]))
      grs<-unlist(sapply(c('FCP', 'FGR', 'OPA', 'UGR'), function(i) grep(i, extab[,1])))
      gsize<-c(gsize, extab[grs, 'Size'])
    }
    if(class(outtab) == 'data.frame'){
      grs<-unlist(sapply(c('FCP', 'FGR', 'OPA', 'UGR'), function(i) grep(i, outtab[,1])))
      gsize<-c(gsize, outtab[grs, 'Size'])
    }
    if(class(subtab) == 'data.frame'){
      grs<-unlist(sapply(c('FCP', 'FGR', 'OPA', 'UGR'), function(i) grep(i, subtab[,1])))
      gsize<-c(gsize, subtab[grs, 'Gross Area'])
    }
    myinfo["garagesqft"]<-getgsize(gsize)
    if(class(saletab) == 'data.frame'){
      myinfo[saleinfo]<-getsale(saletab)
    }
    if(length(grep('Building 2', x)) > 0){
      myinfo["multibuilding"]<-1
    }
    if(length(grep('\\+', myinfo["bedrooms"])) > 0){
      myinfo["nineplus"]<-1
    }
    myinfo["bedrooms"]<-as.numeric(gsub('[^0-9\\.]', '', myinfo['bedrooms']))
  }
  #Assign value to info
  return(myinfo)
}
#Save results as a matrix
system.time(temp<-sapply(1:27307, myparse))
info<-as.data.frame(t(temp), stringsAsFactors = FALSE)
#The bedroom number should be numeric
col.num['bedrooms']<-TRUE
info<-tonum(info, which(col.num == TRUE))
#Transform the logic columns into the right form
info[, "multibuilding"]<-info[, "multibuilding"]!=0
info[is.na(info[, "location"]), "multibuilding"]<-NA
info[, "nineplus"]<-info[, "nineplus"]!=0
info[is.na(info[, "bedrooms"]), "nineplus"]<-NA
#Give the right forms to the html symbols
for(i in 1:ncol(info)){
  info[, i]<-gsub('&lt;', '<', info[, i])
  info[, i]<-gsub('&gt;', '>', info[, i])
  info[, i]<-gsub('&nbsp;', NA, info[, i])
  info[, i]<-gsub('&#39;', "'", info[, i])
}
#'Bathrooms' may have the NUM/NUM format
info[, 'bathrooms']<-gsub('[^0-9\\.\\/]', '', info[, 'bathrooms'])
info[, 'bathrooms']<-sapply(info[, 'bathrooms'], 
                            function(x) return(as.numeric(eval(parse(text = x)))),
                            USE.NAMES = FALSE)
#Eliminate the 'GRADE' in 'grade'
info[, "grade"]<-gsub('GRADE[_:\\s]*', '', info[, "grade"])


#Write data into csv file
write.table(x = info, file = '625.csv', row.names = FALSE, sep=',')