#STAT625 Homework1 
#Real estate information extraction
#Written by Dingjue Ji, 09/12/2016

#Important Note:
#For those with several buildings, the number of rooms
#will be counted only for the first one.
#Besides, the room numbers will be counted as 0 only if 
#they exist and are 0. 
#The others without information will be recorded as NA. During the
#summation, only number with information will be counted.

#First 25 columns with their names, patterns, extracting keywords

col.names<-c('pid','location','totval','address',
             'yearbuilt','sqft','replcost','pctgood',
             'style','model','grade','occupancy',
             'actype','bedrooms','bathrooms','halfbaths',
             'bathstyle','kstyle','exval','acres','zone',
             'neighborhood','landval','garagesqft')

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
                ''
                )
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
             ''
             )

#Tranform the format of numeric values
tonum<-function(x, trans){
  #Funtion to transform 'trans' columns of x
  #Usage tonum(DATA, COLUMNS)
  for(i in trans){
    x[,i]<-as.numeric(x[,i]) 
  }
  return(x)
}

getsale<-function(tab){
  #Function to retrieve information in table of sale history
  #Including first 5 'Owner','Sale Price','Sale Date'
  tab[,'Owner']<-gsub('&amp[;]*', '&', tab[,'Owner'])
  tab[,'Sale Price']<-money2num(tab[,'Sale Price'])
  sales<-as.vector(t(as.matrix(tab[1:5,c('Owner','Sale Price','Sale Date')])))
  return(trimws(sales))
}

#There are columns which need to be numeric
col.num<-rep(FALSE, 41)
col.num[c(3,6,7,8,15,16,20,23,24,26,29,32,35,38)]<-TRUE

#There are many keywords sharing same pattern so we can use
#the same method
usearch<-which(col.patterns!='')

myreg<-function(pat, x, regpat, numonly=FALSE, usefirst=TRUE, nextline=0){
  #Function to retrieve string from a list of strings in html
  #Only for pid location totval
  #Usage: myreg(KEYWORDS, LIST, REGPATTERN, NUMERIC)
  #Get the line with the string and extract it
  ln<-grep(pat,x)
  if(length(ln) == 0){
    return(NA)
  }
  ln<-ln+nextline
  x[ln]<-gsub('<br>', ' ', x[ln])
  reg<-regmatches(x[ln], regexpr(regpat, x[ln]))
  if(usefirst==TRUE){
    reg<-reg[1]
  }
  #Remove useless characters
  o<-gsub('<[^<>]*>','',reg)
  o<-trimws(o)
  if(is.na(o) || o==''){
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
  cap<-which(trimws(x[ln+2])==tabname)
  cap<-ln[cap]+2
  if(length(cap) == 1){
    term<-grep('</table>', x[cap:length(x)])[1]+cap-2
    content<-x[cap:term]
    if(length(grep('No Data for', content))>0){
      return(NA)
    }
    ti<-gsub('<th[^<>]+>', '', trimws(x[cap+2]))
    ti<-gsub('</th>', ',', ti)
    ti<-gsub('<br>', ' ', ti)
    ti<-strsplit(ti,split=',')[[1]]
    content[grep('<[^<>]*tr[^<>]*>', content)]<-'SEPARATOR'
    content<-strsplit(paste(content, collapse = ''), split='SEPARATOR')[[1]]
    content<-content[grep('<td>.+</td>', content)]
    content<-gsub('<td[^<>]*>', '', trimws(content))
    content<-gsub('</td>$', '', content)
    content<-gsub('</td>', '\t', content)
    tab<-as.data.frame(t(sapply(content,function(x) strsplit(x, split='\t')[[1]])), stringsAsFactors=FALSE)
    colnames(tab)<-ti
    rownames(tab)<-1:nrow(tab)
    tab[tab=='']<-NA
    return(tab)
  }
  else{
    return(NA)
  }
}

money2num<-function(x){
  #Function to change money into numeric value
  as.numeric(gsub('[^0-9\\.]', '', x))
}



#single test section###################
fname='1.html'
x<-scan(file = fname, what='', sep='\n', quiet = TRUE)
myinfo<-rep(NA, 39)
getit<-function(i){
  myreg(col.patterns[i],x,col.repat[i],numonly = col.num[i])
}
myinfo[usearch]<-sapply(usearch, getit)
myinfo[24]<-myreg('<td>Garage</td>',x,'\\S+',numonly = TRUE, nextline = 1)
extab<-catchtab(x,'Extra Features')
if(class(extab)=='data.frame'){
  myinfo[19]<- sum(money2num(extab[,grep('[Vv][Aa][Ll][Uu][Ee]', colnames(extab))]))
}
saletab<-catchtab(x,'Ownership History')
if(class(saletab)=='data.frame'){
  myinfo[25:39]<-getsale(saletab)
}
#single test section###################

myparse<-function(i){
  #Function to parse htmls and retrieve information
  #Only for sapply
  fname<-paste(i, '.html', sep = '')
  #Read files when they exist
  myinfo<-rep(NA, 41)
  myinfo[40:41]<-0
  myinfo[1]<-i
  if(file.exists(fname)){
    x<-scan(file=fname, what='', sep='\n', quiet = TRUE)
    getit<-function(i){
      myreg(col.patterns[i],x,col.repat[i],numonly = col.num[i])
    }
    myinfo[usearch]<-sapply(usearch, getit) 
    myinfo[24]<-myreg('<td>Garage</td>',x,'\\S+',numonly = TRUE, nextline = 1)
    extab<-catchtab(x,'Extra Features')
    if(class(extab)=='data.frame'){
      myinfo[19]<- sum(money2num(extab[,grep('[Vv][Aa][Ll][Uu][Ee]', colnames(extab))]))
    }
    saletab<-catchtab(x,'Ownership History')
    if(class(saletab)=='data.frame'){
      myinfo[25:39]<-getsale(saletab)
    }
    if(length(grep('Building 2', x)) > 0){
      myinfo[40]<-1
    }
    if(length(grep('\\+',myinfo[14])) > 0){
      myinfo[41]<-1
    }
    myinfo[14]<-as.numeric(gsub('[^0-9\\.]', '', myinfo[14]))
  }
  #Assign value to info
  return(myinfo)
}
saleinfo<-as.vector(sapply(1:5,function(i) sapply(c('buyer','price','date'),
                              function(x) paste(x, i, sep=''))))
col.names[25:41]<-c(saleinfo, 'multibuilding', 'nineplus')
#Save results as a matrix
system.time(temp<-sapply(1:27307, myparse))
info<-as.data.frame(t(temp),stringsAsFactors = FALSE)
info<-tonum(info, which(col.num==TRUE))
#Transform the logic columns into the right form
info[,40]<-info[,40]!=0
info[is.na(info[,2]),40]<-NA
info[,41]<-info[,41]!=0
info[is.na(info[,14]),41]<-NA
colnames(info)<-col.names
#Clean out the confusion for ',' separator
for(i in c(4, 25, 28, 31, 34, 37)){
  info[,i]<-gsub(',', '', info[, i])
}

#Write data into csv file
write.csv(x = info, file = '625.csv', quote = FALSE, row.names = FALSE)