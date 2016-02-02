
library(XML)
library(RCurl)

#tried via httr content(GET(url)) with credentials - only 10 results per day
#search in google using getURL (~500 requests)
googleSearchURL<-function(search.string,domain='.co.uk',quotes=FALSE)
{
  res<-data.frame()
  search.query<-search.string
  search.string<-gsub(' ','%20',search.string)
  if (quotes) search.string<-paste('%22',search.string,'%22',sep='')
  search.URL<-paste('http://www.google',domain,'//search?q=',search.string,sep='')
  #get search results
  search.row<-getURL(search.URL,httpheader = c("User-Agent" = "R(2.10.0)"))
  #parse results and set href node
  search.row_parse<-htmlTreeParse(search.row,useInternalNodes = TRUE, error=function(...){})
  nodes <- getNodeSet(search.row_parse, "//h3[@class='r']//a")
  #get links 
  search.links<-sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]])
  #in case of no search results
  if(any(!is.na(as.character(search.links)))) {
    #first 5 links will be enough for my purposes
    res<-strsplit(search.links,"/")[[1]][4]
    for (i in 2:5){
      res<-cbind(res,strsplit(search.links,"/")[[i]][4])
    }
  } 
  return(res)
}


#search in google using ajax.googleapis.com (~100 requests)
googleSearchAJAX<-function(search.string,quotes=FALSE)
{
  res<-data.frame()
  search.query<-search.string
  search.string<-gsub(' ','%20',search.string)
  if (quotes) search.string<-paste('%22',search.string,'%22',sep='')
  search.json<-getURL(paste("https://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=",search.string,sep=""))
  search.parse<-fromJSON(search.json)
  #in case of no search results 
  if (length(search.parse$responseData$results)>0){
    res<-search.parse$responseData$results[[1]]$url
    # there are only 4 results
    for (i in 2:4){
      res<-cbind(res,search.parse$responseData$results[[i]]$url)
    }
  }
  return(res)
}


