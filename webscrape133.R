knitr::opts_chunk$set(echo = TRUE)
library(XML)
library(RCurl)
library(DataComputing)
library(printr)

get_links = 
  function(doc)
  {
    info <- doc %>% xpathSApply("//a[@class = 'zsg-photo-card-overlay-link routable hdp-link routable mask hdp-link']/@href")
    
  }



mainUrl <- "https://www.zillow.com/homes/for_rent/San-Francisco-CA/house,mobile,townhouse_type/20330_rid/featured_sort/38.00996,-122.320404,37.625381,-122.57103_rect/10_zm/"

baseURL <- "https://www.zillow.com"

mainPage <- mainUrl %>% 
  getURLContent() %>%
  htmlParse() %>%
  get_links()

links = c() #get all the links in one page (the main page)

#Note: Will add all pages later. One page take significant amoutn of time to load already... 

for (i in 1:length(mainPage)) {
  
  new_link <- paste(baseURL, mainPage[i], sep = "")
  links <- c(links, new_link )
  
}

#This function is not too useful. Leaving here for now. 
get_param <- 
  function(name) {
    
    for (elem in 1: length(links)) {
      URL <- links[elem]
      doc <- URL %>%  #doc is a parsed HTML document
        getURLContent() %>%
        htmlParse() %>%
        xpathSApply(paste('//div[@class =', name, "]", sep = ""), xmlValue)
      doc
    }
    
  }


#Facts and Features asset 

#Different parameters 
type <- c()
laundry <- c()
heating <- c()
cooling <- c()
pets <- c()
parking <- c()
prices <- c()
num_beds <- c()
num_baths <- c()
sqft <- c()

for (elem in 1:length(links)){
  URL <- links[elem]
  doc <- URL %>% getURLContent() %>%
    htmlParse() %>%
    xpathSApply(paste('//div[@class =', '"hdp-fact-ataglance-value"', "]", sep = ""), xmlValue)
  doc2 <- URL %>% getURLContent() %>%
    htmlParse() %>%
    xpathSApply('//div[@class = "main-row  home-summary-row"]', xmlValue)
  
  doc3 <- doc2 <- URL %>% getURLContent() %>%
    htmlParse() %>%
    xpathSApply('//span[@class = "addr_bbs"]', xmlValue)
  
  type <- c(type, doc[1])
  laundry <- c(laundry, doc[2])
  heating <- c(heating, doc[3])
  cooling <- c(cooling, doc[4])
  pets <- c(pets, doc[5])
  parking <- c(parking, doc[6])
  prices <- c(prices, doc2[1])
  num_beds <- c(num_beds, doc3[1])
  num_baths <- c(num_baths, doc3[2])
  sqft <- c(sqft, doc3[3])
}

#table of above parameters
df <- data.frame(type, laundry, heating, cooling, parking, prices, num_beds, num_baths, sqft)

df

#Note: Above parameters are parameters that are common to all the houses. Found that some/most parameters are not. Will look through to see what parameters seem useful.
