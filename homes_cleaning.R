library(XML)
library(RCurl)
library(DataComputing)
library(rvest)
URL <- "https://www.zillow.com/homes/for_rent/San-Francisco-CA/house,mobile,townhouse_type/15193995_zpid/20330_rid/featured_sort/38.00996,-122.320404,37.625381,-122.57103_rect/10_zm/"
doc <- URL %>%  #doc is a parsed HTML document
  getURLContent()%>%
  htmlParse()

info <- doc %>%
  xpathSApply('//div[@class = "zsg-lg-2-3 zsg-sm-1-1 hdp-header-description"]', xmlValue)


get_info = 
  function(data = doc, link = name)
  {
    info <- doc %>% xpathSApply(paste('//div[@class =', name, "]", sep = ""), xmlValue)
    
    info 
  }


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

links = c()

for (i in 1:length(mainPage)) {
  
  new_link <- paste(baseURL, mainPage[i], sep = "")
  links <- c(links, new_link )
  
}

for (i in 1:12){
  added <- paste(toString(i), "_p/", "")
  newUrl <- paste(mainUrl, added, "")
  
  mainPage <- newUrl %>% 
    getURLContent() %>%
    htmlParse() %>%
    get_links()
  
  for (i in 1:length(mainPage)) {
    
    new_link <- paste(baseURL, mainPage[i], sep = "")
    links <- c(links, new_link )
    
  }
  
}

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
rent_p_sq <- c()
address <- c()
zip <- c()

for (elem in 1:length(links)){
  URL <- links[elem]
  doc <- URL %>% getURLContent() %>%
    htmlParse() %>%
    xpathSApply(paste('//div[@class =', '"hdp-fact-ataglance-value"', "]", sep = ""), xmlValue)
  doc2 <- URL %>% getURLContent() %>%
    htmlParse() %>%
    xpathSApply('//div[@class = "main-row  home-summary-row"]', xmlValue)
  
  doc3 <- URL %>% getURLContent() %>%
    htmlParse() %>%
    xpathSApply('//span[@class = "addr_bbs"]', xmlValue)
  
  # doc4 <- URL %>% getURLContent() %>%
  #       htmlParse() %>%
  #       xpathSApply('//span[@class ="hdp-fact-value"]', xmlValue)
  
  doc5 <- URL %>% getURLContent() %>%
    htmlParse() %>%
    xpathSApply("//h1[@class = 'notranslate']", xmlValue)
  
  doc6 <-  URL %>% getURLContent() %>%
    htmlParse() %>%
    xpathSApply("//span[@class = 'zsg-h2 addr_city']", xmlValue)
  
  
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
  #rent_p_sq <- c(rent_p_sq, doc4[3])
  address <- c(address, doc5) #needs to have city omitted
  zip <- c(zip, doc6)
}

#Regex to change strings to numerical 
prices <- as.numeric(gsub("\\D", "", prices)) #per month 
num_beds <-  as.numeric(gsub("\\D", "", num_beds))
num_baths <- as.numeric(gsub("\\D", "", num_baths))
sqft <- as.numeric(gsub("\\D", "", sqft))
zip <- as.numeric(gsub("\\D", "", zip))
#No data to NA
type <- gsub("No Data", NA, type)
laundry <- gsub("No Data", NA, laundry)
heating <- gsub("No Data", NA, heating)
cooling <- gsub("No Data", NA, cooling)
parking <- gsub("No Data", NA, parking)

temp <- strsplit(address, "Incomplete")
new_add <- c()
for (i in 1:length(temp)){
  new_add <- c(new_add, temp[[i]][1])
  
}

temp2 <- strsplit(new_add, ",")
new_add2 <- c()
for (i in 1:length(temp2)){
  new_add2 <- c(new_add2, temp2[[i]][1])
  
}



df <- data.frame(type, laundry, heating, cooling, parking, prices, num_beds, num_baths, sqft, new_add2, zip)

names(df) <- c("House Type", "Laundry", "Heating", "Cooling", "Parking", "Rent/Month", "Beds", "Baths", "Square Feet", "Address", "Zipcode")
df

write.csv(df, "~/statProject/homes.csv")