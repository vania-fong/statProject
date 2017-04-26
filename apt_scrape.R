library(rvest)
library(XML)
library(RCurl)
main.page <- read_html("https://www.zillow.com/homes/for_rent/San-Francisco-CA/apartment_duplex_type/20330_rid/37.791575,-122.347827,37.756635,-122.424216_rect/13_zm/")
main.link <- "https://www.zillow.com/homes/for_rent/San-Francisco-CA/apartment_duplex_type/20330_rid/37.791575,-122.347827,37.756635,-122.424216_rect/13_zm/"

#get links to every page 
# all pages are of form https://www.zillow.com/homes/for_rent/San-Francisco-CA/apartment_duplex_type/20330_rid/37.791575,-122.347827,37.756635,-122.424216_rect/13_zm/NUMBER_p/

#obtain the URLs of all the details of rental locations in each page

link_list = list()
for (i in 1:12){  
  main.link1 = sprintf("https://www.zillow.com/homes/for_rent/San-Francisco-CA/apartment_duplex_type/20330_rid/37.791575,-122.347827,37.756635,-122.424216_rect/13_zm/%d_p/", i)
  subs1 <-  main.link1 %>% 
    getURLContent() %>% 
    htmlParse() %>% 
    xpathSApply("//a[@class = 'zsg-photo-card-overlay-link routable hdp-link routable mask hdp-link']/@href")
  baseURL <- "https://www.zillow.com"
  link_list[[i]] = paste(baseURL, as.character(subs1), sep = "")
}

#scrape details we want from the page
# getDetails <- function(link) {
  doc <-  link %>% 
    getURLContent() %>% 
    htmlParse()
  addr <- xpathApply(doc, "//span[@class='zsg-h2 addr_city']", xmlValue)
  price <- xpathApply(doc, "//div[@class='main-row  home-summary-row]/span[@class]']", xmlValue)
  
  
