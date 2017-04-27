library(rvest)
library(XML)
library(RCurl)
main.page <- read_html("https://www.zillow.com/homes/for_rent/San-Francisco-CA/apartment_duplex_type/20330_rid/37.791575,-122.347827,37.756635,-122.424216_rect/13_zm/")
main.link <- "https://www.zillow.com/homes/for_rent/San-Francisco-CA/apartment_duplex_type/20330_rid/37.791575,-122.347827,37.756635,-122.424216_rect/13_zm/"

#get links to every page 
# all pages are of form https://www.zillow.com/homes/for_rent/San-Francisco-CA/apartment_duplex_type/20330_rid/37.791575,-122.347827,37.756635,-122.424216_rect/13_zm/NUMBER_p/

#obtain the URLs of all the details of rental locations in each page

link_list = list()
for (i in 1:20){  
  main.link1 = sprintf("https://www.zillow.com/homes/for_rent/San-Francisco-CA/apartment_duplex_type/20330_rid/37.82565,-122.331219,37.69917,-122.550945_rect/11_zm/%d_p/", i)
  subs1 <-  main.link1 %>% 
    getURLContent() %>% 
    htmlParse() %>% 
    xpathSApply("//a[@class = 'zsg-photo-card-overlay-link routable hdp-link routable mask hdp-link']/@href")
  baseURL <- "https://www.zillow.com"
  link_list[[i]] = paste(baseURL, as.character(subs1), sep = "")
}

link_long = unlist(link_list)


#scrape details we want from the page
df = data.frame()
getDetails <- function(link_long_list) {
for (link in link_long_list){
  doc <-  link %>% 
    getURLContent() %>% 
    htmlParse()
  addr <- xpathApply(doc, "//h1[@class='notranslate']", xmlValue)
  price <- xpathApply(doc, "//div[@class='main-row  home-summary-row']/span[@class]", xmlValue)
if (length(price) == 0) { next
  # price = xpathSApply(doc, "//span[@class='floorplan-unit-price']", xmlValue)
  # #return a list with number of bedrooms, bathrooms, and square footage
  #   bd_ba_ft <- xpathSApply(doc, "//span[@class = 'floorplan-title']", xmlValue)
  #   features <- list("Type" = NA,   "Laundry"  = NA,"Heating"  = NA,"Cooling"  = NA, "Pets"   = NA,  "Parking" = NA)

} else {
  bd_ba_ft <- xpathApply(doc, "//span[@class = 'addr_bbs']", xmlValue)
  #return a list of features
  features <- xpathApply(doc, "//div[@class = 'zsg-media-bd']/div[@class = 'hdp-fact-ataglance-value']" , xmlValue)
  ft_name <- unlist(xpathApply(doc, "//div[@class = 'zsg-media-bd']/p" , xmlValue))[1:6]
}

feature_list <- c(addr, price, bd_ba_ft, features)
names(feature_list) <- c("Address", "Rent", "Beds", "Ba", "Area", unlist(xpathApply(doc, "//div[@class = 'zsg-media-bd']/p" , xmlValue))[1:6])
df = rbind(df, data.frame(t(as.matrix(unlist(feature_list)))))
}
  df
}

#these take a while
df1 = getDetails(link_long[1:70])
df2 = getDetails(link_long[71:150])
df3 = getDetails(link_long[151:250])
df4 = getDetails(link_long[251:350])
df5 = getDetails(link_long[351:422])
main_df <- rbind(df1, df2, df3, df4, df5)
View(main_df)
                                            
write.csv(main_df, "~/College Academics/3rd Year/Stat 133/statProject/apt.csv")

#cleaning the dataframe
main_df <-  read.csv("~/College Academics/3rd Year/Stat 133/statProject/apt.csv")


library(dplyr)
library(DataComputing)
empt = "Incomplete address or missing price?Sometimes listing partners send Zillow listings that do not include a full address or price.To get more details on this property, please contact the listing agent, brokerage, or listing provider."

main_df1 <- main_df %>% 
  mutate(Area = as.numeric(gsub("(sqft)|-|,", "", Area)), Rent = as.numeric(gsub("[///mo$,]", "", Rent)), Ba = gsub("[baths]", "", Ba), 
         Beds = gsub("(beds|bed)", "",  Beds), Address = as.character(Address)) %>% 
  mutate(zip = as.numeric(substr(Address, nchar(Address) - 5, nchar(Address))))

main_df1[main_df1 == "No Data"] = NA
main_df1[main_df1 == " "] = NA
write.csv(main_df1,"~/College Academics/3rd Year/Stat 133/statProject/apt1.csv")


#after doing some excel cleaning 
apt1 <- read.csv("~/College Academics/3rd Year/Stat 133/statProject/apt2.csv")
View(apt1)

#Summary histogram plot
ggplot(apt1, aes(Rent)) + geom_histogram(aes(fill = Laundry), bins = 50)

#build rpart model 
library(statisticalModeling)
library(rpart)
library("rpart.plot")
rpart(Rent ~ Laundry + Pets + Area + Beds + Ba + zip + Type + Heating + Cooling + Parking,  data=apt1, cp = 0.01) %>% rpart.plot::prp(type=3)
