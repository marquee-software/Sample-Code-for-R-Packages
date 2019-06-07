library(jsonlite)
library(tidyverse)

# Save the JSON url in `jsonUrl`
# To find the JSON url navigate to the category needed to scrape and open developer setting in chrome
# Select Network tab, the select XHR to find all the jsons transferred from the server
# In the url, string 'p=1' suggests page 1
jsonUrl <- "https://www.jaypore.com/shop_sorting_ajax_test.php?shopId=345&mulShopIds=&sort=featured&p=1&max_price=&min_price=&max_off=&min_off=&mobile=false&brand_ids=0&sale_only=0&size="

# Read json from the url using read_json
p1_Data <- read_json(jsonUrl, simplifyVector = T)

# Explore data present in json using glimpse
glimpse(p1_Data)

# Assign i value 1
i <- 1
# Assign an empty list
dfList <- list()

repeat{
    # Read json by replacing page no with value in 1
    pageDf <- read_json(str_replace(jsonUrl, "&p=1", paste0("&p=", i)), simplifyVector = TRUE)
    # break if no products present
    if(is.null(nrow(pageDf))){
        break
    }
    # Save dataframe in a list and append the list
    dfList[[i]] <- pageDf
    # status print
    print(paste0("Compeleted Scraping page:", i))
    # Increase value of i by 1 to scrape next page in the loop
    i <- i + 1
}

# append all dataframe into 1 and slect relevant columns only
finalDF <- dplyr::bind_rows(dfList) %>%
        select(url, id, specialprice, percentoff, price, brand_name, name) %>%
        mutate(segment = urls$Segment[j], 
               category = urls$Category[j], 
               brick = urls$Brick[j], 
               subBrick = urls$subBrick[j])
