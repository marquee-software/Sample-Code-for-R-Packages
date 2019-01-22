library(tidyverse)
library(rvest)
library(jsonlite)
library(RCurl)
library(XML)
library(rebus)
library(readxl)

urlFILE <- read_excel("urlFILE.xlsx") %>%
    filter(crawl == 'Not Done')

jTemp <- 1

for(j in jTemp:nrow(urlFILE)){
    
    jTemp <- j
    
    result <- as.data.frame(matrix(ncol = 5, nrow = 0))
    names(result) <- c("brand", "price_final", "price_listed", "description", "reference")
    
    url0 <- urlFILE$url[j]
    
    if(urlFILE$type == 'stock'){
        url <- paste0(url0, "?rows=50&p=")
    }
    else{
        url <- paste0(url0, "&rows=50&p=")
    }
    
    productCount <- read_html(url0) %>%
        html_node(".index-productCount") %>%
        html_text()
    
    n <- ceiling(as.integer(str_extract(productCount, one_or_more(DGT)))/50)
    # alter i to the number of 
    
    print(paste0("Start scraping: ", urlFILE$brick[j]))
    print(paste0("Pages to be scraped: ", n))
    Sys.sleep(2)
    
    iTemp <- 1
    
    for (i in iTemp:n){
        obj <- read_html(paste0(url, i))
        
        iTemp <- i
        
        Sys.sleep(1)
        # extract brand names
        brand <- as.data.frame(html_nodes(obj, ".product") %>% html_node(".productInfo") %>% html_node(".name") %>% html_text())
        
        # extract discounted price
        price_final <- as.data.frame(html_nodes(obj, ".product") %>% html_node(".productInfo") %>% html_node(".price-discounted") %>% html_text())
        
        # extract listed price
        price_listed <- as.data.frame(html_nodes(obj, ".product") %>% html_node(".productInfo") %>% html_node(".price") %>% html_text())
        
        # extract short description
        description <- as.data.frame(html_nodes(obj, ".product") %>% html_node(".productInfo") %>% html_node(".name-product") %>% html_text())
        
        # extract refererence
        reference <- as.data.frame(html_nodes(obj, ".product") %>% html_nodes("a") %>% html_attr("href"))
        
        names(brand) <- "brand"
        names(price_final) <- "price_final"
        names(price_listed) <- "price_listed"
        names(description) <- "description"
        names(reference) <- "reference"
        
        result <- result %>%
            bind_rows(brand %>%
                          bind_cols(price_final %>%
                                        bind_cols(price_listed %>%
                                                      bind_cols(description %>%
                                                                    bind_cols(reference)))))
        
        print(i)
    }
    
    write_csv(result, "result.csv")
    
    result_FINAL <- read_csv("result.csv")
    result_FINAL$price_final <- as.integer(substr(result_FINAL$price_final, 2, nchar(result_FINAL$price_final)))
    
    result_FINAL <- result_FINAL %>%
        mutate(price_listed = ifelse(is.na(price_listed), price_final, price_listed),
               discount = (price_listed - price_final)/price_listed) %>%
        select(reference, brand, description, price_listed, discount, price_final) %>%
        distinct()
    
    write_csv(result_FINAL, urlFILE$filename[j])
    file.remove("result.csv")
    print(paste0("Scraping completed for: ", urlFILE$brick[j]))
    Sys.sleep(5)
    gc()
}
