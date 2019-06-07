# Scrape data from eCommerce Site using R and JSON
Scraping data from web has become very easy through libraries which read html. The best example I found and used for a couple of months for it's simplicity and tidy principles is `rvest` by Hadley Wickham

However, scraping data from sites which employ Infinite Scroll becomes tricky as loading of a category, viz. men-tshirts becomes a challenge due to very high number of options present. There are 2 ways to handle such situations. 
1. Use RSelenium currently maintained by ROpenSci
2. Read JSONs recieved

The code written in R scrapes JSONs from an indian ecommerce site, jaypore.com, however, it can be extrapolated to get product level data from most ecommerce sites across the globe
