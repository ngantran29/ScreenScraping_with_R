### Set options to increase Java heap space
options(java.parameters = "-Xmx")
### Increase memory limit
memory.limit(size=10000000000024)
###
library("XML")
library("dplyr")
library("ggplot2")
library("RSelenium")

###

webPath <- "https://www.nytimes.com/search?endDate=20181204&query=trade%20war%20china&sort=best&startDate=20181203"
SeeMoreButton <- "//div[@class='css-vsuiox']/button"
LinkPath <- '//div[@class = "css-138we14"]/a'
ColumnPath <- '//p[@class = "css-myxawk"]'
ColumnList <- c("AMERICAS", "ASIA PACIFIC","BUSINESS","ECONOMY","POLITICS"
                ,"U.S.","WORLD","OPINION", "MEDIA", "DEALBOOK")
### Create remote driver and navigate to the search results of rewords
driver<- rsDriver(browser=c("firefox"))
remDr <- driver[["client"]]
remDr$navigate(webPath)

### Navigate full list of results from "See more" button
### This process has been slowed down with many pauses to avoid blocking the website traffic

for (i in 1:50) {
  smbutton <- remDr$findElement(using = 'xpath',
                                value = SeeMoreButton)
  smbutton$clickElement()
  Sys.sleep(sample(2, 1))
}

### Scraping links of articles

webElems <- remDr$findElements(using = 'xpath',
                               value = LinkPath)

nylinks <- unlist(lapply(webElems, function(x){x$getElementAttribute('href')}))

webElems1 <- remDr$findElements(using = 'xpath',
                               value = ColumnPath)
nycollumn <- unlist(lapply(webElems1, function(x){x$getElementText()}))

nylinksdf <- data.frame(nylinks, nycollumn)

# Select relevant columns/remove irrelevant columns
nylinksdf <- filter(nylinksdf, nycollumn %in% ColumnList)
nylinksdf <- filter(nylinksdf, !nycollumn %in% c("BRIEFING"))
nytlinks.unique <- nytlinks[!duplicated(nytlinks$nylinks), ]

### Parse the article

library("RSelenium")
#Set the path to article, title, author, publish date
artPath <- '//div[@class="css-1fanzo5 StoryBodyCompanionColumn"]/div/p'
titlePath <- '//h1/span[@class="balancedHeadline"]'
datePath <- '//time[@class = "css-qddhf4 e16638kd0"]'
authorPath <- '//p[@class = "css-16vrk19 e1jsehar1"]/a/span'
### Create remote driver and navigate to the search results of topic "Trump"
driver<- rsDriver(browser=c("firefox"))
remDr <- driver[["client"]]
remDr$navigate(nylinksdf$nylinks[1])
Id <- 1
art_ny <- xml2::read_html(remDr$getPageSource()[[1]])
ny_art <- art_ny %>% html_nodes(xpath=artPath) %>% html_text('p')
ny_art <- paste(ny_art, collapse = ' ')
ny_title <- art_ny %>% html_nodes(xpath=titlePath) %>% html_text('span')
ny_date <- art_ny %>% html_nodes(xpath=datePath) %>% html_attr('datetime')
ny_author <- art_ny %>% html_nodes(xpath=titlePath) %>% html_text('span')
ny_author <- paste(ny_author, collapse = ' , ')
ny_author <- ny_author[!duplicated(ny_author)]
ny <- data.frame(Id, ny_title, ny_art, ny_date, ny_author)
dataClean <- function(text){
  text <- text[!duplicated(text)]
  text <- paste(text, collapse = ' ')
  text[text== ""] = NA
}
### loop over the list of links to
for (i in 2 :length(nylinksdf$nylinks)) {
  remDr$navigate(nylinksdf$nylinks[i])
  art_ny <- xml2::read_html(remDr$getPageSource()[[1]])
  Id1 = i
  ny_art1 <- art_ny %>% html_nodes(xpath=artPath) %>% html_text('p')
  dataClean(ny_art1)
  ny_title1 <- art_ny %>% html_nodes(xpath=titlePath) %>% html_text('span')
  dataClean(ny_title1)
  ny_date1 <- art_ny %>% html_nodes(xpath=datePath) %>% html_attr('datetime')
  dataClean(ny_date1)
  ny_author1 <- art_ny %>% html_nodes(xpath=authorPath) %>% html_text('span')
  dataClean(ny_author1)
  ny1 <- data.frame(Id1, ny_title1, ny_art1, ny_date1, ny_author1)
  colnames(ny1) <- c( 'Id', 'ny_title', 'ny_art', 'ny_date', 'ny_author')
  ny <- rbind(ny, ny1)
  Sys.sleep(sample(1:2, 1))
}

ny $agency <- "nytimes"
colnames(ny) <- c( 'Id', 'title', 'art', 'date', 'author', 'agency')
write.csv(ny, file = "ny.csv")
nyt <- dplyr::filter(nyt, grepl('china|chinese|beijing|PRC|Xi Jinping|Sino|US-Sino', ignore.case = TRUE, ny_art))
nyt.unique <- nyt[!duplicated(nyt$ny_title), ]
