#'
#' @title China's universities Crawler
#' @author r3dmaohong
#' @date 2017/03/09
#' @description Crawler for China's universities information. 
#'              (Three sources. Baidu, gatzs and google.)
#' @keywords rvest, 
#'           rselenium click event by location,
#'           rselenium search event

# Run selenium server
# cmd
# java -jar selenium-server-standalone-2.53.1.jar
library(RSelenium)
library(rvest)
library(httr)
library(ropencc) # For transfering Chinese between "Traditional" and "Simplified" Chinese
library(stringr)

setwd("git/UniversityInfoCrawler")

# http://www.studychina.tw/undergraduate/un_infor.htm
chinaHS <- read.csv("大陸地區大學及高等教育機構認可名冊155所.csv", stringsAsFactors = FALSE)
# Session
uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

#' First Part
#' Baibu
#' Using rvest
##### Start #####

for(i in 1:nrow(chinaHS)){
  url <- paste0("http://baike.baidu.com/item/", converter(T2S)[chinaHS$校名[i]])
  #remDr$navigate(url)# website to crawl
  total_css <- read_html(html_session(url, user_agent(uastring)))
  title <- total_css %>% html_nodes(".cmn-clearfix .name") %>% html_text()
  value <- total_css %>% html_nodes(".cmn-clearfix .value") %>% html_text()
  
  title <- converter(S2T)[title]
  value <- converter(S2T)[value]
  
  title <- c(title, "校名", "英文名", "簡介")
  value <- c(value, total_css %>% html_nodes("h1") %>% html_text() %>% converter(S2T)[.] %>% toString,
             total_css %>% html_nodes(".poster-top h3") %>% html_text() %>% converter(S2T)[.] %>% toString,
             total_css %>% html_nodes(".poster-top .para") %>% html_text() %>% converter(S2T)[.] %>% paste0(., collapse = "\n")
  )
  
  tmp <- data.frame("title" = title, "content" = value, stringsAsFactors = FALSE)
  
  #dir.create("output")
  write.csv(tmp, paste0("output/", chinaHS$校名[i], ".csv"), row.names = FALSE)
  cat("\r", format(round(i/nrow(chinaHS)*100, 2), nsmall = 2), "%", rep(" ", 10))
  Sys.sleep(runif(1, 2, 5))
}

baidu.files<- list.files("output/baidu", full.names = TRUE)

for(i in 1:length(baidu.files)){
  tmp <- read.csv(baidu.files[i], stringsAsFactors = FALSE)
  tmp$title   <- gsub("<U+00A0>", "", tmp$title, fixed = TRUE)
  tmp$content <- gsub("<U+00A0>", "", tmp$content, fixed = TRUE)
  write.csv(tmp, baidu.files[i], row.names = FALSE)
}
##### End #####

#' Second Part
#' gatzs
#' Using rvest and Rselenium
#' Rselenium : Click tabs by finding its location 
##### Start #####
#http://www.gatzs.com.cn/

ssdm    <- read.csv("招生信息網列表ssdm.csv", stringsAsFactors = FALSE)
totalhs <- data.frame("area" = character(), "hsname" = character(), "hshref" = character(), "hstype" = character(), stringsAsFactors = FALSE)

for(i in 26:nrow(ssdm)){
  url <- paste0("http://www.gatzs.com.cn/yxxx/welcome.action?ssdm=", ssdm$ssdm[i])
  
  total_css <- read_html(html_session(url, user_agent(uastring)))
  hsname <- total_css %>% html_nodes(".ui-column-head-title a") %>% html_text()
  hshref <- total_css %>% html_nodes(".ui-column-head-title a") %>% html_attr("href")
  hshref <- paste0("http://www.gatzs.com.cn", hshref)
  hstype <- total_css %>% html_nodes(".sch-type") %>% html_text()
  
  if(toString(hsname)!=""){
    totalhs <- rbind(totalhs,
                     data.frame("area" = ssdm$area[i], "hsname" = hsname %>% converter(S2T)[.], "hshref" = hshref, "hstype" = hstype %>% converter(S2T)[.], stringsAsFactors = FALSE)
    )
  }
  
  pages <- total_css %>% html_nodes(".ui-paging-bold") %>% html_text()
  Sys.sleep(runif(1, 5, 7))
  if(grepl("/" , toString(pages), fixed = TRUE)){
    pageindex <- as.numeric(str_split_fixed(pages, "/", 2)[,2]) - 1
    if(pageindex > 0){
      for(page in seq(pageindex)){
        pageurl <- paste0(url, "&start=", page*6)
        
        total_css <- read_html(html_session(pageurl, user_agent(uastring)))
        hsname <- total_css %>% html_nodes(".ui-column-head-title a") %>% html_text()
        hshref <- total_css %>% html_nodes(".ui-column-head-title a") %>% html_attr("href")
        hshref <- paste0("http://www.gatzs.com.cn", hshref)
        hstype <- total_css %>% html_nodes(".sch-type") %>% html_text()
        
        totalhs <- rbind(totalhs,
                         data.frame("area" = ssdm$area[i], "hsname" = hsname %>% converter(S2T)[.], "hshref" = hshref, "hstype" = hstype %>% converter(S2T)[.], stringsAsFactors = FALSE)
        )
        Sys.sleep(runif(1, 4, 6))
      }
    }
  }
  cat("\r", format(round(i/nrow(ssdm)*100, 2), nsmall = 2), "%", rep(" ", 10))
}
totalhs <- unique(totalhs)
write.csv(totalhs, paste0("output/", "港澳台招生信息網", ".csv"), row.names = FALSE)

# Inner pages
category <- read.csv("gatzs學校頁籤query.csv", stringsAsFactors = FALSE)
names(totalhs)

remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName ="firefox"
)
remDr$open() #open browser

hsInfo <- data.frame("area" = character(), "hsname" = character(), "hshref" = character(), "hstype" = character(), "cate" = character(), "content" = character(), stringsAsFactors = FALSE)

x <- 1
for(i in 1:nrow(totalhs)){
  url <- totalhs$hshref[i]
  remDr$navigate(url)
  
  tryCatch({
    for(j in 1:20){
      css <- paste0("#sch_nav li:nth-child(", j, ") a")
      ##invisible
      webElem <- remDr$findElement("css selector", css)
      #webElem$highlightElement(1)
      cate <- webElem$getElementText() %>% unlist
      
      #webElem$clickElement()
      remDr$mouseMoveToLocation(webElement = webElem) # move to the required element
      remDr$click() # left mouse button click 
      #webElem$sendKeysToElement(list(key = "control", "t"))
      
      page_source <- remDr$getPageSource()
      total_css <- read_html(page_source[[1]])
      
      content <- total_css %>% html_nodes("#sch_cont") %>% html_text()
      content <- content %>% converter(S2T)[.]
      cate <- cate %>% converter(S2T)[.]
      
      hsInfo[x, ] <- c(totalhs[i, ], 
                       cate, content)
      
      Sys.sleep(runif(1, 2, 5))
      x <- x + 1
    }
  }, error = function(e) {
  })
  cat("\r", format(round(i/nrow(totalhs)*100, 2), nsmall = 2), "%", rep(" ", 10))
}
write.csv(hsInfo, "output/hsInfo.csv", row.names = FALSE)

#hsInfo <- readRDS("hsInfo.rds")
#hsInfo$content <- hsInfo$content %>% strsplit(., "(?<=.{10000})", perl = TRUE)
#library(data.table)
#hsInfo <- setDT(hsInfo)[, lapply(.SD, function(x) unlist(x)), by = .(area, hsname, hshref, hstype, cate)
#         ][!is.na(content)]
#write.csv(hsInfo, "output/hsInfo2.csv", row.names = FALSE)

hsInfo$cate[which(nchar(hsInfo$content)>10000)]
hsInfo$content <- ifelse(nchar(hsInfo$content)>10000, substr(hsInfo$content, 1, 10000), hsInfo$content)
hsInfo$content <- gsub("  ", "", hsInfo$content)
hsInfo$content <- gsub("\t", "", hsInfo$content)
hsInfo$content <- gsub("\n ", "\n", hsInfo$content)
hsInfo$content <- gsub("\n\n", "\n", hsInfo$content)

for(i in 1:ncol(hsInfo)){
  hsInfo[,i]   <- gsub("<U+00A0>", "", hsInfo[,i], fixed = TRUE)
}
write.csv(hsInfo, "output/hsInfo_less1w.csv")
##### End #####

#' Third Part
#' google
#' Using rvest and Rselenium
#' This method is bad, it can be done by rvest...
#' But let's try something new in Rselenium... 
##### Start #####
remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName ="firefox"
)
remDr$open() #open browser

chinaHS <- read.csv("大陸地區大學及高等教育機構認可名冊155所.csv", stringsAsFactors = FALSE)

url <- "https://www.google.com.tw/"
remDr$navigate(url)

rtnDF <- data.frame("title" = character(), 
                    "subtitle" = character(), 
                    "content" = character(), stringsAsFactors = FALSE)

for(i in 1:nrow(chinaHS)){
  webElem <- remDr$findElement("css selector","#lst-ib")
  webElem$clearElement() # Clear text in element
  #Sys.sleep(runif(1, 9, 14))
  webElem$sendKeysToElement(list(chinaHS$校名[i] %>% iconv(from = "BIG-5", to = "UTF-8")))
  if(i == 1){
    Sys.sleep(runif(1, 9, 14))
    webElem <- remDr$findElement("css selector","center input")
  }else{
    webElem <- remDr$findElement("css selector",".sbico-c")
  }
  #webElem <- remDr$findElement("css selector","center input")
  #if(!webElem$isElementDisplayed()[[1]]){
  #  webElem <- remDr$findElement("css selector",".sbico-c")
  #}
  #Sys.sleep(runif(1, 9, 12))
  webElem$clickElement()
  #Sys.sleep(runif(1, 9, 14))
  
  page_source <- remDr$getPageSource()
  total_css <- read_html(page_source[[1]])
  
  title <- total_css %>% html_nodes("._hdf") %>% html_text() %>% toString()
  if(title==""){
    title <- total_css %>% html_nodes("._Q1n") %>% html_text()
  }
  
  cnt.elm <- total_css %>% html_nodes("._xle div span") %>% html_text()
  sub.elm <- total_css %>% html_nodes("._xle .kno-fb-ctx") %>% html_text()
  #sub.elm.ttl <- total_css %>% html_nodes("._xle .fl") %>% html_text()
  #sub.elm.ctn <- total_css %>% html_nodes("._xle ._Xbe") %>% html_text()
  #total_css %>% html_nodes(".mod ._Dnh ._NRl") %>% html_text()
  srch.elm <- total_css %>% html_nodes("._Wtj ._Dnh") %>% html_text()
  
  #sub.elm.ttl <- sub.elm.ttl[sub.elm.ttl!="維基百科"]
  #sub.elm.ctn <- sub.elm.ctn %>% converter(S2T)[.]
  #sub.elm.ctn <- sub.elm.ctn[!grepl("更多內容", sub.elm.ctn)]
  #sub.elm.ttl <- sub.elm.ttl[!grepl("更多內容", sub.elm.ttl)]
  sub.elm <- sub.elm[grepl("：", sub.elm)]
  sub.elm.ttl <- str_split_fixed(sub.elm, "：", 2)[,1]
  sub.elm.ctn <- str_split_fixed(sub.elm, "：", 2)[,2]
  
  stopifnot(length(sub.elm.ttl)==length(sub.elm.ctn))
  
  tmp <- data.frame("title" = chinaHS$校名[i] %>% iconv(from = "BIG-5", to = "UTF-8"),
                    "subtitle" = c("name",
                                   "content",
                                   sub.elm.ttl,
                                   "highly correlated"
                    ), 
                    "content" = c(title[1],
                                  cnt.elm[1],
                                  sub.elm.ctn,
                                  srch.elm[-1] %>% paste(., collapse = "、")
                    ), stringsAsFactors = FALSE)
  rtnDF <- rbind(rtnDF, tmp)
  cat("\r", i, ". ", title, " ==> ", format(round(i/nrow(chinaHS)*100, 2), nsmall = 2), "%", rep(" ", 20))
  Sys.sleep(runif(1, 9, 14))
}
rtnDF$subtitle <- rtnDF$subtitle %>% converter(S2T)[.]
rtnDF$subtitle <- str_trim(rtnDF$subtitle)
rtnDF$content <- rtnDF$content %>% converter(S2T)[.]
rtnDF$content <- str_trim(rtnDF$content)

saveRDS(rtnDF, "output/HSgoogleInfo.rds")
write.csv(rtnDF, "output/HSgoogleInfo.csv", row.names = FALSE)
##### End #####