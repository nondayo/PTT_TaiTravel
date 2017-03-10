library(xml2)
library(dplyr)
library(httr)

#先在頁面層抓取文章資訊title date author
big_table <- lapply(c(1:1116),function(num){
        page <- paste0("https://www.ptt.cc/bbs/Tai-travel/index", num, ".html", sep="") %>%
                as.character()    
        urls_num <- GET(page) %>%
                    content() %>%
                    xml_find_all(xpath = "//div[@class='title']/a/@href") %>%                    
                    xml_text()    
        page_response <- GET(page)    
        df <- data.frame(id = urls_num, stringsAsFactors = FALSE)
        df$title <- page_response %>%
                    content() %>%
                    xml_find_all(xpath = "//div[@class='title']/a") %>%   
                    xml_text() 
        df$date <- page_response %>%
                   content() %>%
                   xml_find_all(xpath = "//div[@class='date']") %>%                    
                   xml_text() 
        df$author <- page_response %>%
                     content() %>%
                     xml_find_all(xpath = "//div[@class='author']") %>%                    
                     xml_text()    
        df$url <- paste0("https://www.ptt.cc",urls_num, sep="") %>%
                  as.character()
    Sys.sleep(5)
    df
})
df_info <- Reduce(x = big_table, f = rbind) 
save(df_info, file = "df_info.RData")

# 拿所有連結
article_urls <- lapply(c(1:1116),function(num){
    page <- paste0("https://www.ptt.cc/bbs/Tai-travel/index", num, ".html", sep="") %>%
            as.character()
    urls_num <- GET(page) %>%
                content() %>%
                xml_find_all(xpath = "//div[@class='title']/a/@href") %>%                    
                xml_text() 
    urls <- paste0("https://www.ptt.cc",urls_num, sep="") %>%
            as.character()
    Sys.sleep(3)    
    data.frame(urls, stringsAsFactors = F)
})
article_urls.df = Reduce(f=rbind, article_urls)
save(article_urls.df, file = "article_urls_df.RData")

# 爬取內文
urls <- article_urls.df$urls
table <- lapply(c(1:22320), function(url_idx) {
    url <- urls[url_idx]
    response <- GET(url)
    df <- data.frame(url, stringsAsFactors = FALSE)
    if(status_code(response)==200){    
        df$status = 200;    
        df$article = tryCatch({
          a <- content(response, as = 'text', encoding = 'UTF-8')
          non_char_removed = gsub(pattern = '\xef\xbf\xbe', '', a)
          article_html = read_html(non_char_removed)
          article_div = xml_find_all(x = article_html, xpath = "/html/body/div[4]/div[1]")
          article_txt = xml_text(article_div)
        }, error = function (err) {
          return (paste0('Error: ', as.character(err)))
        })
        Sys.sleep(3)
        df
    }
    else {
        df$status = status_code(response)
        df$article = ''
        Sys.sleep(3)
        df
    }
})

df_a = Reduce(f = rbind, x = table)
save(df_a, file = "df_a.RData")


