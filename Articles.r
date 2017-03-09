library(dplyr)
library(jiebaR)
library(wordcloud)

load(file = "df_a.RData")
load(file = "df_info2.RData")
df <- left_join(df_info2, df_a, by = "url")

# 把公告拿掉
df <- df[-grep("公告", unique(df_info$title)),]

# 看標題的關鍵字
cutter <- worker(stop_word = "stopword.txt")
title_seg <- segment(df$title, cutter)
title_seg_freq <- arrange(freq(title_seg), desc(freq))
title_seg_freq[1:50,]

# 看內文的關鍵字
cutter = worker(stop_word = "stopword.txt")
seg <- segment(df$article, cutter)
freq <- arrange(freq(seg), desc(freq))
freq <- freq[-1,]
freq[1:50,]
wordcloud(freq$char, freq$freq, min.freq=4054, colors = rainbow(50))

# 2-gram
cutter <- worker(bylines = T, stop_word = "stopword.txt")
article_words <- sapply(df$article, function(x)
    segment(x, cutter)
)

library(text2vec)
seg_token <- itoken(article_words)
vocab <- create_vocabulary(seg_token, ngram = c(2,2))
twogram <- as.data.frame(vocab$vocab) %>%
		   arrange(twogram, desc(terms_counts))

# 利用2-gram搜尋關鍵詞的關聯詞
# 環島、花蓮、宜蘭、台北、台南、公園、步道、園區、老街、溫泉、火車
kw <- twogram[grep("環島", twogram$terms),]
kw$n <- c(1:length(kw$terms))
head(kw, 50)
kw <- kw[-c(2,4,5,7,8,10,12,13,14,17,20,21,25,26,27,28,29,32,34,38,39,40,42,44,46,47),]
#手動清理 
wordcloud(kw$terms, kw$terms_counts, min.freq=30, 
          colors = brewer.pal(8,"Dark2"))

