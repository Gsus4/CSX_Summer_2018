#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tm)
library(tmcn)
library(Matrix)
library(wordcloud)
library(bitops)
library(httr)
library(RCurl)
library(XML)
library(NLP)
library(jiebaRD)
library(jiebaR)
library(devtools)
library(scales)
library(grid)
library(ggbiplot)
library(factoextra)
library(shiny)
library(shinythemes)

# 資料爬取
d.corpus <- Corpus( DirSource("./data/poem", encoding = "UTF-8"))
d.corpus <- tm_map(d.corpus, removePunctuation) #移除標點符號
d.corpus <- tm_map(d.corpus, removeNumbers) #移除數字
#d.corpus <- tm_map(d.corpus,stripWhitespace) #消除空格
#d.corpus <- tm_map(d.corpus, function(word) { # 移除大小寫
#  gsub("[A-Za-z0-9]", "", word)
#})

#進行斷詞
mixseg = worker()
#斷詞
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

#計數
count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

##建立文本矩陣
n = length(seg)
TDM = tokens[[1]]
colNames <- c('白居易','杜甫','李白','孟浩然','王維')
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}

TDM[is.na(TDM)] <- 0 #將NA填0

library(knitr)
kable(head(TDM))

kable(tail(TDM))

## TF-IDF
tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum) #直向相加計算總數

idfCal <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)

doc.tfidf <- TDM

tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX

#刪除不重要(td-idf=0)的字
stopLine = rowSums(doc.tfidf[,2:(n+1)])
delID = which(stopLine == 0)

kable(head(doc.tfidf[delID,1]))

kable(tail(doc.tfidf[delID,1]))

# final result
TDM = TDM[-delID,]
doc.tfidf = doc.tfidf[-delID,]
TopWords = data.frame()
for( id in c(1:n) )
{
  dayMax = order(doc.tfidf[,id+1], decreasing = TRUE)
  showResult = t(as.data.frame(doc.tfidf[dayMax[1:10],1])) #取前10
  TopWords = rbind(TopWords, showResult)
}
rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
TopWords = droplevels(TopWords)
kable(TopWords)
query.tfidf <- function(q){
  q.tfidf <- doc.tfidf[doc.tfidf$d==q, ]
  return (q.tfidf)
}

query=c("美酒", "夕陽", "故人", "老翁", "西風", "琵琶")
result = query.tfidf(query[1])
for ( id in c(2:length(query)) )
{
  q.tfidf = query.tfidf(query[id])  
  result = rbind(result, q.tfidf)
}
cos <- function(x, y){
  return (x %*% y / sqrt(x %*% x * y %*% y))[1, 1]
}
# compare with 李白
docs.cos.sim <- apply(doc.tfidf[,2:6], 2, cos, y = doc.tfidf[,4])
docs.cos.sim

# 製作文字雲
library(wordcloud)
rownames(doc.tfidf) = doc.tfidf$d
f <- sort(rowSums(doc.tfidf[,2:6]), decreasing = T)
docs.df <- data.frame(
  word = names(f),
  freq = f
)

# PCA
t = t(doc.tfidf) #轉置
t = t[-1,] #刪除第一列
t = apply(t, 2, as.numeric) #由'character'轉成'numeric'

# PCA
pcat = prcomp(t)
g <- ggbiplot(pcat, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)

# Kmeans
docs.ind <- get_pca_ind(pcat)
ind.coord2 <- docs.ind$coord[, 1:2]
wss <- c()
for (i in 1:4) { wss[i] <- kmeans(ind.coord2, i)$tot.withinss }
plot(wss, type = "b")
kmeansData = pcat$x[,1:2]
rownames(kmeansData) <- c('白居易','杜甫','李白','孟浩然','王維')

##=====================================================================================
# Define UI for application that draws a histogram
ui <- navbarPage(
   
   # Application title
   "唐詩作者分析",
   theme = shinythemes::shinytheme("cyborg"),
   # Sidebar with a slider input for number of bins 
   tabPanel(
     "文字雲",
     tags$h1("畫出文字雲"),
     
     sidebarLayout(
       sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 100,
                     value = 70)
       ),
       # Show a plot of the generated distribution
       mainPanel(
         plotOutput("WordCloud")
       )
     )
    ),
   tabPanel(
     "PCA",
       # Show a plot of the generated distribution
       mainPanel(
         plotOutput("PCA.plot1"),
         plotOutput("PCA.plot2"),
         plotOutput("PCA.plot3")
       )
     ),
   tabPanel(
     "K-means",
     tags$h1("K-means分群"),
     sidebarLayout(
       sidebarPanel(
         numericInput("k",
                     "Number of k:",
                     min = 1,
                     max = nrow(kmeansData) - 1,
                     value = 3)
       ),
       # Show a plot of the generated distribution
       mainPanel(
         plotOutput("Kmeans.plot")
       )
     )
   )
   
)
   
   
##==========================================================================================
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$WordCloud <- renderPlot({
      wordcloud(docs.df$word, docs.df$freq, scale=c(5,0.1),max.words=input$bins,
                random.order=TRUE, random.color=FALSE, 
                rot.per=.1, colors=brewer.pal(8, "Dark2"),
                ordered.colors=FALSE,use.r.layout=FALSE,
                fixed.asp=TRUE)
   })
   output$PCA.plot1 <- renderPlot({
      fviz_eig(pcat)
   })
   output$PCA.plot2 <- renderPlot({
     fviz_pca_ind(pcat, geom= c("point","text","arrow"), col.ind = "cos2")
   })
   output$PCA.plot3 <- renderPlot({
     fviz_pca_var(pcat, col.var = "contrib")
   })
   output$Kmeans.plot <- renderPlot({
     
     cl <- kmeans(kmeansData, input$k)
     plot(kmeansData, col = cl$cluster)
     text(kmeansData, labels = rownames(kmeansData), col = cl$cluster)
     points(cl$centers, col = 1:2, pch = 8, cex = 1) # pch 點樣式, cex 點大小
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

