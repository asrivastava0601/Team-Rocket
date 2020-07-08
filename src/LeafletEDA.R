
#Libraries for EDA
install.packages('tm')
install.packages('wordcloud')
install.packages("ggplot2")
library(ggplot2)
library(tm)
library(wordcloud)
library(leaflet)
library('dplyr')

#setwd("D:/temp")

# Graph 1
final_df <- read.csv("./business.csv")
pal2 <- brewer.pal(8,"Dark2")
wordcloud(final_df$Sub.category,scale=c(4,1), min.freq = 18,random.order=FALSE, random.color= TRUE,rot.per=.35, colors=pal2)


#-------For leaflet

dfFOrLeaflet <- readRDS("./shinyAppDFv7.rds")

dfForGraph <- dfFOrLeaflet %>%
  group_by(Latitude,Longitude,City,Price_range) %>%
  summarise(count=n())%>% filter(count >20) 

# Graph 3
#-----With legend
#Define color palette
pal <- colorNumeric(
  palette = 'RdBu',
  domain = dfForGraph$count
)
# Create Map
map<-leaflet(dfForGraph) %>% addTiles() %>% setView(lng = -111.92556, 
                                               lat = 33.56518, zoom = 12) %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1, color = ~pal(count),
             radius = ~sqrt(count) * 30, popup = ~City 
  )
#Add legend
map %>%
  addLegend("bottomright", pal = pal, values = ~dfForGraph$count,
                     title = "Count of Restaurants",
                     opacity = 1)
  




