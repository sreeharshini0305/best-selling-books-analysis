#installing libraries
library(reshape2)
library(tidyverse)
library(ggthemes)
library(ggplot2)


#setting directory
getwd()
setwd("F:/1-2/SMDA")
getwd()

#reading the data
books <- read.csv("bestsellers with categories.csv", stringsAsFactors = FALSE)

#dimensions of the data
dim(books)

head(books)
tail(books)

#structure of the data
str(books)
#summary of the data
summary(books)
summary(books$User.Rating)
summary(books$Reviews)
summary(books$Year)

#boxplot
boxplot(books$User.Rating)
boxplot(books$Reviews)
boxplot(books$Price)
boxplot(books$Year)

#piechart
options(repr.plot.width = 16, repr.plot.height = 8)

books %>%
  select(Name,Genre) %>%
  distinct(Name,Genre) %>%
  group_by(Genre) %>%
  summarise(Count=n(),.groups = "drop")%>% 
  mutate(Percent=prop.table(Count) * 100) %>%
  ggplot(aes(x="", y=Percent,fill = Genre))+
  geom_bar(stat="identity",width = 1)+
  coord_polar("y",start = pi / 3)+
  
  scale_fill_manual(values=c('#D55E00','#56B4E9'))+
  geom_label(aes(label = paste0(round(Percent,2), "%")), position = position_stack(vjust = 0.5),
             colour = "black",  fontface = "italic")+
  theme(plot.title = element_text(hjust=0.5)) + 
  labs(title="Percentage of Genre")


#bargraph
options(repr.plot.width = 16, repr.plot.height = 8)

books %>%
  mutate(Year = as.character(Year))%>%
  group_by(Genre,Year) %>%
  summarise(Count=n(),.groups = "drop")%>% 
  ggplot(aes(x=Year, y=Count,fill = Genre))+
  geom_bar(stat="identity", position=position_dodge(0.8))+
 
  scale_fill_manual(values=c('#D55E00','#56B4E9'))+
  theme(plot.title = element_text(hjust=0.5)) + 
  labs(x="Year", y="Number of Books", title="Yearly Number of Books by Genre")+
  theme(legend.position="right",axis.text.x = element_text(vjust = 0.5))


#another bargraph
book_total<-books %>%
  mutate(Year = as.character(Year))%>%
  group_by(Year) %>%
  summarise(Total_Avg_Price= mean(Price),.groups = "drop") 

book_nonfic<-books %>%
  filter(Genre=="Non Fiction")%>%
  mutate(Year = as.character(Year))%>%
  group_by(Year) %>%
  summarise(Non_Ficition_Avg_Price= mean(Price),.groups = "drop")

book_fic<-books %>%
  filter(Genre=="Fiction")%>%
  mutate(Year = as.character(Year))%>%
  group_by(Year) %>%
  summarise(Ficition_Avg_Price= mean(Price),.groups = "drop") 

merge_1<- merge(book_total, book_nonfic, by.x="Year", by.y="Year")
merge_2<- merge(merge_1, book_fic, by.x="Year", by.y="Year")
books_avg_price<- melt(merge_2, id.vars="Year")
books_avg_price<- books_avg_price %>% mutate_if(is.numeric, ~round(., 2))


options(repr.plot.width = 16, repr.plot.height = 8)

ggplot(data=books_avg_price,aes(x=Year, y=value,fill=variable))+
  geom_bar(stat="identity", position=position_dodge(0.8))+
  
  scale_fill_manual(values=c('#999999','#56B4E9','#D55E00'))+
  theme(plot.title = element_text(hjust=0.5)) + 
  labs(x="Year", y="Avg Price", title="Avg Price distribution by Year")

