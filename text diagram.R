#modified from http://github.com/halhen/viz-pub/blob/master/sentence-drawings/code.nb.html
#packages
library(tidyverse)
library(tidytext)
library(stringr)
library(colorRamps) 
library(colorspace)
library(viridis)
library(wordcloud)
#load text and create plot
text<-readLines("Hobbit.txt")
df<-tibble(text) %>% 
  unnest_tokens(sentence, text, token = 'sentences') %>% 
  mutate(sentence.id = row_number()) %>% 
  unnest_tokens(word, sentence, drop = FALSE) %>% 
  left_join(get_sentiments('afinn'), by = 'word') %>% 
  group_by(sentence.id) %>% 
  summarise(sentence =first(sentence), 
            n.words = n(),
            sentiment = coalesce(mean(score, na.rm = TRUE), 0)) %>% 
  arrange(sentence.id)

plot.out<-df %>% 
  mutate(d.x = round(n.words*cos(2*pi*sentence.id/4)), 
         d.y = round(n.words*sin(2*pi*sentence.id/4))) %>% 
  mutate(end.x = cumsum(d.x),
         end.y = cumsum(d.y),
         start.x = lag(end.x, default = 0),
         start.y = lag(end.y, default = 0)) %>% 
  {ggplot(., aes(
             start.x, start.y, xend = end.x, yend= end.y, color= sentiment))  +
             geom_point(data = filter(., sentence.id == min(sentence.id)))+
             geom_point(data = filter(., sentence.id==max(sentence.id)), 
                        aes(end.x, end.y))+
             geom_segment()+
             coord_equal()+
             theme_void()+
             theme(legend.position="none")+
      scale_colour_gradient(low = "red", high = "yellow")}
plot.out
ggsave(filename = "Hobbit6.png", width = 16, height = 10, units = "in", bg = "transparent")

png(filename="Hobbit.png", width=11, height=17, units=in, res=300, bg = "transparent")        
