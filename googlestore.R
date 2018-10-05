library(tidyverse)

gstore <- read_csv("data/googleplaystore.csv")

gstore %>% group_by(App) %>%
  summarize(Reviews=mean(Reviews), Rating=mean(Rating)) %>%
  arrange(desc(Reviews)) %>% select(App, Reviews,Rating)



#data cleaning
gstore<-gstore %>% 
  filter(!is.na(Category),Type !="NaN",!is.nan(Rating))




#reviews by installs
ggplot(gstore) + geom_boxplot(aes(x=Installs,y=Reviews))+
  coord_flip()

#boxplot grouped
gstore %>% group_by(App,Installs) %>% summarize(Reviews=mean(Reviews)) %>%
  ggplot+geom_boxplot(aes(Installs,Reviews))+coord_flip()

#notice the scales are not equal
gstore %>% group_by(App,Installs) %>% summarize(Reviews=mean(Reviews)) %>%
  ggplot+geom_boxplot(aes(Installs,log(Reviews)))+coord_flip()

#now we need to order
gstore %>% group_by(App,Installs) %>% summarize(Reviews=mean(Reviews)) %>%
  ggplot+geom_boxplot(aes(reorder(Installs,Reviews,median),log(Reviews)))+
  coord_flip()


##plot ratings against reviews colored by category
gstore%>% group_by(App,Category)%>%summarize(Reviews=mean(Reviews),Rating=mean(Rating))%>%
  ggplot()+geom_point(aes(y=Rating,x=Reviews,color=Category))

                      