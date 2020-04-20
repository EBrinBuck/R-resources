library(quanteda)
library(readtext)
library(dplyr)
library(ggplot2)

###tutorials for help: 
###https://quanteda.io/articles/pkgdown/quickstart.html
###https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/advancing-text-mining/
###https://towardsdatascience.com/a-light-introduction-to-text-analysis-in-r-ea291a9865a8


##read in csv and convert to corpus format
C1 <- readtext("PlatteBasinEventDatabase_4.6.20.csv", text_field = "Text")
C1c<-corpus(C1)
##view first 5 summary
summary(corpus(C1), 5)

##create tokens
C1token <- tokens(C1c,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = FALSE,
    split_hyphens = FALSE)



##convert to data frame, stem words, and remove common words
C1dfm <- dfm(C1c, remove= stopwords("english"), remove_numbers=TRUE, remove_symbols=TRUE,
             remove_punct = TRUE)
C1dfm <- dfm(C1dfm,
             remove=list("Kearney","central","district","nebraska","Gibbon", "get", "Grand","Island","Omaha","Lincoln","Hastings", "holdrege"))


###most commonly occuring words
topfeatures(C1dfm, 20)

tstat_freq <- textstat_frequency(C1dfm, n = 10, groups = "Decade")
?reorder
tstat_freq
tstat_freq$group<-recode(tstat_freq$group, D1="2005-2009",D2="2010-2014", D3="2015-2019")
ggplot(tstat_freq, aes(x = reorder(feature, frequency), y = rank, color=rank)) +
  facet_wrap(~group)+ geom_point(size=3) + coord_flip() + 
  labs(x = NULL, y = "Ranked by frequency") + ggtitle("Top 10 Words")+
  theme_minimal()+ scale_y_continuous(breaks=c(1,5,10))+theme(text = element_text(size=20))+theme(legend.position="none")

###wordcloud
set.seed(100)
textplot_wordcloud(C1dfm, min_count = 10, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(10,"BrBG"))
###frequency plot
features_C1 <- textstat_frequency(C1dfm, n = 100)
# Sort by reverse frequency order
features_C1$feature <- with(features_C1, reorder(feature, -frequency))
ggplot(features_C1, aes(x = feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##defining a dictionary 
my_dict <- dictionary(list(wildlife = c("crane", "cranes", "bird", "wildlife", "habitat"),
                           waterquantity = c("drought", "flood", "quantity")))
c1dic <- dfm(C1dfm, dictionary = my_dict)

