# please download the dataset from https://www.kaggle.com/rtatman/blog-authorship-corpus

# to get current script folder
dir <- dirname(rstudioapi::getSourceEditorContext()$path)

blogtext <- read.csv(paste0(dir,"/blogtext.csv"))


blogtext <- read.csv('./blogtext.csv')
library(dplyr)
install.packages('tidyverse')

library(tidyverse)
library(tidytext)
bloggers <- blogtext %>% count(id) %>% arrange(desc(n))


topBlogText <- blogtext %>%
  filter(id %in% bloggers$id[c(1,2,3)])%>%
  select(id,text) %>%
  rowid_to_column(var = "sample")

metadata <- count(topBlogText,id)


# note: ungroup is a mandatory step to prevent errors with unnest_tokens
set.seed(1) 
train <- topBlogText %>%
  group_by(id)%>%
  slice_sample(prop = 0.8)%>%
  ungroup()
test <- topBlogText%>%
  filter(!(sample %in% train$sample))

sentences <- train%>%
  unnest_sentences(text,text)


trigrams <- sentences%>%
  mutate(text = paste("s_", "s_",text,"_s","_s"))%>%
  unnest_tokens(output = trigrams,input = text, token = "ngrams", n = 3, drop = F)
format(object.size(trigrams), units = "Mb")

bigrams <- sentences%>%
  mutate(text = paste("s_", "s_",text,"_s","_s"))%>%
  unnest_tokens(output = bigrams,input = text, token = "ngrams", n = 2, drop = F)
format(object.size(bigrams), units = "Mb")

unigrams<- sentences%>%
  unnest_tokens(output = unigrams,input = text, drop = F)
format(object.size(unigrams), units = "Mb")


#find V

unigrams.counts <- unigrams%>%
  group_by(id)%>%
  count(unigrams)%>%
  ungroup()

V <- count(unigrams.counts,id, name = "V")

bigrams.counts <- bigrams%>%
  group_by(id)%>%
  count(bigrams)%>%
  ungroup()

trigrams.counts <- trigrams%>%
  group_by(id)%>%
  count(trigrams)%>%
  ungroup()

#############   testing phase  #########
set.seed(1) 

t1 <- test%>%
  group_by(id)%>%
  sample_n(1)%>%
  ungroup()

testText <- tibble(text = t1$text[1])
testText <- testText%>%
  unnest_sentences(text,text)%>%
  rowid_to_column(var = "sentence")

testText <- testText%>%
  mutate(text = paste("s_", "s_",text,"_s","_s"))

aux <- expand_grid(sentence = testText$sentence,id = metadata$id)
testText <- dplyr::left_join(testText, aux, by = c("sentence"))

testText <- testText%>%
  unnest_tokens(output = trigrams,input = text
                , token = "ngrams", n = 3, drop = F)

# https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
# https://regenerativetoday.com/a-complete-beginners-guide-to-regular-expressions-in-r/
testText <- testText %>%
  mutate(bigrams = sub(" \\S+$.*","",trigrams))

## Laplace Smoothing

testText <- left_join(testText,trigrams.counts,by = c("trigrams", "id"))
testText <- left_join(testText,bigrams.counts,by = c("bigrams", "id"))
testText <- left_join(testText,V,by = c("id"))
testText[is.na(testText)] <- 0
#View(testText)

testText <- testText %>%
  mutate(p_laplace = (n.x+1)/(n.y+V))

testText <- testText %>%
  mutate(P.log = log(p_laplace))

outcome <- testText %>%
  group_by(id, sentence)%>%
  summarise(p_sentence = sum(P.log))%>%
  arrange(desc(p_sentence))%>%
  group_by(sentence)%>%
  ungroup()

outcome
outcome %>% 
  group_by(id)%>%
  summarise(joint_p = sum(p_sentence))%>%
  arrange(desc(joint_p))


##  Add-k smoothing

k=0.001
testText <- testText %>%
  mutate(p_Ksmooth = (n.x+k)/(n.y+k*V))

testText <- testText %>%
  mutate(P.logK = log(p_Ksmooth))

outcome <- testText %>%
  group_by(id, sentence)%>%
  summarise(p_sentence = sum(P.logK))%>%
  arrange(desc(p_sentence))

outcome
outcome %>% 
  group_by(id)%>%
  summarise(joint_p = sum(p_sentence))%>%
  arrange(desc(joint_p))

##

