library(data.table)
library(stringi)
library(dplyr)
library(tidytext)
library(tidyr)
library(text2vec)
path<-getwd()

posts<-fread("input_data/posts.txt",sep = "|",fill=T,header=F,encoding = "UTF-8")
comments<-fread("input_data/comments_mn.txt",sep="|",fill=T,header=F,encoding = "UTF-8")

get_text_c<-function(x){
  tmp<-stri_split(x,fixed="|") %>% unlist()
  tmp[5]
}

get_text_p<-function(x){
  tmp<-stri_split(x,fixed="|") %>% unlist()
  tmp[3]
}

txt1<-lapply(comments$V1,get_text_c)
txt2<-lapply(posts$V1,get_text_p)

txt1<-unlist(txt1)
txt2<-unlist(txt2)
txt<-c(txt1,txt2)

#--- split into sentences
text_data<-data.frame(txt=txt,stringsAsFactors = F)
sentences<-text_data %>%
  unnest_tokens(output=sentence,input=txt,token="sentences",to_lower=F,collapse=F)

#--- remove links
sentences_c1 <- sentences %>%
  mutate(sentence_clean=stri_replace_all(stri_replace_all(sentence,regex="http[[:graph:]]*"," "),regex="[[:space:]]+"," "))

sentences_c2 <- sentences_c1 %>%
  mutate(
    txt2=paste0("xxxstart1xxx ",sentence_clean, " xxxendxxx"),
    txt3=paste0("xxxstart1xxx xxxstart2xxx ",sentence_clean," xxxendxxx")) %>%
  mutate(txt2=stri_replace_all(txt2,regex="[^[:alnum:]]",replacement = " "))

bigrams <- sentences_c2 %>%
  unnest_tokens(input=txt2,output=ngram,token="ngrams",n=2,to_lower=T)


term<-function(x,w){
  y<-x %>% stri_split(regex="[[:space:]]") %>% unlist()
  y[w]
}

f_term<-function(x,w){
  lapply(x,term,w=w) %>% unlist()
}


bigrams_2 <- bigrams %>% 
  separate(col=ngram,into=c("term1","term2"),sep=" ",remove=F) %>%
  select(ngram,term1,term2) 

lhs<-bigrams_2 %>%
  group_by(term1) %>%
  summarize(lhs=n())

rhs<-bigrams_2 %>%
  group_by(term1,term2) %>%
  summarize(rhs=n())

probs<-lhs %>%
  inner_join(rhs,by="term1") %>%
  mutate(prob=rhs/lhs) %>%
  arrange(lhs,desc(prob))

saveRDS(probs,"probs_2.RDS")
#-----------------------------------------------------------
#---- generate word vectors
#----------------------------------------------------------
it_sentences <- itoken(sentences_c2$sentence,
                             tokenizer = space_tokenizer,
                             preprocessor = tolower,
                             progressbar = T)


vocab <- create_vocabulary(
  it_sentences)

#vocab_cut<-vocab_diagnosis[stri_length(vocab_diagnosis$term)>=3,]
vocab_cut<-vocab
# vocab <- prune_vocabulary(vocab_diagnosis, term_count_min = 5L)
vectorizer <- vocab_vectorizer(vocab_cut)
# use window of 5 for context words
tcm <- create_tcm(it_sentences, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(
  50, 
  vocab_cut, 
  x_max=10, 
  learning_rate = 0.05,
  alpha = 0.75, 
  lambda = 0.0, 
  shuffle = FALSE, 
  initial = NULL)

word_vectors_main <- glove$fit_transform(x=tcm, n_iter = 50L, convergence_tol = -1, n_check_convergence = 1L)

word_vectors_context <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_context)

schizo<-word_vectors["kaczyński", , drop = FALSE]

cos_sim = sim2(x = word_vectors, y = schizo, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 20)

word_vectors.df <- data.frame(word_vectors)
word_vectors.df$term<-rownames(word_vectors)

test_sentence<-"Jarosław Kaczyński chuj"
test.df <- data.frame(test_sentence=c(test_sentence),stringsAsFactors = F)
test_terms <- test.df %>% 
  unnest_tokens(input=test_sentence,output=term,token="words",to_lower = T)

test_vec <- test_terms %>%
  inner_join(word_vectors.df,by="term") %>%
  select(-term) %>%
  summarise_all(mean)

test_vec<-as.matrix(test_vec)

cos_sim = sim2(x = word_vectors, y = test_vec, method = "cosine", norm = "l2")
syns<-names(head(sort(cos_sim[,1], decreasing = TRUE), 20))

model_terms <- unique(probs$term1)

potential_starters<-intersect(model_terms,syns)

starter<-{
  if(length(potential_starters)==0) "xxxstart1xxx"
  else potential_starters[1]
}

generate_one_sentence(start_term = starter)


