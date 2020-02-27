library(tidyverse)
library(broom)
library(text2vec)
library(tidytext)

GLOVE <- "~/Data/glove.6B.100d.txt"
# tidytext contains three sentiment lexicons. NRC contains binary indicators
#       for a large set of words for 10 sentiment dimensions:
#   anger anticipation disgust fear joy negative positive sadness surprise trust
sentiment <- tidytext::get_sentiments('nrc')

setwd("~/Projects/SPSP-TextAnalysis/TheoryDrivenTextAnalysis/")
meta <- readRDS("./data/tdta_clean_house_data.RDS")

# subset data (so I can run on baby chromebook)
# comment following line for complete code
meta <- meta[1:5000,]
# make house data tidy
tdta <- meta %>% 
    select(doc_num, text) %>% 
    unnest_tokens(word, text)

# use text2vec::itoken, create_vocabulary and prune_vocabulary to quickly 
# build vocabulary (max_size=20000)
it = itoken(list(tdta$word))
house_vocab = create_vocabulary(it)
house_vocab = prune_vocabulary(house_vocab, vocab_term_max=20000)
vocab <- unique(c(house_vocab$term, sentiment$word))
# remove words not in vocabulary
tdta <- tdta %>%
    filter(word %in% vocab)

# read in GloVe vectors (100-dim)
# (1) create header ["word", X1, ..., X100]
# (2) use read_table2 to read space-delimited file
# (3) immediately filter embedding object to our vocabulary 
cs <- c("word")
for(i in 1:100) {cs <- c(cs, paste(c("X", i), collapse=""))}
E <- read_table2(GLOVE, col_names = cs) %>%
    filter(word %in% vocab)

# compute average embeddings for (1) dictionary and (2) corpus (tidy)
# (1) dictionary: join sentiment tibble (sentiment = [anger, sad,...],
#                                        word = [...]) to embedding tibble, 
#                 group_by and compute mean
# (2) repeat for corpus: tdta is tidy (doc_num, word), join at word
sentiment_embed <- sentiment %>%
    left_join(E, by = "word") %>%
    group_by(sentiment) %>%
    summarise_at(vars(-word), list(mean=~mean(., na.rm=TRUE)))
corpus_embed <- tdta %>%
    left_join(E, by='word') %>%
    group_by(doc_num) %>%
    summarise_at(vars(-word), list(mean=~mean(., na.rm=TRUE)))

# compute similarities use text2vec::sim2
#
doc_nums <- corpus_embed$doc_num
sentiment_idx <- sentiment_embed$sentiment
sentiment_sims <- sim2(as.matrix(corpus_embed %>% select(-doc_num)),
                       as.matrix(sentiment_embed %>% select(-sentiment)),
                       method = 'cosine', norm = 'l2')

# make similarity matrix into tibble                
sentiment_sims <- as.data.frame(sentiment_sims)
colnames(sentiment_sims) <- sentiment_idx 
rownames(sentiment_sims) <- doc_nums
sentiment_sims <- sentiment_sims %>%
    rownames_to_column(var="doc_num") %>%
    as_tibble()
sentiment_sims
