library(tidyverse)
library(broom)
library(text2vec)
library(tidytext)
#library(knitr)
#library(ggfortify)

# function definitions --------------------------------------------------------------------------

# input .txt file, exports list of list of values and character vector of names (words)
proc_pretrained_vec <- function(p_vec, vocab, size_vec=100) {
    # initialize space for values and the names of each word in vocab
    vals <- vector(mode = "list", length(vocab))
    names <- vector(mode = "character", length(vocab))
    added <- 0
    # loop through to gather values and names of each word
    for(i in 1:length(p_vec)) {
        if(i %% 1000 == 0) {
            print(i)
        }
        if(added == length(vocab)) {
            break;  # early termination
        }
        this_vec <- p_vec[i]
        this_vec_unlisted <- unlist(strsplit(this_vec, " "))
        this_vec_name <- this_vec_unlisted[1]
        index <- match(this_vec_name, vocab)

        if(this_vec_name %in% vocab) {
            this_vec_values <- as.numeric(this_vec_unlisted[-1]) 
            vals[[index]] <- this_vec_values
            added <- added + 1
            names[[index]] <- this_vec_name
        }
    }
    
    v <- rep(0.0, size_vec)
    for(i in 1:length(names)) {
        word <- vocab[[i]]
        embed_word <- names[[i]]
        if(word != embed_word) {
            vals[[i]] <- v  # zero vector
            names[[i]] <- word
        }
    }
    glove <- data.frame(vals)
    names(glove) <- names
    glove <- as.data.frame(t(glove))
    return(glove)
}

# tidy_doc is a tidytext formatted tibble with (word + doc) indices. 
# dictionary is a tibble with a "sentiment" column; all other columns are glove means
ddr <- function(tidy_doc, dictionary) {
    this_wv_mat <- matrix(this_wv, ncol=length(this_wv), nrow=1)
    all_wvs_mat <- as.matrix(all_wvs)
    cos_sim = sim2(x=all_wvs_mat, y=this_wv_mat, method="cosine", norm="l2")
}

sentiment <- tidytext::get_sentiments('nrc')
glove_sentiment <- proc_pretrained_vec(g6b_100, unique(sentiment$word))
sentiment_embed <- sentiment %>%
    left_join(glove_sentiment %>%
                  rownames_to_column(var="word") %>%
                  as_tibble(), 
              by = "word") %>%
    group_by(sentiment) %>%
    summarise_at(vars(-word), list(mean=~mean(., na.rm=TRUE)))

g6b_100 <- scan(file = "~/Data/glove.6B.100d.txt", what="", sep="\n")
m <- as.matrix(sentiment_embed[2,2:101])
find_sim_wvs(m, glove_sentiment, top_n_res = 200)
sentiment_embed
