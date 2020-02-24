setwd("/home/btkenned/Projects/SPSP-TextAnalysis/TheoryDrivenTextAnalysis/")
df <- readRDS("./data/tdta_clean_house_data.RDS")
head(df)

# notes:
# tidytext is not docterm matrix, so need to switch back and forth

library(tidyverse)
library(tidytext)
library(scales)

text_df <- tibble(text=df$text, speech=1:nrow(df),
                  state=df$State, speaker=df$speaker,
                  party=df$Party)

text_df <- text_df %>%
    unnest_tokens(sentence, text, token = "sentences")
text_df <- text_df %>%
    mutate(sentence_id = 1:nrow(text_df))

# not run
(uni_dist <- text_df %>%
    unnest_tokens(word, text) %>%
    count(word, sort=TRUE) %>%
    mutate(rank=row_number(),
           total=sum(n),
           'term frequency' = n / total) %>%
    ggplot(aes(rank, `term frequency`)) +
    geom_line(show.legend = FALSE) + 
    scale_x_log10() + 
    scale_y_log10() + 
    ggtitle("Unigram Distribution (Log Scale)"))
(bi_dist <- text_df %>%
        unnest_tokens(word, text, token = 'ngrams', n=2) %>%
        count(word, sort=TRUE) %>%
        mutate(rank=row_number(),
               total=sum(n),
               'term frequency' = n / total) %>%
        ggplot(aes(rank, `term frequency`)) +
        geom_line(show.legend = FALSE) + 
        scale_x_log10() + 
        scale_y_log10() + 
        ggtitle("Bigram Distribution (Log Scale)"))

text_df

cleaned <- text_df %>% 
    unnest_tokens(word, sentence) %>%
    anti_join(stop_words) %>%
    drop_na(word) %>%    
    mutate(word=str_extract(word, "[a-z']+")) %>%
    group_by(sentence_id) %>%
    summarize(sentence.clean = str_c(word, collapse = " ")) %>% 
    left_join(text_df, by=c("sentence_id")) %>%
    drop_na() %>%
    select(-sentence)

bigram <- cleaned %>%
    unnest_tokens(word, text.clean, token = "ngrams", n=2)
unigram <- cleaned %>%
    unnest_tokens(word, sentence.clean)
unigram

frequency <- unigram %>%
    filter(party != "Independent") %>%
    count(party, word) %>%
    filter(n > 50, n < 1000) %>%
    group_by(party) %>%
    mutate(proportion = n / sum(n)) %>%
    select(-n) %>%
    spread(party, proportion) %>%
    drop_na()
frequency

ggplot(frequency, aes(x=Democratic, y=Republican)) +
    geom_abline(color="gray40", lty=2) +
    geom_jitter(alpha = 0.1, size = .5, 
                width = 0.05, height=0.05) +
    geom_text(aes(label=word), size=3, check_overlap=TRUE, vjust=.15) +
    scale_x_log10(labels=percent_format()) + 
    scale_y_log10(labels=percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), low="darkslategray4", high="gray75") +
    theme(legend.position="none")
    
get_sentiments('nrc')
