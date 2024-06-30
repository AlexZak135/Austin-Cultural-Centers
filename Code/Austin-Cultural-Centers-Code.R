# Title: Austin Cultural Centers Analysis
# Author: Alexander Zakrzeski
# Date: June 30, 2024

# Part 1: Setup and Configuration

# Load to import, clean, and wrangle data
library(dplyr)
library(janitor)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

# Load to visualize data
library(ggplot2)
library(scales)

# Load for natural language processing
library(ldatuning)
library(textstem)
library(tidytext)
library(topicmodels)
library(vader)

# Define a function to standardize the theme of ggplot outputs
theme_custom <- function(margin_size, legend = FALSE) {  
  # Create an empty theme  
  empty <- theme_void() 
  
  # Add the various styling elements to the theme 
  custom <- empty + theme( 
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  
    panel.spacing.x = unit(2.75, "lines"), 
    panel.spacing.y = unit(1.25, "lines"), 
    text = element_text(family = "Roboto"),   
    plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                              size = 17.5, face = "bold"), 
    strip.text = element_text(margin = margin(0, 0, 10, 0), size = 15.5), 
    panel.grid.major.x = element_line(linetype = 3, linewidth = 0.3, 
                                      color = "#808080"), 
    axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 15.5), 
    axis.text.x = element_text(size = 14, color = "#000000"), 
    axis.text.y = element_text(margin = margin(0, margin_size, 0, 0), size = 14, 
                               color = "#000000", hjust = 1)
    )
  
  # Conditionally add a legend to the theme  
  if (legend) { 
    custom <- custom + theme( 
      legend.position = "top", 
      legend.key.size = unit(0.6, "cm"), 
      legend.text = element_text(size = 15.5), 
      legend.spacing.x = unit(0.25, "cm"),
      legend.margin = margin(0, 0, 7.5, 0) 
      )
  }
  
  # Return the custom theme 
  return(custom) 
}

# Part 2: Data Preprocessing

# Load the data from the CSV file
df_qual <- read_csv("Austin-Cultural-Centers-Qualitative-Data.csv") |>
  # Rename the columns and then drop certain columns
  clean_names() |>
  rename(prompt = survey_item,
         label = auditor_assigned_category) |> 
  select(-c(re_assigned_response, translated, original_language)) |>
  # Create a new column and modify values of existing columns 
  mutate(alias = case_when(
    facility == "African American Cultural and Heritage Facility" ~ "AACHF", 
    facility == "Asian American Resource Center" ~ "AARC", 
    facility == "George Washington Carver Museum" ~ "Carver Museum",
    str_detect(facility, "Mexican American Cultural Center") ~ "ESB-MACC" 
    ),  
    prompt = case_when( 
      str_detect(prompt, "facilities") ~ "1-Facilities",
      str_detect(prompt, "staff") ~ "2-Staff", 
      str_detect(prompt, "fees") ~ "3-Fees",
      str_detect(prompt, "programs") ~ "4-Programs" 
      ),  
    response = str_squish(response),
    label = str_to_lower(label)) |>
  # Filter based on the set conditions and reset the column order 
  arrange(prompt, facility, response) |>
  rowid_to_column("id") |>
  filter(label != "n/a" & !id %in% c(30, 41, 68, 76, 86, 89, 91, 94, 98, 102, 
                                     103, 110, 131, 137, 152, 159, 161, 162, 
                                     171, 178, 183, 197, 198, 210, 224, 228,
                                     237, 257, 272, 275, 276, 286, 292, 345, 
                                     351, 374, 380, 384, 391, 401, 409, 413, 
                                     418, 434, 435, 448, 455, 475, 484, 486, 
                                     488, 490, 493, 516, 536, 539, 598, 615,
                                     644, 648, 671, 678, 705, 715, 732, 739, 
                                     745, 776, 795, 830, 832, 839, 848, 862)) |>
  select(-id) |>
  mutate(prompt = str_sub(prompt, 3)) |>
  relocate(facility, alias, .after = prompt)

# Load the data from the CSV file
df_quant <- read_csv("Austin-Cultural-Centers-Quantitative-Data.csv") |>
  # Rename the columns and change the order of the columns 
  clean_names() |>
  rename(prompt = survey_item,
         carver_museum = gwc) |>
  relocate(aachf, aarc, carver_museum, .after = response) |>
  # Modify values in the columns and filter appropriately
  mutate(prompt = str_squish(str_replace_all(prompt, "\\.{3}|…", " ")),  
         across(aachf:esb_macc, ~ if_else(
           is.na(.x), "No Responses", paste0(as.numeric(.x) * 100, "%") 
           ))) |> 
  filter(str_detect(prompt, str_squish("increased my understanding or knowledge|
                                        reflected the center's mission|
                                        affordable for attending performances|
                                        affordable for attending programs|
                                        at this center are welcoming|
                                        at this center are knowledgeable|
                                        parking options for attending programs|
                                        rooms and spaces are clean"))) |>
  # Create an id column to differentiate the prompts
  mutate(prompt = factor(prompt, levels = unique(prompt))) |>
  group_by(prompt) |>
  mutate(id = cur_group_id()) |>
  ungroup() |>
  relocate(id)

# Generate n-grams (unigrams, bigrams, and trigrams) from the responses
ngrams <- map_df(1:3, function(number) {  
  # Tokenize responses into n-grams with specified size "number" 
  processed <- df_qual |>
    unnest_tokens(ngram, response, token = "ngrams", n = number) |>
    drop_na(ngram) 
  
  # Additional processing for unigrams 
  if (number == 1) {
    processed <- processed |>
      filter(!str_detect(ngram, "^[0-9]+$")) |>
      anti_join(stop_words, by = c("ngram" = "word")) |> 
      mutate(ngram = lemmatize_words(ngram))  
  }
  
  # Add a column to indicate whether it is a unigram, bigram, or trigram
  processed |>
    mutate(type = case_when(
      number == 1 ~ "unigram",
      number == 2 ~ "bigram", 
      number == 3 ~ "trigram" 
      )) |>
    relocate(ngram, label, .after = type) 
})

# Part 3: Term Frequencies

# Generate term frequencies for specific n-grams in responses to prompts
frequencies <- ngrams |> 
  filter((prompt == "Facilities" & ngram %in% c("auditorium", "clean", "dance",
                                                "expansion", "light", 
                                                "location", "more parking", 
                                                "stage")) | 
         (prompt == "Staff" & ngram %in% c("amaze", "knowledgeable", "pleasant",
                                           "professional", "respectful", 
                                           "very friendly", "very helpful", 
                                           "welcome")) |
         (prompt == "Fees" & ngram %in% c("affordable", "discount", "expensive",
                                          "income", "low", "reasonable", 
                                          "rental", "scholarship")) |
         (prompt == "Programs" & ngram %in% c("artist", "camp", "child", 
                                              "educational", "excellent", 
                                              "family", "history", 
                                              "theater"))) |>
  count(prompt, alias, ngram) |>
  mutate(prompt = factor(prompt, levels = c("Facilities", "Staff", "Fees", 
                                            "Programs")),
         alias = factor(alias, levels = c("ESB-MACC", "Carver Museum", "AARC", 
                                          "AACHF")),
         ngram = str_to_title(ngram))

# Create a faceted bar chart to display term frequencies
ggplot(frequencies, aes(x = reorder(ngram, n, FUN = sum), y = n, 
                        fill = alias)) +
  geom_col(width = 0.825, position = "stack") +
  geom_text(aes(label = after_stat(y), group = ngram),
            stat = "summary", fun = "sum",
            vjust = 0.25, hjust = -0.25, size = 5) + 
  geom_hline(yintercept = 0, linewidth = 1.1, color = "#000000") + 
  scale_y_continuous(breaks = pretty_breaks(4), 
                     labels = label_number(drop0trailing = TRUE)) + 
  scale_fill_manual(values = c("#c41230", "#ffac1c", "#5e9732", "#0078ae")) +
  labs(title = "Figure 1: Term Frequencies from Feedback on Cultural Centers",
       x = "", y = "Frequency") +
  guides(fill = guide_legend(title = "", reverse = TRUE)) +
  facet_wrap(~ prompt, scales = "free") +
  coord_flip() +
  theme_custom(margin_size = -15, legend = TRUE) 

# Part 4: TF-IDF

# Generate tf-idf for specific unigrams in responses to prompts
tfidf <- map_df(unique(ngrams$prompt), function(value) {
  # Process and compute tf-idf of unigrams  
  processed <- ngrams |>
    filter(prompt == value & type == "unigram") |>
    count(alias, ngram) |>
    filter(n > 2) |>
    bind_tf_idf(ngram, alias, n) |>
    select(-c(tf, idf)) 
  
  # Filter based on the set conditions for the "Facilities" prompt
  if (value == "Facilities") { 
    processed <- processed |>  
      filter((alias == "AACHF" & ngram %in% c("floor", "large", "studio", 
                                              "tiny")) |
             (alias == "Carver Museum" & ngram %in% c("dirty", "seat", "stage", 
                                                      "theater")))  
    # Filter based on the set conditions for the "Staff" prompt
  } else if (value == "Staff") {
    processed <- processed |> 
      filter((alias == "AARC" & ngram %in% c("age", "visitor")) |
             (alias == "Carver Museum" & ngram %in% c("accessible", "director", 
                                                      "helpful", 
                                                      "knowledgeable", 
                                                      "professional")) |
             (alias == "ESB-MACC" & ngram == "train")) 
    # Filter based on the set conditions for the "Fees" prompt 
  } else if (value == "Fees") { 
    processed <- processed |> 
      filter((alias == "AARC" & ngram %in% c("affordable", "free", 
                                             "reasonable")) |
             (alias == "Carver Museum" & ngram == "theater") | 
             (alias == "ESB-MACC" & ngram %in% c("affordable", "class", 
                                                 "expensive", "family", "low"))) 
    # Filter based on the set conditions for the "Programs" prompt 
  } else if (value == "Programs") {
    processed <- processed |> 
      filter((alias == "AARC" & ngram %in% c("diverse", "food", "informative", 
                                             "senior")) |
             (alias == "Carver Museum" & ngram %in% c("enjoy", "exhibit")) |
             (alias == "ESB-MACC" & ngram %in% c("artist", "family")))
  }
  
  # Perform additional processing steps, such as changing data types 
  processed |>
    mutate(prompt = factor(value, levels = c("Facilities", "Staff", "Fees", 
                                             "Programs")), 
           alias = factor(alias, levels = c("AACHF", "AARC", "Carver Museum", 
                                            "ESB-MACC")),
           ngram = reorder_within(str_to_title(ngram), tf_idf, prompt, 
                                  sep = "_"), 
           scaled_tf_idf = tf_idf * 100) |>
    relocate(prompt)
})

# Create a faceted bar chart to display scaled tf-idf scores 
ggplot(tfidf, aes(x = ngram, y = scaled_tf_idf, fill = alias)) +
  geom_col(width = 0.825) +
  geom_hline(yintercept = 0, linewidth = 1.1, color = "#000000") +
  scale_x_discrete(labels = function(x) str_replace_all(x, "_.*", "")) +
  scale_y_continuous(labels = label_number(drop0trailing = TRUE)) +
  scale_fill_manual(values = c("#0078ae", "#5e9732", "#ffac1c", "#c41230")) +
  labs(title = "Figure 2: TF-IDF Scores from Feedback on Cultural Centers",
       x = "", y = "Scaled TF-IDF Score (x100)") +
  guides(fill = guide_legend(title = "")) +
  facet_wrap(~ prompt, scales = "free") + 
  coord_flip() + 
  theme_custom(margin_size = -15, legend = TRUE) 

# Part 5: Sentiment Analysis

# Generate sum sentiment scores for the top positive or negative unigrams
sums <- map_df(unique(df_qual$prompt), function(value) { 
  # Filter for each unique prompt and tokenize the responses 
  processed <- df_qual |>
    filter(prompt == value) |>
    unnest_tokens(ngram, response, token = "ngrams", n = 1) |> 
    # Get sum sentiment scores of each unigram  
    inner_join(get_sentiments("afinn"), by = c("ngram" = "word")) |> 
    group_by(ngram) |>
    summarize(sum = sum(value)) |> 
    mutate(prompt = factor(value, levels = c("Facilities", "Staff", "Fees", 
                                             "Programs")),
           label = if_else( 
             sum > 0, "Positive", "Negative" 
             ) |>  
                   factor(levels = c("Positive", "Negative")),
           sum = if_else( 
             label == "Negative", sum * -1, sum 
             ),
           ngram = reorder_within(str_to_title(ngram), sum, prompt, 
                                  sep = "_")) |>
    # Select the top eight positive or negative unigrams 
    slice_max(sum, n = 8, with_ties = FALSE) |>
    relocate(prompt)
})

# Create a faceted bar chart to display sum sentiment scores 
ggplot(sums, aes(x = ngram, y = sum, fill = label)) +
  geom_col(width = 0.825) +
  geom_hline(yintercept = 0, linewidth = 1.1, color = "#000000") + 
  scale_x_discrete(labels = function(x) str_replace_all(x, "_.*", "")) +
  scale_fill_manual(values = c("#5e9732", "#c41230")) +
  labs(title = str_squish("Figure 3: Sum Sentiment Scores for Terms from 
                           Feedback on Cultural Centers"),
       x = "", y = "Sum Sentiment Score") + 
  guides(fill = guide_legend(title = "")) +
  facet_wrap(~ prompt, scales = "free") +
  coord_flip() +
  theme_custom(margin_size = -15, legend = TRUE)

# Generate mean sentiment scores by alias using AFINN, Bing, NRC, and VADER
means <- map_df(unique(df_qual$prompt), function(value) {
  # Process each sentiment lexicon for a given prompt
  processed1 <- map_df(c("afinn", "bing", "nrc", "vader"), function(lexicon) {
    processed2 <- df_qual |>
      filter(prompt == value)
    
    # Get compound sentiment scores 
    if (lexicon == "vader") { 
      processed2 <- processed2 |> 
        mutate(score = vader_df(response) |> pull(compound)) 
      # Tokenize responses and join with sentiment scores
    } else { 
      processed2 <- processed2 |> 
        unnest_tokens(ngram, response, token = "ngrams", n = 1)
      if (lexicon == "afinn") {  
        processed2 <- processed2 |>
          inner_join(get_sentiments(lexicon), by = c("ngram" = "word"), 
                     relationship = "many-to-one")  
      } else if (lexicon == "bing" || lexicon == "nrc") {  
        processed2 <- processed2 |>
          inner_join(get_sentiments(lexicon), by = c("ngram" = "word"), 
                     relationship = "many-to-many") 
      }
    }
    
    # Rename the column
    if (lexicon == "afinn") { 
      processed2 <- processed2 |>
        rename(score = value) 
      # Convert sentiments to numeric scores and remove NA values
    } else if (lexicon == "bing" || lexicon == "nrc") { 
      processed2 <- processed2 |>
        mutate(score = case_when(
          sentiment == "positive" ~ 1,
          sentiment == "negative" ~ -1,
          TRUE ~ NA_real_ 
          )) |>
        drop_na(score) 
    } 
    
    # Calculate the mean sentiment scores for the aliases
    processed2 |>
      group_by(alias) |>
      summarize(mean = mean(score)) |>
      ungroup() |>
      mutate(lexicon = if_else(
        lexicon == "bing", str_to_title(lexicon), str_to_upper(lexicon) 
        ),  
        prompt = value) |>
      relocate(prompt, lexicon) 
  })
})

# Remove objects from global environment
rm(incl_nt, neu_set)

# Define a function to plot the mean sentiment scores in a faceted bar chart
plot_means <- function(type) { 
  # Perform processing steps, including filtering and creating a new column 
  processed <- means |>
    filter(lexicon == type) |> 
    select(-lexicon) |>
    mutate(prompt = factor(prompt, levels = c("Facilities", "Staff", "Fees", 
                                              "Programs")), 
           modified_alias = reorder_within(alias, mean, prompt, sep = "_")) |> 
    relocate(modified_alias, .after = alias)
  
  # Create a faceted bar chart to display mean sentiment scores 
  plot <- ggplot(processed, aes(x = modified_alias, y = mean, fill = prompt)) +  
    geom_col(width = 0.825, show.legend = FALSE) +
    geom_hline(yintercept = 0, linewidth = 1.4, color = "#000000") +
    scale_x_discrete(labels = function(x) str_replace_all(x, "_.*", "")) +
    scale_y_continuous(breaks = pretty_breaks(3), 
                       labels = label_number(drop0trailing = TRUE)) + 
    scale_fill_manual(values = c("#0078ae", "#5e9732", "#ffac1c", "#c41230")) +
    labs(x = "", y = "Mean Compound Sentiment Score") +
    facet_wrap(~ prompt, scales = "free") +
    coord_flip() +
    theme_custom(margin_size = -12.5) 
  
  # Dynamically set the title of the plot
  if (type == "VADER") { 
    plot <- plot +  
    ggtitle(label = str_squish("Figure 4: Mean Compound Sentiment Scores from 
                                Feedback on Cultural Centers"))
  } else {  
    plot <- plot + 
    ggtitle(label = str_squish("Figure 4: Mean Sentiment Scores from Feedback
                                on Cultural Centers"))
  }
  
  # Return the plot 
  return(plot)
}

# Output the faceted bar chart for the VADER lexicon
plot_vader <- plot_means("VADER")

# Part 6: Topic Modeling

# Define a function that performs topic modeling
topic_modeling <- function(value, number) { 
  # Process data and create a document-term matrix
  dtm <- ngrams |> 
    filter(prompt == value & type == "unigram") |>
    group_by(ngram) |> 
    mutate(n = n()) |> 
    ungroup() |>
    filter(between(n, 2, number) & !ngram %in% c("aachf", "aarc", "carver", 
                                                 "esb", "macc", "museum")) |> 
    select(-n) |>
    count(alias, ngram) |>
    cast_dtm(document = alias, term = ngram, value = n) |>
    as.matrix()
  
  # Perform hyperparameter tuning to get the optimal number of topics
  hp <- FindTopicsNumber(dtm, 
                         topics = seq(from = 2, to = 20, by = 1),  
                         metrics = "CaoJuan2009", 
                         method = "Gibbs", 
                         control = list(seed = 123)) |> 
    as_tibble() |>
    filter(CaoJuan2009 == min(CaoJuan2009))
  
  # Use latent dirichlet allocation for topic modeling
  model <- LDA(dtm, k = hp$topics, control = list(seed = 123)) 
  
  # Return the model output 
  return(model)
}

# Generate the four outputs from topic modeling
lda1 <- topic_modeling("Facilities", 12)
lda2 <- topic_modeling("Staff", 13)
lda3 <- topic_modeling("Fees", 14)
lda4 <- topic_modeling("Programs", 20)

# Generate an output containing the various topics and terms for the prompts  
lda_output1 <- map2_df(c(lda1, lda2, lda3, lda4), unique(df_qual$prompt), ~ { 
  # Assign values to lda and value for processing
  lda = .x
  value = .y 
  
  # Perform processing steps to obtain the top five words for each topic
  processed <- lda |>
    tidy() |>
    mutate(prompt = value) |>
    group_by(topic) |>
    slice_max(beta, n = 5, with_ties = FALSE) |>
    ungroup() |>
    arrange(topic, desc(beta)) |>
    relocate(prompt)
})

# Define a function to plot the probabilities of terms for the given topics
plot_lda <- function(value) { 
  # Perform processing steps, renaming columns and filtering
  processed <- lda_output1 |>
    rename(ngram = term,
           probability = beta) |>
    filter(prompt == "Programs") |>
    select(-prompt) |>
    mutate(topic = str_c("Topic ", topic), 
           ngram = reorder_within(str_to_title(ngram), probability, topic, 
                                  sep = "_"))
  
  # Create a faceted bar chart to display the term probabilities of topics 
  plot <- ggplot(processed, aes(x = ngram, y = probability)) +   
    geom_col(width = 0.825, fill = "#0078ae") +
    geom_hline(yintercept = 0, linewidth = 1.3, color = "#000000") + 
    scale_x_discrete(labels = function(x) str_replace_all(x, "_.*", "")) +
    scale_y_continuous(breaks = pretty_breaks(3), 
                       labels = label_number(drop0trailing = TRUE)) + 
    labs(title = paste(str_squish("Figure 5: Top Terms in Topics from Feedback 
                                   on the Cultural Centers'"), value),  
         x = "", y = "Term Probability") +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    theme_custom(margin_size = -7.5)
  
  # Return the plot 
  return(plot)
}

# Output the faceted bar chart for the specified prompt 
plot_topics <- plot_lda("Programs")

# Generate an output that displays the document-topic probabilities
lda_output2 <- map2_df(c(lda1, lda2, lda3, lda4), unique(df_qual$prompt), ~ {  
  # Assign values to lda and value for processing
  lda = .x
  value = .y 
  
  # Perform processing steps to find the most likely topic for each document
  processed <- lda |>
    tidy(matrix = "gamma") |>
    mutate(prompt = value,
           gamma = round(gamma, 3)) |>
    group_by(document) |> 
    slice_max(gamma, n = 1) |> 
    ungroup() |>
    group_by(prompt) |>
    arrange(topic) |>
    ungroup() |>
    relocate(prompt)
})