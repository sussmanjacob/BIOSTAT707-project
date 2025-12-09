
# Packages
library(tidyverse)
library(naniar)
library(ggplot2)
library(reshape2)
library(scales)
library(gt)
library(patchwork)

# Data
df <- read.csv("subject_level_FC.csv")

df <- df %>%
  separate(BP, into = c("BP_sys", "BP_dia"), sep = "/", remove = FALSE) %>%
  mutate(
    BP_sys = as.numeric(BP_sys),
    BP_dia = as.numeric(BP_dia)
  )

df$Height_inches <- df$Height %>%
  na_if(".") %>%          # convert "." → NA
  as.numeric()   

df <- df %>% select(-c(BP, Height))


# Variables to ignore (leave BP & Height IN the numeric dataset)
ignore_vars <- c("n_BMI", "n_BP", "n_Height", "n_Weight_Lbs", "BMI", "subject_id")

df_use <- df %>% select(-any_of(ignore_vars))

# Set color palette
my_cols <- c(
  "#643b9f", "#ac94f4", "#795695",
  "#6baed6", "#6488ea", "#4e9fe5",
  "#67c897", "#3ab270"
)

# Missing Data Summary
missing_summary <- df_use %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "missing_count") %>%
  mutate(missing_pct = missing_count / nrow(df_use) * 100)

missing_summary

# Visualize Missing Data
ggplot(missing_summary, aes(x = reorder(variable, -missing_count),
                            y = missing_count, fill = missing_count)) +
  geom_col() +
  geom_text(aes(label = paste0(round(missing_pct, 1), "%")),
            hjust = -0.1, size = 3) +
  scale_fill_gradientn(colors = my_cols) +
  coord_flip() +
  labs(title = "Missing Values per Variable",
       x = "Variable",
       y = "Number of Missing Observations") +
  theme_minimal() +
  expand_limits(y = max(missing_summary$missing_count) * 1.15)


# Select numeric variables except ignored (BP & Height now included)
df_num <- df %>%
  select(-any_of(ignore_vars)) %>%   # remove only unwanted ones
  select(where(is.numeric))  

# Build summary table
summary_table <- data.frame(
  variable = names(df_num),
  mean = sapply(df_num, function(x) mean(x, na.rm = TRUE)),
  sd   = sapply(df_num, function(x) sd(x, na.rm = TRUE)),
  min  = sapply(df_num, function(x) min(x, na.rm = TRUE)),
  max  = sapply(df_num, function(x) max(x, na.rm = TRUE))
)

# Create GT summary
summary_table %>%
  gt() %>%
  fmt_number(columns = c(mean, sd, min, max), decimals = 2) %>%
  tab_header(title = "Summary Statistics for Numeric Variables")


# Winsorization function
winsorize_1_99 <- function(x) {
  p1  <- quantile(x, 0.01, na.rm = TRUE)
  p99 <- quantile(x, 0.99, na.rm = TRUE)
  
  x[x < p1]  <- p1
  x[x > p99] <- p99
  
  return(x)
}

# Apply winsorization to numeric variables
df_num_wins <- df_num %>%
  mutate(across(everything(), winsorize_1_99))

# Build summary table
summary_table <- data.frame(
  variable = names(df_num_wins),
  mean = sapply(df_num_wins, function(x) mean(x, na.rm = TRUE)),
  sd   = sapply(df_num_wins, function(x) sd(x, na.rm = TRUE)),
  min  = sapply(df_num_wins, function(x) min(x, na.rm = TRUE)),
  max  = sapply(df_num_wins, function(x) max(x, na.rm = TRUE))
)

summary_table %>%
  gt() %>%
  fmt_number(columns = c(mean, sd, min, max), decimals = 2) %>%
  tab_header(title = "Summary Statistics for Numeric Variables (Winsorized 1–99%)")


numeric_vars <- df %>%
  select(-any_of(ignore_vars)) %>%
  select(where(is.numeric)) %>%
  colnames()

df[numeric_vars] <- lapply(df[numeric_vars], winsorize_1_99)

df_icu_true  <- df %>% filter(ever_icu_any_visit == TRUE)
df_icu_false <- df %>% filter(ever_icu_any_visit == FALSE)

df_icu_true_complete     <- df_icu_true  %>% filter(complete.cases(.))
df_icu_true_incomplete   <- df_icu_true  %>% filter(!complete.cases(.))
df_icu_false_complete    <- df_icu_false %>% filter(complete.cases(.))
df_icu_false_incomplete  <- df_icu_false %>% filter(!complete.cases(.))


# Histogram function
make_hist <- function(data, var, col) {
  ggplot(data, aes_string(x = var)) +
    geom_histogram(
      bins = 30,
      fill = col,
      color = "white",
      na.rm = TRUE       # <--- THIS FIXES THE WARNING
    ) +
    scale_x_continuous(breaks = pretty_breaks(4)) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_blank(),
      strip.text = element_text(size = 10)
    ) +
    labs(x = var, y = "Count")
}


numeric_vars <- colnames(df_num_icu_true)

# ICU — complete
plots_complete_true <- lapply(numeric_vars, function(v)
  make_hist(df_icu_true_complete, v, my_cols[4]))
wrap_plots(plots_complete_true, ncol = 3) +
  plot_annotation(title = "Distributions of Numeric Variables — Complete Cases with ICU Admission")

# ICU — incomplete
plots_incomplete_true <- lapply(numeric_vars, function(v)
  make_hist(df_icu_true_incomplete, v, my_cols[7]))
wrap_plots(plots_incomplete_true, ncol = 3) +
  plot_annotation(title = "Incomplete Cases with ICU Admission")

# No ICU — complete
plots_complete_false <- lapply(numeric_vars, function(v)
  make_hist(df_icu_false_complete, v, my_cols[4]))
wrap_plots(plots_complete_false, ncol = 3) +
  plot_annotation(title = "Complete Cases without ICU Admission")

# No ICU — incomplete
plots_incomplete_false <- lapply(numeric_vars, function(v)
  make_hist(df_icu_false_incomplete, v, my_cols[7]))
wrap_plots(plots_incomplete_false, ncol = 3) +
  plot_annotation(title = "Incomplete Cases without ICU Admission")


# Winsorize original df, remove BMI, then split & save
df_wins <- df
df_wins[numeric_vars] <- lapply(df_wins[numeric_vars], winsorize_1_99)
df_wins <- df_wins %>% select(-BMI)

df_wins_true  <- df_wins %>% filter(ever_icu_any_visit == TRUE)
df_wins_false <- df_wins %>% filter(ever_icu_any_visit == FALSE)

df_true_complete     <- df_wins_true  %>% filter(complete.cases(.))
df_true_incomplete   <- df_wins_true  %>% filter(!complete.cases(.))
df_false_complete    <- df_wins_false %>% filter(complete.cases(.))
df_false_incomplete  <- df_wins_false %>% filter(!complete.cases(.))

write.csv(df_true_complete,     "df_icu_true_complete.csv",     row.names = FALSE)
write.csv(df_true_incomplete,   "df_icu_true_incomplete.csv",   row.names = FALSE)
write.csv(df_false_complete,    "df_icu_false_complete.csv",    row.names = FALSE)
write.csv(df_false_incomplete,  "df_icu_false_incomplete.csv",  row.names = FALSE)


# Create a unified table of counts
summary_df <- tibble(
  ICU_status   = c("TRUE", "TRUE", "FALSE", "FALSE"),
  Completeness = c("Complete", "Incomplete", "Complete", "Incomplete"),
  Count = c(
    nrow(df_true_complete),
    nrow(df_true_incomplete),
    nrow(df_false_complete),
    nrow(df_false_incomplete)
  )
)

# Convert to wide format for a clean gt table
summary_wide <- summary_df %>%
  tidyr::pivot_wider(
    names_from = Completeness,
    values_from = Count
  )

# Produce GT table
summary_wide %>%
  gt() %>%
  tab_header(
    title = "Completeness by ICU Status"
  ) %>%
  cols_label(
    ICU_status = "ICU Status",
    Complete   = "Complete Cases",
    Incomplete = "Incomplete Cases"
  )
