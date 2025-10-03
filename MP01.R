if(!dir.exists(file.path("data", "mp01"))){
  dir.create(file.path("data", "mp01"), showWarnings = FALSE,recursive=TRUE)
}

GLOBAL_TOP_10_FILENAME <- file.path("data", "mp01", "global_top10_alltime.csv")

if(!file.exists(GLOBAL_TOP_10_FILENAME)){
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-global.tsv", 
                destfile=GLOBAL_TOP_10_FILENAME)
}

COUNTRY_TOP_10_FILENAME <- file.path("data", "mp01", "country_top10_alltime.csv")

if(!file.exists(COUNTRY_TOP_10_FILENAME)){
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-countries.tsv", 
                destfile=COUNTRY_TOP_10_FILENAME)
}


if(!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)
GLOBAL_TOP_10 <- read_tsv(GLOBAL_TOP_10_FILENAME)

str(GLOBAL_TOP_10)

glimpse(GLOBAL_TOP_10)

GLOBAL_TOP_10 <- GLOBAL_TOP_10 |>
  mutate(season_title = if_else(season_title == "N/A", NA_character_, season_title))

str(GLOBAL_TOP_10)

glimpse(GLOBAL_TOP_10)

if(!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)
COUNTRY_TOP_10 <- read_tsv(COUNTRY_TOP_10_FILENAME)

str(COUNTRY_TOP_10)

glimpse(COUNTRY_TOP_10)

COUNTRY_TOP_10 <- COUNTRY_TOP_10 |>
  mutate(season_title = if_else(season_title == "N/A", NA_character_, season_title))

str(COUNTRY_TOP_10)

glimpse(COUNTRY_TOP_10)

install.packages("DT")

library(DT)
GLOBAL_TOP_10 |> 
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE))

library(stringr)
format_titles <- function(df){
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}

GLOBAL_TOP_10 |> 
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

GLOBAL_TOP_10 |> 
  select(-season_title) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

GLOBAL_TOP_10 |> 
  mutate(`runtime_(minutes)` = round(60 * runtime)) |>
  select(-season_title, 
         -runtime) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

##1.How many different countries does Netflix operate in? 

library(dplyr)

COUNTRY_TOP_10 |>
  summarise(num_countries = n_distinct(country_name))

library(dplyr)
library(DT)

COUNTRY_TOP_10 |>
  distinct(country_name) |>
  arrange(country_name) |>
  datatable(
    rownames = FALSE,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      dom = 'tip',
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
    )
  )

##2.Which non-English-language film has spent the most cumulative weeks in the global top 10?
## How many weeks did it spend?

library(dplyr)

non_english_top_film <- GLOBAL_TOP_10 |>
  filter(grepl("Films", category), grepl("Non-English", category)) |>
  group_by(show_title) |>
  summarise(total_weeks = n(), .groups = "drop") |>
  arrange(desc(total_weeks)) |>
  slice(1)

non_english_top_film
## Table format
library(dplyr)
library(DT)

non_english_top_film <- GLOBAL_TOP_10 |>
  filter(grepl("Films", category), grepl("Non-English", category)) |>
  group_by(show_title) |>
  summarise(total_weeks = n(), .groups = "drop") |>
  arrange(desc(total_weeks)) |>
  slice(1)

datatable(
  non_english_top_film,
  rownames = FALSE,
  options = list(
    pageLength = 5,
    autoWidth = TRUE,
    dom = 'tip',
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  )
)

##3.What is the longest film (English or non-English) to have ever appeared in the Netflix global Top 10? 
## How long is it in minutes?

library(dplyr)

longest_film <- GLOBAL_TOP_10 |>
  filter(str_detect(category, "Films")) |>
  filter(!is.na(runtime)) |>
  mutate(runtime_minutes = round(60 * runtime)) |>
  arrange(desc(runtime_minutes)) |>
  slice(1)

##4.For each of the four categories, what program has the most total hours of global viewership?

if(!require(scales)) install.packages("scales")
library(scales)
GLOBAL_TOP_10 |>
  mutate(show_title = if_else(is.na(season_title), show_title, season_title)) |>
  group_by(category, show_title) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE), .groups = "drop") |>
  group_by(category) |>
  slice_max(order_by = total_hours, n = 1, with_ties = FALSE) |>
  arrange(category) |>
  mutate(total_hours = comma(total_hours)) |>
  datatable(
    options = list(pageLength = 5, searching = FALSE, info = FALSE),
    rownames = FALSE,
    caption = "Programs with the Most Total Global Hours by Category"
  )

##5.Which TV show had the longest run in a country’s Top 10? 
##How long was this run and in what country did it occur?

library(dplyr)

longest_tv_run <- COUNTRY_TOP_10 |>
  group_by(country_name, show_title) |>
  summarise(total_weeks = n(), .groups = "drop") |>
  arrange(desc(total_weeks)) |>
  slice(1)

longest_tv_run

##6.Netflix provides over 200 weeks of service history for all but one country in our data set. 
##Which country is this and when did Netflix cease operations in that country?


library(dplyr)
library(DT)


COUNTRY_TOP_10 <- COUNTRY_TOP_10 |>
  mutate(week = as.Date(week))


country_weeks <- COUNTRY_TOP_10 |>
  group_by(country_name) |>
  summarise(weeks_recorded = n_distinct(week), .groups = "drop") |>
  arrange(weeks_recorded)


short_history <- country_weeks |>
  filter(weeks_recorded < 200)


last_week <- COUNTRY_TOP_10 |>
  filter(country_name == short_history$country_name[1]) |>
  summarise(last_week = max(week, na.rm = TRUE))


result <- short_history |>
  left_join(last_week, by = character())


datatable(
  result,
  rownames = FALSE,
  colnames = c("Country", "Weeks Recorded", "Last Week of Operation"),
  options = list(
    pageLength = 5,
    autoWidth = TRUE,
    dom = 'tip',
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  )
)

##7.What is the total viewership of the TV show Squid Game? 

library(dplyr)
library(DT)

squid_game_total <- GLOBAL_TOP_10 |>
  filter(show_title == "Squid Game") |>
  group_by(show_title) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE), .groups = "drop")


datatable(
  squid_game_total,
  rownames = FALSE,
  colnames = c("Show", "Total Hours"),
  options = list(
    pageLength = 5,
    autoWidth = TRUE,
    dom = 'tip',
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  )
)

##8.The movie Red Notice has a runtime of 1 hour and 58 minutes. 
##Approximately how many views did it receive in 2021? 

library(dplyr)
library(lubridate)


COUNTRY_OR_GLOBAL <- GLOBAL_TOP_10  
COUNTRY_OR_GLOBAL <- COUNTRY_OR_GLOBAL |>
  mutate(week = as.Date(week))


runtime_hours <- 1 + 58/60  


red_notice_2021 <- COUNTRY_OR_GLOBAL |>
  filter(show_title == "Red Notice", year(week) == 2021)


total_hours_2021 <- sum(red_notice_2021$weekly_hours_viewed, na.rm = TRUE)


approx_views <- total_hours_2021 / runtime_hours


red_notice_table <- tibble(
  Movie = "Red Notice",
  Runtime_hours = runtime_hours,
  Total_Hours_Viewed = total_hours_2021,
  Approx_Views = round(approx_views)
)

library(DT)
datatable(
  red_notice_table,
  rownames = FALSE,
  options = list(
    pageLength = 5,
    autoWidth = TRUE,
    dom = 'tip',
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  )
)

##9.How many Films reached Number 1 in the US but did not originally debut there? 

library(dplyr)
library(lubridate)

# Filter for US films (season_title not available, so all rows are films)
us_films <- COUNTRY_TOP_10 |>
  filter(country_name == "United States")

# For each film, check first rank and if it ever hit #1
films_hit_1 <- us_films |>
  group_by(show_title) |>
  summarise(
    first_rank = weekly_rank[which.min(week)],  # rank in first week
    first_week = min(week),                     # first week appeared
    ever_rank1 = any(weekly_rank == 1),        # did it ever hit #1?
    last_week = max(week),                      # most recent week
    .groups = "drop"
  ) |>
  filter(ever_rank1 == TRUE, first_rank != 1)  # did not debut at #1 but eventually hit #1

# Count how many films
num_films <- nrow(films_hit_1)

# Find the most recent film
most_recent_us_film <- films_hit_1 |>
  arrange(desc(last_week)) |>
  slice(1)


num_films
most_recent_us_film

##10. Which TV show/season hit the top 10 in the most countries in its debut week? 
##In how many countries did it chart?


library(dplyr)
library(lubridate)

tv_data <- COUNTRY_TOP_10 |>
  filter(!is.na(season_title))

debut_weeks <- tv_data |>
  group_by(show_title, season_title) |>
  summarise(debut_week = min(week), .groups = "drop")

top_10_countries_in_debut <- tv_data |>
  inner_join(debut_weeks, by = c("show_title", "season_title")) |>
  filter(week == debut_week) |>
  group_by(show_title, season_title) |>
  summarise(countries_charted = n_distinct(country_name), .groups = "drop") |>
  arrange(desc(countries_charted)) |>
  slice(1)

top_10_countries_in_debut

## Press Release 1

