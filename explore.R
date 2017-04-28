library(tidyverse)

# import flights
all_flights <- read_csv("data/flights-2008.csv")
flights <- all_flights %>% sample_n(10000)

## how many unique origin, destination, and carriers are there?
flights %>%
  select(Origin, Dest, UniqueCarrier) %>%
  summarise(
    unique_origins = length(unique(Origin)),
    unique_dests = length(unique(Dest)),
    unique_carriers = length(unique(UniqueCarrier))
  )

## how is departure time distributed?
hhmm_to_timestamp <- function(hhmm) {
  mins <- str_sub(hhmm, -2)
  hours <- str_replace(hhmm, mins, "")
  timestamp <- 60*as.numeric(hours) + as.numeric(mins)
  return(timestamp)
}

flights <- flights %>% mutate(ScheduledDepTimestamp = hhmm_to_timestamp(CRSDepTime))
flights$ScheduledDepTimestamp %>% hist(main = "Scheduled Departure Timestamp Distribution\n(Midnight is 1440)")

## how are delays distributed, and which origin airports have the highest?
flights <- flights %>%
  mutate_each(funs(replace(., is.na(.), 0)), CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>%
  mutate(total_delay = CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay)

### 25 airports with highest average delays
flights %>%
  group_by(Origin) %>%
  summarise(avg_total_delay = mean(total_delay)) %>%
  select(Origin, avg_total_delay) %>%
  arrange(desc(avg_total_delay)) %>%
  head(25) %>%
  ggplot(aes(x=reorder(Origin, -avg_total_delay), y=avg_total_delay)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    labs(
      title = "Average Total Delays by Airport (Top 25)",
      x = "Airport Code",
      y = "Average Delay (Minutes)"
    )

### average delay distribution
flights %>%
  group_by(Origin) %>%
  summarise(avg_total_delay = mean(total_delay)) %>%
  select(avg_total_delay) %>%
  ggplot(aes(avg_total_delay)) +
    geom_histogram(aes(color=I("white"))) +
    theme_minimal() +
    labs(
      title = "Distribution of Average Total Delays",
      x = "Average Total Delay (in Minutes)",
      y = "Count"
    )

### log average delay distribution
flights %>%
  group_by(Origin) %>%
  summarise(avg_total_delay = mean(total_delay)) %>%
  mutate(log_avg_total_delay = log(avg_total_delay + 1)) %>%
  select(log_avg_total_delay) %>%
  ggplot(aes(log_avg_total_delay)) +
  geom_histogram(aes(color=I("white"))) +
  theme_minimal() +
  labs(
    title = "Distribution of Log Average Total Delays",
    x = "Log Average Total Delay (in Minutes)",
    y = "Count"
  )

