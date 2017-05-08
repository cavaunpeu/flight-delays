library(tidyverse)
library(stringr)
library(feather)

# import flights
all_flights <- read_csv("data/flights-2008.csv") %>% filter(Cancelled != 1 & !is.na(CRSDepTime))
flights <- all_flights %>% sample_n(50000)

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
  hours <- ifelse(hours == "", 0, hours)
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
ggsave("figures/average_total_delays_by_airport.png")

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
ggsave("figures/distribution_of_log_average_total_delays.png")

## how do day-of-week, day-of-month, month of year relate to total delay?
flights %>%
  group_by(DayOfWeek) %>%
  summarise(avg_total_delay = mean(total_delay)) %>%
  ggplot(aes(x=DayOfWeek, y=avg_total_delay)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    xlim("1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thursday",
         "5" = "Friday", "6" = "Saturday", "7" = "Sunday") +
    labs(
      title = "Average Total Delays by Day of Week",
      x = "Day of Week",
      y = "Average Delay (Minutes)"
    )
ggsave("figures/average_total_delays_by_day_of_week.png")

flights %>%
  group_by(DayofMonth) %>%
  summarise(avg_total_delay = mean(total_delay)) %>%
  ggplot(aes(x=DayofMonth, y=avg_total_delay)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_x_discrete(limits=as.character(1:31)) +
  labs(
    title = "Average Total Delays by Day of Month",
    x = "Day of Month",
    y = "Average Delay (Minutes)"
  )
ggsave("figures/average_total_delays_by_day_of_month.png")

flights %>%
  group_by(Month) %>%
  summarise(avg_total_delay = mean(total_delay)) %>%
  ggplot(aes(x=Month, y=avg_total_delay)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_x_discrete(limits=as.character(1:31)) +
  labs(
    title = "Average Total Delays by Month",
    x = "Month",
    y = "Average Delay (Minutes)"
  )
ggsave("figures/average_total_delays_by_month.png")

# export to feather
path <- "data/flights-2008-sample.feather"
write_feather(flights, path)

# import routes
col_names <- c("airline", "airline_id", "origin", "origin_id", "dest", "dest_id", "codeshare", "stops", "equipment")
routes <- read_csv("data/routes.csv", col_names=col_names)

## which origin airport has the most unique destinations?
routes %>%
  group_by(origin) %>%
  summarise(unique_dests = length(unique(dest))) %>%
  arrange(desc(unique_dests)) %>%
  head(25) %>%
  ggplot(aes(x=reorder(origin, -unique_dests), y=unique_dests)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(
    title = "Unique Destinations by Airport (Top 25)",
    x = "Airport Code",
    y = "Unique Destinations"
  )
ggsave("figures/unique_destinations_by_airport.png")
