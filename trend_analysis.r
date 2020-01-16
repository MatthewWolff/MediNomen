library(tidyverse)
library(gtrendsR)
library(maps)

candidates <- c("Rehab physician",
                "PM & R",
                "Physiatry",
                "Physical Medicine and Rehabilitation",
                "Rehab medicine")

all_data <- gtrends(keyword=candidates, gprop=c("web"), geo="US", time="all")
plot(all_data)

all_data$interest_over_time %>% 
  ggplot(aes(x=date, y=hits, group=keyword, col=keyword)) +
  geom_smooth(span=0.4, se=FALSE) + 
  xlab('Time') + 
  ylab('Relative Interest') +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text=element_text(size=12)) +
  ggtitle("Google Search Volume (all of US)")

states_map <- map_data("state")
mapped_search <- merge(states_map, all_data, by.x="region", by.y="location")

states_map <- map_data("state")
harvey <- gtrends(c("physiatry"), 
                  gprop = "web",
                  time  = "all", 
                  geo   = "US")
harvey$interest_by_region %>% as_tibble %>%
  mutate(location=tolower(location)) %>%
  merge(states_map, ., by.x="region", by.y="location") %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=log(hits)), colour="white") +
  blank + theme(axis.ticks = element_blank(), 
                axis.line  = element_blank(),
                axis.text  = element_blank(),
                axis.title = element_blank())
