library(tidyverse)
library(here)
library(ggthemes)
library(geomtextpath)

data.elections <- read_csv(here("results_NL_tweedekamer.csv")) %>%
  select(-where(is.logical)) %>% select(-c("exitpoll1","exitpoll2","ochtendprognose")) %>%
  pivot_longer(
    cols = `1994`:`2023`,
    names_to = "Year",
    values_to = "Seats"
  ) %>% mutate(Year=as.numeric(Year))
data.elections$`Political orientation` = factor(data.elections$`Political orientation`,
                              levels=c('progressive/left',
                                       'centre',
                                       'centre/right',
                                       'radical right'))

election.years <- data.elections %>% pull(Year) %>% unique() %>% sort()

orientation.colors <- c(
  "progressive/left" = "red",
  "centre" = "#b2df8a",
  "centre/right" = "#1f78b4",
  "radical right" = "#4a412a"
)

# party.colors <- c(
#   # all parties
#   "PVV" =  "#4a412a",
#   "GL/PVDA" =  "red",
#   "VVD" =  "#1f78b4",
#   "NSC" =  "#1f78b4",
#   "D66" =  "#b2df8a",
#   "BBB" =  "#1f78b4",
#   "CDA" =  "#1f78b4",
#   "SP" =  "red",
#   "PvdD" =  "red",
#   "FvD" =  "#4a412a",
#   "CU" =  "#b2df8a",
#   "SGP" =  "#1f78b4",
#   "Volt" =  "red",
#   "DENK" =  "red",
#   "JA21" =  "#4a412a",
#   "50PLUS" =  "#b2df8a",
#   "BIJ1" =  "red",
#   "Fortuyn" =  "#4a412a",
#   # ...
# )



# Plot political orientation ====
p.orientation <- ggplot(data.elections,
                        aes(x=Year,
                            y=Seats)) +
  geom_hline(yintercept = 75, color = "grey", linetype = "dashed") +
  geom_col(aes(fill=`Political orientation`),colour="white", position = "stack") +
  # geom_text(aes(label = `Party`), colour="white", position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = orientation.colors) +
  scale_x_continuous(breaks = election.years) +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Seats in Dutch general elections")
p.orientation
