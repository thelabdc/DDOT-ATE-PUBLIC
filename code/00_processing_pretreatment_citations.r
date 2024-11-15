library(tidyverse)
library(here)
# Alex's code simulates his outcome data, but we can use real pre-treatment data. So instead of generating fake, correlated data, here we read in our real pre-treatment data
# (We'll still permute fake treatment status)
 sample <- read_csv(here("data/02_Evaluation/treatment_assignment_20230504.csv"))
 # The citations family includes 2 time periods (3 month, 1 year) x 2 outcome types (total, risky)
 citations <- read_csv(here("data/02_Evaluation/citations.csv"))
 # box_read(1701487010065)
 citations <- citations |> filter(plate %in% sample$plate)
 
 citations_3mo <- citations|> filter(as.Date(ticket_issue_date) > as.Date("2022-11-22"))|>
             group_by(plate) |> summarize(total_cites_3mo = n(), 
                                                      stop_sign_or_red_light_3mo =sum(stop_sign_or_red_light, na.rm = TRUE))
citations_12mo <- citations|> filter(ticket_issue_date > as.Date("2022-02-20"))|>
             group_by(plate) |> summarize(total_cites_12mo = n(), 
                                                      stop_sign_or_red_light_12mo =sum(stop_sign_or_red_light, na.rm = TRUE))
                                                      
                                                      sample_citations <- left_join(citations_3mo,citations_12mo, by = c("plate"))
sample_citations <- left_join(sample, sample_citations)
sample_citations$total_cites_3mo <- ifelse(is.na(sample_citations$total_cites_3mo ), 0, sample_citations$total_cites_3mo)
sample_citations$total_cites_12mo <- ifelse(is.na(sample_citations$total_cites_12mo ), 0, sample_citations$total_cites_12mo)
sample_citations$stop_sign_or_red_light_3mo <- ifelse(is.na(sample_citations$stop_sign_or_red_light_3mo ), 0, sample_citations$stop_sign_or_red_light_3mo)
sample_citations$stop_sign_or_red_light_12mo <- ifelse(is.na(sample_citations$stop_sign_or_red_light_12mo ), 0, sample_citations$stop_sign_or_red_light_12mo)