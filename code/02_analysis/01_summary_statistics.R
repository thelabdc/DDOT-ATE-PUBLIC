# Summary Statistics/Balance 
# Note that if you want pretreat citation you need to uncomment it in master (it takes a long time to load)

# TABLE 1: SUMMARY OF COVARIATE BALANCE
ate |>
  filter(match == 0)|>
  mutate(assignment = factor(assignment, levels = c("Control", "Mailer", "Text", "Both")))|>
  group_by(assignment)|> 
  mutate(virginia = ifelse(state_ward == "VA",1,0),
         maryland = ifelse(state_ward == "MD",1,0), 
         dc = ifelse(state_ward %in% c('DC', 'Ward 1','Ward 2', 'Ward 3', 'Ward 4', 'Ward 5', 'Ward 6', 'Ward 7', 'Ward 8'),1,0),
         ward1 = ifelse(state_ward == "Ward 1",1,0),
         ward2= ifelse(state_ward == "Ward 2",1,0),
         ward3 = ifelse(state_ward == "Ward 3",1,0),
         ward4 = ifelse(state_ward == "Ward 4",1,0),
         ward5 = ifelse(state_ward == "Ward 5",1,0),
         ward6= ifelse(state_ward == "Ward 6",1,0),
         ward7 = ifelse(state_ward == "Ward 7",1,0),
         ward8 = ifelse(state_ward == "Ward 8",1,0),
         otherdc = ifelse(state_ward == "DC",1,0)) |>
  summarize(round(mean(risk_score),3), 
            paste(100*round(mean(virginia),2),"%", sep = ""), 
            paste(100*round(mean(maryland),2),"%", sep = ""), 
            paste(100*round(mean(dc),2),"%", sep = ""), 
            paste(100*round(mean(ward1),2),"%", sep = ""), 
            paste(100*round(mean(ward2),2),"%", sep = ""), 
            paste(100*round(mean(ward3),2),"%", sep = ""), 
            paste(100*round(mean(ward4),2),"%", sep = ""), 
            paste(100*round(mean(ward5),2),"%", sep = ""), 
            paste(round(mean(ward6),2),"%", sep = ""), 
            paste(100*round(mean(ward7),2),"%", sep = ""), 
            paste(100*round(mean(ward8),2),"%", sep = ""), 
            paste(100*round(mean(otherdc),2),"%", sep = ""), 
  ) |>
  as.data.frame()|>
  write_csv(file = "tables/summary/balance_nomatch.csv")

ate |>
  mutate(assignment = factor(assignment, levels = c("Control", "Mailer", "Text", "Both")))|>
  filter(match == 1)|>
  group_by(assignment)|> 
  mutate(virginia = ifelse(state_ward == "VA",1,0),
         maryland = ifelse(state_ward == "MD",1,0), 
         dc = ifelse(state_ward %in% c('DC', 'Ward 1','Ward 2', 'Ward 3', 'Ward 4', 'Ward 5', 'Ward 6', 'Ward 7', 'Ward 8'),1,0),
         ward1 = ifelse(state_ward == "Ward 1",1,0),
         ward2= ifelse(state_ward == "Ward 2",1,0),
         ward3 = ifelse(state_ward == "Ward 3",1,0),
         ward4 = ifelse(state_ward == "Ward 4",1,0),
         ward5 = ifelse(state_ward == "Ward 5",1,0),
         ward6= ifelse(state_ward == "Ward 6",1,0),
         ward7 = ifelse(state_ward == "Ward 7",1,0),
         ward8 = ifelse(state_ward == "Ward 8",1,0),
         otherdc = ifelse(state_ward == "DC",1,0)) |>
  summarize(round(mean(risk_score),3), 
            paste(100*round(mean(virginia),2),"%", sep = ""), 
            paste(100*round(mean(maryland),2),"%", sep = ""), 
            paste(100*round(mean(dc),2),"%", sep = ""), 
            paste(100*round(mean(ward1),2),"%", sep = ""), 
            paste(100*round(mean(ward2),2),"%", sep = ""), 
            paste(100*round(mean(ward3),2),"%", sep = ""), 
            paste(100*round(mean(ward4),2),"%", sep = ""), 
            paste(100*round(mean(ward5),2),"%", sep = ""), 
            paste(round(mean(ward6),2),"%", sep = ""), 
            paste(100*round(mean(ward7),2),"%", sep = ""), 
            paste(100*round(mean(ward8),2),"%", sep = ""), 
            paste(100*round(mean(otherdc),2),"%", sep = ""), 
  ) |>
  as.data.frame()|>
  write_csv(file = "tables/summary/balance_match.csv")



# TABLE 2: SUMMARY OF PRETREATMENT OUTCOMES 

# Filter data to one year 
pretreat_citation_1yr_summary <- pretreat_citation |> 
  filter(ticket_issue_date > as.Date("2021-04-28") & ticket_issue_date < as.Date("2022-04-28"))|>
  mutate(
    risky = ifelse(violation %in% c("OVER 25 MPH CONTROL", "OVER 25 MPH NON CONT", 
                                    "SPEED 21-25 OVR LIMT", "PASS REDLIGHT"),1,0)  )|>
  group_by(plate_rep_id, ticket_rep_id)|>
  slice(1) |>
  ungroup() |>
  group_by(plate_rep_id)|>
  summarize(n_citations = n(), 
            n_risky = sum(risky)) |>
  rename(n_pretreat_citations = n_citations, 
         n_pretreat_risky = n_risky)

pretreat_citation_1yr_summary <- left_join(ate, pretreat_citation_1yr_summary, by = "plate_rep_id")|>
  mutate(n_pretreat_citations = ifelse(is.na(n_pretreat_citations), 0, n_pretreat_citations), 
         n_pretreat_risky = ifelse(is.na(n_pretreat_risky), 0,n_pretreat_risky)) 

pretreat_crash_1yr_summary <- pretreat_crash |>
  clean_names()|>
  filter(accidentdate  > as.Date("2021-04-28") & accidentdate < as.Date("2022-04-28") )|>
  group_by(complaint_rep_id, plate_rep_id)|>
  slice(1)|>
  ungroup()|>
  group_by(plate_rep_id)|>
  summarize(n_crashes = n())|>
  rename(n_pretreat_crashes = n_crashes)

pretreat_crash_1yr_summary <- left_join(ate, pretreat_crash_1yr_summary)|>
  dplyr::mutate(n_pretreat_crashes = ifelse(is.na(n_pretreat_crashes), 0, n_pretreat_crashes))

pretreat_outcomes <- left_join(pretreat_crash_1yr_summary, pretreat_citation_1yr_summary)

#For pretreatment outcomes summary table 
pretreat_outcomes |>
  summarize(mean_pretreat_crashes = mean(n_pretreat_crashes), 
            mean_pretreat_citation_risky = mean(n_pretreat_risky), 
            mean_pretreat_citations = mean(n_pretreat_citations))

pretreat_outcomes |>
            filter(match ==1)|>
            summarize(mean_pretreat_crashes = mean(n_pretreat_crashes), 
            mean_pretreat_citation_risky = mean(n_pretreat_risky), 
            mean_pretreat_citations = mean(n_pretreat_citations))

# By assignment group, for balance table 
pretreat_outcomes |> 
  filter(match == 0)|>
  group_by(assignment)|>
  summarize(mean_pretreat_citations = format(mean(n_pretreat_citations),nsmall=4),
            mean_pretreat_citation_risky = mean(n_pretreat_risky), 
            mean_pretreatcrashes = mean(n_pretreat_crashes))|>
  write_csv(file = "tables/summary/pretreat_outcome_balance_no_match.csv")


pretreat_outcomes |> 
  mutate(assignment = factor(assignment, levels = c("Control", "Mailer", "Text", "Both")))|>
  filter(match == 1)|>
  group_by(assignment)|>
  summarize(mean_pretreat_citations = format(round(mean(n_pretreat_citations),4), nsmall =4),
            mean_citation_pretreat_risky = round(mean(n_pretreat_risky),4), 
            mean_pretreat_crashes = round(mean(n_pretreat_crashes),4))|>
  write_csv(file = "tables/summary/pretreat_outcome_balance_match.csv")

