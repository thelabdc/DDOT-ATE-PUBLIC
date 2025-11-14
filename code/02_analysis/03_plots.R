# Bar plots on confirmatory analyses -------------------------------------------
# Start by running the regressions 
source("code/04-evaluation/02_analysis/01_analysis.R")

# Style guide things 
lab_blue <- "#2B4888"
lab_pink <- "#DE4057"
lab_green <- "#0F9247"
obpm_darkblue <- "#173a47"

# Two plots: 
# Risky Citations 
# Control vs. Mailer vs. Text vs Both
n_c1 <-  nrow(matched)

C1_tidy <- tidy(C1)
coefs_risky <- tibble(
  name = c("Control", "Letter", "Text Message", "Both"),
  estimate = c(C1_tidy[C1_tidy$term == "(Intercept)","estimate"],
               C1_tidy[C1_tidy$term == "assignmentMailer","estimate"],
               C1_tidy[C1_tidy$term == "assignmentText","estimate"],
               C1_tidy[C1_tidy$term == "assignmentBoth","estimate"]),
  std.error = c(C1_tidy[C1_tidy$term == "(Intercept)","std.error"],
                C1_tidy[C1_tidy$term == "assignmentMailer","std.error"],
                C1_tidy[C1_tidy$term == "assignmentText","std.error"],
                C1_tidy[C1_tidy$term == "assignmentBoth","std.error"] )
)
# Add intercept to get linear combinations
coefs_risky$value <- ifelse(coefs_risky$name == "Control", coefs_risky$estimate,
                            C1_tidy[C1_tidy$term == "(Intercept)","estimate"] + coefs_risky$estimate )
coefs_risky$conf.low <- coefs_risky$value -1.96*coefs_risky$std.error
coefs_risky$conf.high <- coefs_risky$value +1.96*coefs_risky$std.error

coefs_risky$name <- factor(coefs_risky$name,
                           levels = c("Control", "Letter", "Text Message", "Both"),
                           labels = c("No message", "Letter", "Text Message", "Both"))

plot_risky <- ggplot(coefs_risky) +
  geom_bar(aes(x = name,
               y = value, 
               fill = name), stat = "identity")+ 
  geom_errorbar(aes(x= name, ymin = conf.low, ymax = conf.high), width = .4) + 
  theme_classic()+ 
  theme(legend.position = "none") + 
  labs(x = "", y = "Citations: Red light or \n speeding by 20+mph",
       title = "Riskiest Citations \n 3 Month-Results", 
       fill = "",
       caption = paste("Source: Analysis of ", n_c1, " high-risk drivers with public-record phone numbers \n July-October 2023", sep = "")) +
  ylim(0,.050) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_manual(values = c( "#A8A8A8","#7AA6CA", obpm_darkblue, lab_green))+
  theme(text = element_text(family = "Neutra Text Light Alt", size = 25),
        plot.title = element_text(face = "bold"))

ggsave(plot_risky, file = here("figs/lab-report/risky_cites_with_ci.png"))


D1_tidy <- tidy(D1)
coefs_total <- tibble(
  name = c("Control", "Letter", "Text Message", "Both"),
  estimate = c(D1_tidy[D1_tidy$term == "(Intercept)","estimate"],
               D1_tidy[D1_tidy$term == "assignmentMailer","estimate"],
               D1_tidy[D1_tidy$term == "assignmentText","estimate"],
               D1_tidy[D1_tidy$term == "assignmentBoth","estimate"]),
  std.error = c(D1_tidy[D1_tidy$term == "(Intercept)","std.error"],
                D1_tidy[D1_tidy$term == "assignmentMailer","std.error"],
                D1_tidy[D1_tidy$term == "assignmentText","std.error"],
                D1_tidy[D1_tidy$term == "assignmentBoth","std.error"] )
)
# Add intercept to get linear combinations
coefs_total$value <- ifelse(coefs_total$name == "Control", coefs_total$estimate,
                            D1_tidy[D1_tidy$term == "(Intercept)","estimate"] + coefs_total$estimate )
coefs_total$conf.low <- coefs_total$value -1.96*coefs_total$std.error
coefs_total$conf.high <- coefs_total$value +1.96*coefs_total$std.error

coefs_total$name <- factor(coefs_total$name,
                           levels = c("Control", "Letter", "Text Message", "Both"),
                           labels = c("No message", "Letter", "Text Message", "Both"))


plot_total <- ggplot(coefs_total) +
  geom_bar(aes(x = name,
               y = value, 
               fill = name), stat = "identity")+ 
  geom_errorbar(aes(x= name, ymin = conf.low, ymax = conf.high), width = .4) + 
  theme_classic()+ 
  theme(legend.position = "none") + 
  labs(x = "", y = "Citations: All",
       title = "Total Citations  \n 3-Month Results", 
       fill = "")+ 
  ylim(0,1)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c( "#A8A8A8","#7AA6CA", obpm_darkblue, lab_green))+
  
  theme(text = element_text(family = "Neutra Text Light Alt",  size = 25))+
  theme(plot.title = element_text(face = "bold"))

ggsave(plot_total, file = here("figs/lab-report/total_cites_with_ci.png"))

# Backup slides ----------------------------------------------------------------
# Summary Stats on the outcomes ------------------- ----------------------------
citations <- box_read(1376261171539)
citations <- citations |> group_by(violation)|> mutate(count = n())
citations<- citations%>% 
  mutate(
    risky = ifelse(violation %in% c("OVER 25 MPH CONTROL", "OVER 25 MPH NON CONT", 
                                    "SPEED 21-25 OVR LIMT", "SIGNAL PASS REDLIGHT"),"Riskiest","Other")  )
citations$violation <- ifelse(citations$violation %in% c("OVER 25 MPH CONTROL", "OVER 25 MPH NON CONT",
                                                         "SPEED 21-25 OVR LIMT"), "SPEEDING 20+", citations$violation)

citations$violation <- factor(citations$violation, 
                              levels = c("SPEED 16-20 OVR LIMT", "SPEED 11-15 OVR LIMT","SPEEDING 20+", 
                                         "SIGNAL PASS REDLIGHT", "STOP SIGN, PASSING", "FAIL CMPL STOP RONRD", "NO RIGHT TURN ON RED", ""), 
                              labels = c("Speeding 16-20 over limit", "Speeding 11-15 over limit","Speeding 20+ mph over limit", 
                                         "Red light violation", "Passed stop sign", "No stop before right-on-red", "Illegal right-on-red", ""))

sample <- citations |> filter(tick_plate %in% ate$plate) |> filter(as.Date(ticket_issue_date) > as.Date("2023-06-30"))

cite_category <- ggplot(sample) + 
  geom_bar(aes(x =reorder(violation, count), fill= risky))+
  coord_flip() + 
  theme_classic() + 
  labs(title = "Citations by Category", 
       subtitle = "July 2023-October 2023 \n 100,000 Riskiest Drivers", 
       x = "", 
       y = "Number of citations",
       fill = "") +
  scale_fill_manual(values = c(obpm_darkblue,lab_green)) + 
  theme(legend.position =  "bottom")+
  theme(text = element_text(family = "Neutra Text Light Alt",  size = 25))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
ggsave(cite_category, file = here("figs/cite_category.png"))

# Number of citations per driver 
driver_summaries <- sample |> 
  group_by(tick_plate) |> 
  summarize(not_risky_count = sum(risky != "Riskiest"),
            risky_count = sum(risky=="Riskiest"),
            total = n()) 
plates <- tibble(plate =unique(ate$plate))
driver_summaries <- left_join(plates, driver_summaries, by = c("plate" = "tick_plate"))
driver_summaries$total <- ifelse(is.na(driver_summaries$total), 0, driver_summaries$total)
driver_summaries$risky_count <- ifelse(is.na(driver_summaries$risky_count), 0, driver_summaries$risky_count)

driver_summaries$cat_total <- case_when(
  driver_summaries$total ==0 ~ "0", 
  driver_summaries$total == 1 ~ "1", 
  driver_summaries$total %in% 1:3 ~ "2-3", 
  driver_summaries$total %in% 4:7 ~ "4-7", 
  driver_summaries$total %in% 8:10 ~ "8-10", 
  driver_summaries$total %in% 11:15 ~ "11-15", 
  driver_summaries$total %in% 16:30 ~ "16-30", 
  driver_summaries$total %in% 31:50 ~ "31-50", 
  driver_summaries$total > 50 ~ "50+",
  TRUE ~ NA_character_
)

driver_summaries$cat_risky <- case_when(
  driver_summaries$risky_count ==0 ~ "0", 
  driver_summaries$risky_count == 1 ~ "1", 
  driver_summaries$risky_count %in% 1:3 ~ "2-3", 
  driver_summaries$risky_count %in% 4:7 ~ "4-7", 
  driver_summaries$risky_count %in% 8:10 ~ "8-10", 
  driver_summaries$risky_count %in% 11:15 ~ "11-15", 
  driver_summaries$risky_count %in% 16:30 ~ "16-30", 
  driver_summaries$risky_count %in% 31:50 ~ "31-50", 
  driver_summaries$risky_count > 50 ~ "50+",
  TRUE ~ NA_character_
)


# Coef plot for full sample (mailer)--------------------------------------------
# Risky Citations 
# Control vs. Mailer vs. Text vs Both
n_a1 <-  nrow(ate[ate$assignment=="Mailer"|ate$assignment == "Control",])

A1_tidy <- tidy(A1)
coefs_risky_all <- tibble(
  name = c("Control", "Letter"),
  estimate = c(A1_tidy[A1_tidy$term == "(Intercept)","estimate"],
               A1_tidy[A1_tidy$term == "mailer","estimate"]),
  std.error = c(A1_tidy[A1_tidy$term == "(Intercept)","std.error"],
                A1_tidy[A1_tidy$term == "mailer","std.error"]
  )
)
# Add intercept to get linear combinations
coefs_risky_all$value <- ifelse(coefs_risky_all$name == "Control", coefs_risky_all$estimate,
                                A1_tidy[A1_tidy$term == "(Intercept)","estimate"] + coefs_risky_all$estimate )
coefs_risky_all$conf.low <- coefs_risky_all$value -1.96*coefs_risky_all$std.error
coefs_risky_all$conf.high <- coefs_risky_all$value +1.96*coefs_risky_all$std.error

coefs_risky_all$name <- factor(coefs_risky_all$name,
                               levels = c("Control", "Letter"), 
                               labels =c("No message", "Letter"))

plot_risky_all <- ggplot(coefs_risky_all) +
  geom_bar(aes(x = name,
               y = value, 
               fill = name), stat = "identity")+ 
  geom_errorbar(aes(x= name, ymin = conf.low, ymax = conf.high), width = .4) + 
  theme_classic()+ 
  theme(legend.position = "none") + 
  labs(x = "", y = "Citations: Red light or  \n speeding by 20+mph",
       title = "Riskiest Citations \n 3-Month Results", 
       fill = "", 
       caption = paste("Source: Analysis of ", n_a1, " high-risk drivers between July-October 2023", sep = "")) +
  ylim(0,.075) +
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_manual(values = c( "#A8A8A8","#7AA6CA", obpm_darkblue, lab_green))+
  theme(text = element_text(family = "Neutra Text Light Alt", size = 25),
        plot.title = element_text(face = "bold"))

ggsave(plot_risky_all, file = here("figs/lab-report/risky_cites_with_ci_wholesample.png"))


B1_tidy <- tidy(B1)
coefs_total_all <- tibble(
  name = c("Control", "Letter"),
  estimate = c(B1_tidy[B1_tidy$term == "(Intercept)","estimate"],
               B1_tidy[B1_tidy$term == "mailer","estimate"]),
  std.error = c(B1_tidy[B1_tidy$term == "(Intercept)","std.error"],
                B1_tidy[B1_tidy$term == "mailer","std.error"])
)
# Add intercept to get linear combinations
coefs_total_all$value <- ifelse(coefs_total_all$name == "Control", coefs_total_all$estimate,
                                B1_tidy[B1_tidy$term == "(Intercept)","estimate"] + coefs_total_all$estimate )
coefs_total_all$conf.low <- coefs_total_all$value -1.96*coefs_total_all$std.error
coefs_total_all$conf.high <- coefs_total_all$value +1.96*coefs_total_all$std.error

coefs_total_all$name <- factor(coefs_total_all$name,
                               levels = c("Control", "Letter"),
                               labels = c("No message", "Letter"))

plot_total_all <- ggplot(coefs_total_all) +
  geom_bar(aes(x = name,
               y = value, 
               fill = name), stat = "identity")+ 
  geom_errorbar(aes(x= name, ymin = conf.low, ymax = conf.high), width = .4) + 
  theme_classic()+ 
  theme(legend.position = "none") + 
  labs(x = "", y = "Citations: All",
       title = "Total Citations \n 3-Month Results", 
       fill = "")+ 
  ylim(0,1)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c( "#A8A8A8","#7AA6CA", obpm_darkblue, lab_green))+
  
  theme(text = element_text(family = "Neutra Text Light Alt",  size = 25))+
  theme(plot.title = element_text(face = "bold"))

ggsave(plot_total_all, file = here("figs/lab-report/total_cites_with_ci_wholesample.png"))

# Regression Tables 


