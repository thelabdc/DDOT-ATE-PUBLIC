
ate <- left_join(citations_3mo, citations_12mo[,c("plate", "num_citations_12mo", "riskiest_citations_12mo")], by = "plate")
ate <- left_join(ate, crash_12mo[,c("plate", "n_crashes")], by = "plate")

# checks 
# table(ate$num_citations_12mo >= ate$num_citations_3mo) # ALL TRUE
# table(ate$riskiest_citations_12mo >= ate$riskiest_citations_3mo) # ALL TRUE

ate$mailer <- ifelse(ate$assignment == "Mailer",1,0)
ate$sms <- ifelse(ate$assignment == "Text",1,0)
ate$both <- ifelse(ate$assignment == "Both",1,0)

# Eliminate "make" categories of make that are too small to get rid of rank deficiency in interacted design matrix 
ate <- ate |> group_by(make_cleaned)|> mutate(make_count = n())|>
  mutate(make_cleaned = ifelse(make_count < 100, "Small Group", make_cleaned))
ate$make_count <- NULL

# Relevel factors to make them more intuitive in regression tables 
#ate$risk_tercile <- factor(ate$risk_tercile, levels = c("Low", "Medium", "High"))
#ate$state_ward <- factor(ate$state_ward, levels = c("DC", "MD", "VA", "Ward 1", "Ward 2", "Ward 3", "Ward 4", "Ward 5", "Ward 6", "Ward 7", "Ward 8"))

# List of Regressions: 
  # A: Risky Citations 3 mo, Mailer 
  # B: Total Citations 3 mo, Mailer
  # C: Risky Citations 3 mo, SMS, Mailer, and Both (Matched sample)
  # D: Total Citations 3 mo, SMS, Mailer, and Both (Matched sample)
  # E: Risky Citations 12 mo, Mailer 
  # F: Total Citations 12 mo, Mailer
  # G: Risky Citations 12 mo, SMS, Mailer, and Both (Matched sample)
  # H: Total Citations 12 mo, SMS, Mailer, and Both (Matched sample)
  # I: Crashes, Mailer
  # J: Crashes, SMS, Mailer, and Both (Matched sample)

################################################################################
# 3 Month Citations -----------------------------------------------------------
################################################################################
# Analyses on the whole sample -------------------------------------------------

# Analysis with Make  (Pre-specified)
A1 <- lm_lin(formula = riskiest_citations_3mo ~ mailer, 
             covariates= ~ risk_tercile + state_ward + match+make_cleaned , data = ate[ate$assignment=="Mailer"|ate$assignment == "Control",])

# Analysis without Make  
A2 <- lm_lin(formula = riskiest_citations_3mo ~ mailer, 
             covariates= ~ risk_tercile + state_ward + match , data = ate[ate$assignment=="Mailer"|ate$assignment == "Control",])

# Analysis with make (Pre-specified)
B1 <- lm_lin(formula = num_citations_3mo ~ mailer, 
             covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate[ate$assignment=="Mailer"|ate$assignment == "Control",])

# Analysis without Make 
B2 <- lm_lin(formula = num_citations_3mo ~ mailer, 
             covariates= ~ risk_tercile + state_ward + match , data = ate[ate$assignment=="Mailer"|ate$assignment == "Control",])


# Analyses on the matched sample only -----------------------------------------
matched <- ate |> filter(match ==1)
# Relevel 
matched$assignment <- factor(matched$assignment,
                             levels = c("Control", "Mailer",  "Text","Both"))
# Analysis with Make (pre-specified)
C1 <- lm_lin(riskiest_citations_3mo ~ assignment, covariates = ~   risk_tercile + state_ward  + make_cleaned, data = matched) 
# Analysis without Make 
C2 <- lm_lin(riskiest_citations_3mo ~ assignment, covariates = ~   risk_tercile + state_ward  , data = matched) 

# Analysis with Make (pre-specified)
D1 <- lm_lin(num_citations_3mo ~ assignment, covariates = ~   risk_tercile + state_ward + make_cleaned, data = matched) 

# Analysis without Make 
D2 <- lm_lin(num_citations_3mo ~ assignment, covariates = ~   risk_tercile + state_ward , data = matched) 

################################################################################
# 12 Month Citations -----------------------------------------------------------
################################################################################
# Analyses on the whole sample -------------------------------------------------
# Analysis with Make  (Pre-specified)
E1 <- lm_lin(formula = riskiest_citations_12mo ~ mailer, 
             covariates= ~ risk_tercile + state_ward + match+make_cleaned , data = ate[ate$assignment=="Mailer"|ate$assignment == "Control",])

# Analysis without Make  
E2 <- lm_lin(formula = riskiest_citations_12mo ~ mailer, 
             covariates= ~ risk_tercile + state_ward + match , data = ate[ate$assignment=="Mailer"|ate$assignment == "Control",])

# Analysis with make (Pre-specified)
F1 <- lm_lin(formula = num_citations_12mo ~ mailer, 
             covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate[ate$assignment=="Mailer"|ate$assignment == "Control",])

# Analysis without Make 
F2 <- lm_lin(formula = num_citations_12mo ~ mailer, 
             covariates= ~ risk_tercile + state_ward + match , data = ate[ate$assignment=="Mailer"|ate$assignment == "Control",])

# Analyses on the matched sample -----------------------------------------------
# Analysis with Make (pre-specified)
G1 <- lm_lin(riskiest_citations_12mo ~ assignment, covariates = ~   risk_tercile + state_ward  + make_cleaned, data = matched) 

# Analysis without Make 
G2 <- lm_lin(riskiest_citations_12mo ~ assignment, covariates = ~   risk_tercile + state_ward  , data = matched) 

# Analysis with Make (pre-specified)
H1 <- lm_lin(num_citations_12mo ~ assignment, covariates = ~   risk_tercile + state_ward + make_cleaned, data = matched) 

# Analysis without Make 
H2 <- lm_lin(num_citations_12mo ~ assignment, covariates = ~   risk_tercile + state_ward , data = matched) 


################################################################################
# Crashes (12 month) -----------------------------------------------------------
################################################################################
# Analyses on the whole sample -------------------------------------------------
# Analysis with Make  (Pre-specified)
I1 <- lm_lin(formula = n_crashes ~ mailer, 
             covariates= ~ risk_tercile + state_ward + match+make_cleaned , data = ate[ate$assignment=="Mailer"|ate$assignment == "Control",])

# Analysis without Make  
I2 <- lm_lin(formula = n_crashes ~ mailer, 
             covariates= ~ risk_tercile + state_ward + match , data = ate[ate$assignment=="Mailer"|ate$assignment == "Control",])

# Analyses on the matched sample -----------------------------------------------
# Analysis with Make (pre-specified)
J1 <- lm_lin(n_crashes ~ assignment, covariates = ~   risk_tercile + state_ward  + make_cleaned, data = matched) 

# Analysis without Make 
J2 <- lm_lin(n_crashes ~ assignment, covariates = ~   risk_tercile + state_ward  , data = matched) 


################################################################################
# Regression tables and plots ------------------------------------------------------------
################################################################################
p_val_simulation_citations <- 0.004704705
p_val_simulation_crash <- 0.0171

# Create a tidy tibble to store results in
results_tibble <- tibble("model" = c(),
                         "estimate" = c(),
                         "std.error" = c(),
                         "statistic" = c(),
                         "p.value" = c(),
                         "conf.low" = c(), 
                         "conf.high"= c(), 
                         "df" = c(), 
                         "outcome" = c())              
# Adapt formulas to the appropriate subset condition and run regression
models <- c(A1, B1, C1, D1, E1, F1, G1, H1, I1, J1)
results_tibble <- bind_rows(tidy(A1, conf.level = 1- p_val_simulation_citations)|> mutate(model_name = "A1"),
                            tidy(B1, conf.level = 1- p_val_simulation_citations)|> mutate(model_name = "B1"),
                            tidy(C1, conf.level = 1- p_val_simulation_citations)|> mutate(model_name = "C1"),
                            tidy(D1, conf.level = 1- p_val_simulation_citations)|> mutate(model_name = "D1"),
                            tidy(E1, conf.level = 1- p_val_simulation_citations)|> mutate(model_name = "E1"),
                            tidy(F1, conf.level = 1- p_val_simulation_citations)|> mutate(model_name = "F1"),
                            tidy(G1, conf.level = 1- p_val_simulation_citations)|> mutate(model_name = "G1"),
                            tidy(H1, conf.level = 1- p_val_simulation_citations)|> mutate(model_name = "H1"),
                            tidy(I1, conf.level = 1- p_val_simulation_crash)|> mutate(model_name = "I1"),
                            tidy(J1, conf.level = 1- p_val_simulation_crash)|> mutate(model_name = "J1")
) |>
  filter(term %in% c("any", "mailer", "assignmentMailer", "assignmentText", "assignmentBoth", "(Intercept)"))|>
  mutate(outcome = ifelse(outcome == "n_crashes", "n_crashes_12mo", outcome))

write.csv(results_tibble, file = "data/regression_results/results_all_confirmatory.csv")

# -----------------------------------------------------------------------------
# Box and Whisker Plots 
# -----------------------------------------------------------------------------
# Main coefficient plot for whole sample results 
mean_risky <- round(mean(ate[ate$assignment == "Control",]$riskiest_citations_12mo),2)
mean_total <- round(mean(ate[ate$assignment == "Control",]$num_citations_12mo),2)
mean_crash <- round(mean(ate[ate$assignment == "Control",]$n_crashes),2)

 (results_tibble |>
  filter(term == "mailer") |>
  mutate(outcome_name = str_split_fixed(outcome, "(?=\\d)",2)[,1],
         period = str_split_fixed(outcome, "(?=\\d)",2)[,2]) |>
     mutate(period = factor(period, levels = c( "3mo", "12mo"), 
                            labels = c( "3 months","12 months")))|>
     mutate(term = factor(term, levels = c("mailer"), labels = c("Letter Only")))|>
     mutate(outcome_name = factor(outcome_name, 
                                  levels = c( "n_crashes_","riskiest_citations_", "num_citations_"), 
                                  labels = c( "Crashes", "Riskiest citations", 
                                              "Total citations" 
                                  ))
     )|>
  ggplot() + 
  geom_point(aes(x = term, y= estimate, group = outcome_name, color = outcome_name), position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(x = term, ymin = conf.low, ymax = conf.high, group = outcome_name, color = outcome_name), position=position_dodge(width=0.5), width = .5) + 
  geom_hline(aes(yintercept = 0), color = cb_red, linetype = "dashed")+
  coord_flip()+
  facet_wrap(~ period, ncol = 2) + 
  labs(#title = "Effect of Letter Intervention in Full Sample",
    x = "", y = "Estimate", color = "Outcome\n(12-month control mean)",
       caption = paste("Coefficient estimates of effect of \"letter only\" intervention in full sample, using Lin Estimator. N = ", nobs(A1), "\n Confidence intervals use p-values corrected for multiple hypothesis testing."), sep = "") + 
  theme_classic() + 
    scale_color_manual(values = c(  cb_red,cb_purple,cb_green))+
     theme(plot.title =element_text(size = 10),
           axis.title = element_text(size = 10),
           axis.text = element_text(size = 10),
           plot.caption = element_text(size = 10),
           legend.title = element_text(size =10),
           legend.text = element_text(size = 10),
           strip.text = element_text(size = 10))+
   #theme(
    #   legend.position = c(0.5, 0.885),  # Centered and above the facets
     #  legend.justification = c(0.5, 0.5),
      # legend.direction = "horizontal",
       #legend.background = element_rect(fill = "white"),
       #legend.box.background = element_blank(),
       #legend.box.margin = margin(t = -10)  # Adjust this to control spacing
     #) 
     theme(legend.position = "bottom")
     
    )|>
  ggsave(file = "figs/2024-12/confirmatory_mailer.png", width= 6.9, height = 4)

# Main coefficient plot for matched sample (By treatment arm) 
mean_risky_match <- round(mean(ate[ate$assignment == "Control" & ate$match==1,]$riskiest_citations_12mo),2)
mean_total_match <- round(mean(ate[ate$assignment == "Control"& ate$match==1,]$num_citations_12mo),2)
mean_crash_match <- round(mean(ate[ate$assignment == "Control"& ate$match==1,]$n_crashes),2)

(results_tibble |>
    filter(str_detect(term, "assignment")==TRUE)  |>
    mutate(term = factor(term,
                         levels = c("assignmentBoth","assignmentText", "assignmentMailer"),
                         labels = c( "Both", "Text Message","Letter" ))) |>
    mutate(outcome_name = str_split_fixed(outcome, "(?=\\d)",2)[,1],
           period = str_split_fixed(outcome, "(?=\\d)",2)[,2]) |>
    mutate(period = factor(period, levels = c( "3mo","12mo"), 
                           labels = c( "3 months","12 months")))|>

    mutate(outcome_name = factor(outcome_name, 
                                 levels = c( "n_crashes_","riskiest_citations_", "num_citations_"), 
                                 labels = c( "Crashes", "Riskiest citations", 
                                             "Total citations" 
                                 ))
    )|>
  ggplot() + 
  geom_point(aes(x = term, y= estimate, group = outcome_name, color = outcome_name), position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(x = term, ymin = conf.low, ymax = conf.high, group = outcome_name, color = outcome_name),  position=position_dodge(width=0.5), width = .5)+
  facet_wrap(~ period, ncol = 3) +
  coord_flip()+
  geom_hline(aes(yintercept = 0), color = cb_red, linetype = "dashed")+
  labs(#title = "Effect of Different Messaging Modes Among Drivers with Publicly-Available Phone Numbers",
       x = "", y = "Estimate", 
       color = "Outcome\n(12-month control mean)",
       caption = paste("Coefficient estimates of effects in phone-matched sample, using Lin Estimator. N = ", nobs(C1), "\n Confidence intervals use p-values corrected for multiple hypothesis testing."), sep = "") + 
  theme_classic()  + 
    theme_classic() + 
    scale_color_manual(values = c(  cb_red,cb_purple,cb_green))+
    theme(plot.title =element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.caption = element_text(size = 10),
          legend.title = element_text(size =10),
          legend.text = element_text(size = 10))+
   # theme(
    #  legend.position = c(0.5, 0.95),  # Centered and above the facets
     # legend.justification = c(0.5, 0.5),
      #legend.direction = "horizontal",
      #legend.background = element_rect(fill = "white"),
      #legend.box.background = element_blank(),
      #legend.box.margin = margin(t = -10)  # Adjust this to control spacing
    #) 
    theme(legend.position = "bottom")
)|>
  ggsave(file = "figs/2024-12/confirmatory_matched.png", width= 6.9, height = 8)

# -----------------------------------------------------------------------------
# Regression Tables
# -----------------------------------------------------------------------------

# Whole sample results (just mailer)
extracted_A1 <- texreg::extract(A1,  include.ci=FALSE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
extracted_B1 <- texreg::extract(B1,include.ci=FALSE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
extracted_C1 <- texreg::extract(C1,include.ci=FALSE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
extracted_D1 <- texreg::extract(D1,include.ci=FALSE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
extracted_E1 <- texreg::extract(E1,include.ci=FALSE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
extracted_F1 <- texreg::extract(F1,include.ci=FALSE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
extracted_G1 <- texreg::extract(G1,include.ci=FALSE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
extracted_H1 <- texreg::extract(H1,include.ci=FALSE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
extracted_I1 <- texreg::extract(I1,include.ci=FALSE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
extracted_J1 <- texreg::extract(J1,include.ci=FALSE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)

output <- wordreg(c(extracted_A1,   extracted_E1, extracted_B1, extracted_F1, extracted_I1),
                 caption = "Whole Sample Results: (Mailer vs. No Message)",
                 stars = c(p_val_simulation_crash,p_val_simulation_citations),
                 custom.model.names = c("3 months","12 months",  "3 months", "12 months", "12 months"),
                 #custom.header  = list("Risky Citations" = 1:2, "All citations" = 3:4, "Crashes" = 5),
                 digits = 4,
                 use.packages = FALSE,
                 threeparttable = TRUE,
                 custom.coef.map = list("mailer" = "Letter only", 
                                        "risk_tercileMedium_c" = "Medium", 
                                        "risk_tercileLow_c" = "Low",
                                        "state_wardMD_c" = "Maryland",
                                        "state_wardVA_c"= "Virginia",
                                        "state_wardWard 1_c" = "Ward 1",
                                        "state_wardWard 2_c" = "Ward 2",
                                        "state_wardWard 3_c" = "Ward 3",
                                        "state_wardWard 4_c" = "Ward 4",
                                        "state_wardWard 5_c"= "Ward 5",
                                        "state_wardWard 6_c" = "Ward 6",
                                        "state_wardWard 7_c" = "Ward 7",
                                        "state_wardWard 8_c" = "Ward 8",
                                        "state_wardOther_c" = "Other",
                                        "match" = "Phone Match Sample",
                                        "(Intercept)" = "Constant"),
                 groups = list("Risk Tercile" = 2:3,"State or Ward" = 4:14),
                 custom.note = c("\\item Lin Estimator. Car make and interacted coefficients on covariates omitted from display for brevity.
                             \\item Stars: * < .017 (Simulated crash p-value) ** < .004 (Simulated citation p-value) "),
                 caption.above = TRUE, 
                 file =  "tables/2024-10/Confirmatory/confirmatory_wholesample.doc"
)




# Matched sample 
output_matched <- wordreg(c(extracted_C1,  extracted_G1, extracted_D1, extracted_H1, extracted_J1),
                 omit.coef =  c(":|make"),
                 threeparttable = TRUE,
                 use.packages = FALSE,
                 caption = "Matched Sample Results",
                 digits = 4,
                 stars = c(p_val_simulation_crash,p_val_simulation_citations),
                 custom.model.names = c("3 months","12 months",  "3 months", "12 months", "12 months"),
                 #custom.header  = list("Risky Citations" = 1:2, "All citations" = 3:4, "Crashes" = 5),
                 groups = list("Treatment" = 1:3, 
                             "Risk Tercile" = 4:5,
                               "State or Ward" = 6:16),
                    caption.above = TRUE,
                 custom.coef.map = list("assignmentMailer" = "Letter only", 
                                        "assignmentText" = "Text message only",
                                        "assignmentBoth" = "Both",
                                        "risk_tercileMedium_c" = "Medium", 
                                        "risk_tercileLow_c" = "Low",
                                        "state_wardMD_c" = "Maryland",
                                        "state_wardVA_c"= "Virginia",
                                        "state_wardWard 1_c" = "Ward 1",
                                        "state_wardWard 2_c" = "Ward 2",
                                        "state_wardWard 3_c" = "Ward 3",
                                        "state_wardWard 4_c" = "Ward 4",
                                        "state_wardWard 5_c"= "Ward 5",
                                        "state_wardWard 6_c" = "Ward 6",
                                        "state_wardWard 7_c" = "Ward 7",
                                        "state_wardWard 8_c" = "Ward 8",
                                        "state_wardOther_c" = "Other",
                                        "(Intercept)" = "Constant"),
                 
                 #custom.coef.names = c("\\textbf{Mailer}", "\\textbf{Text Message}", "\\textbf{Both}"),
       
                 custom.note = c("\\item Lin Estimator. Car make, intercept and interacted coefficients on covariates omitted from display for brevity.
                             \\item Stars: * < .017 (Simulated crash p-value) ** < .004 (Simulated citation p-value) "),
                 file = "tables/2024-10/Confirmatory/confirmatory_matched_sample.docx"
)
