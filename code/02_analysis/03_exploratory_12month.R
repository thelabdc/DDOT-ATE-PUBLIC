# Exploratory 
# source("code/04-evaluation/02_analysis/00_master.R")
# source("code/04-evaluation/02_analysis/01_analysis_confirmatory.R")


# Create an indicator for "any treatment" 
ate$any <- ifelse(ate$assignment %in% c("Mailer", "Text", "Both"),1,0)
matched$any <- ifelse(matched$assignment %in% c("Mailer", "Text", "Both"),1,0)

# Function to re-run main analyses for any subset of the data

# Nice formatting of conditions 
conditions <- tibble(condition = c("state_ward == 'VA'", 
                                   "state_ward == 'MD'",
                     "state_ward %in% c('DC', 'Ward 1','Ward 2', 'Ward 3', 'Ward 4', 'Ward 5', 'Ward 6', 'Ward 7', 'Ward 8')",
                     "risk_tercile == 'High'","risk_tercile == 'Low'", "risk_tercile == 'Medium'"),
                     pretty = c("Virginia",  "Maryland",  "DC",  "High-Risk", "Low-Risk", "Medium-Risk"))

run_ate_regressions <- function(subset_condition = "state_ward == 'VA'", 
                                drop_make = FALSE
                                ){
  condition_pretty <- pull(conditions[match(subset_condition, conditions$condition ), "pretty"])
  ate_subset <- ate |> 
    filter(eval(parse(text = subset_condition)))
  matched_subset <- matched |> 
    filter(eval(parse(text = subset_condition)))
  
  formulas <- list(A1 = "lm_lin(formula = riskiest_citations_3mo ~ mailer,  covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate_subset[ate_subset$assignment=='Mailer'|ate_subset$assignment == 'Control',])",
                   B1 = "lm_lin(formula = num_citations_3mo ~ mailer, covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate_subset[ate_subset$assignment=='Mailer'|ate_subset$assignment == 'Control',])",
                   C1 = "lm_lin(riskiest_citations_3mo ~ assignment, covariates = ~   risk_tercile + state_ward + make_cleaned, data = matched_subset)",
                   D1 = "lm_lin(num_citations_3mo ~ assignment, covariates = ~   risk_tercile + state_ward + make_cleaned, data = matched_subset)",
                   E1 = "lm_lin(formula = riskiest_citations_12mo ~ mailer,  covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate_subset[ate_subset$assignment=='Mailer'|ate_subset$assignment == 'Control',])",
                   F1 = "lm_lin(formula = num_citations_12mo ~ mailer,  covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate_subset[ate_subset$assignment=='Mailer'|ate_subset$assignment == 'Control',])",
                   G1 = "lm_lin(riskiest_citations_12mo ~ assignment, covariates = ~   risk_tercile + state_ward + make_cleaned, data = matched_subset)", 
                   H1 = "lm_lin(num_citations_12mo ~ assignment, covariates = ~   risk_tercile + state_ward + make_cleaned, data = matched_subset)",
                   I1 = "lm_lin(formula = n_crashes ~ mailer,  covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate_subset[ate_subset$assignment=='Mailer'|ate_subset$assignment == 'Control',])",
                   J1 = "lm_lin(n_crashes ~ assignment, covariates = ~   risk_tercile + state_ward + make_cleaned, data = matched_subset)",
   # Regressions with any treatment as treatment (uses full sample)
                   K1 = "lm_lin(formula = riskiest_citations_3mo ~ any,  covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate_subset)",
                   L1 = "lm_lin(formula = num_citations_3mo ~ any,  covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate_subset)",
                   M1 = "lm_lin(formula = riskiest_citations_12mo ~ any,  covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate_subset)",
                   N1 = "lm_lin(formula = num_citations_12mo ~ any,  covariates= ~ risk_tercile + state_ward + match + make_cleaned, data = ate_subset)",
                   O1 = "lm_lin(n_crashes ~ any, covariates = ~   risk_tercile + state_ward + make_cleaned, data = ate_subset)"
      )
  
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
  for(i in 1:length(formulas)){
  # Edit the formulas to make sure that we're not including the variable we subsetted on in the condition
  if(str_detect(subset_condition, "state_ward")==TRUE){
    formulas[i] <- str_replace(formulas[i], "state_ward \\+", "")
  }else{
  if(str_detect(subset_condition, "risk_tercile")==TRUE){
    formulas[i] <- str_replace(formulas[i], "risk_tercile \\+", "")
  }
  }
  
  # If drop_make = TRUE, we remove make from the equation to deal with the rank deficiency problem 
    if(drop_make == TRUE){
      formulas[i] <- gsub(x = formulas[i], pattern ="\\+ make_cleaned|\\+  make_cleaned|\\+   make_cleaned", replacement = " ")
    }
      
  # Evaluate each of the regressions 
  model <- eval(parse(text = formulas[i]))
  model_name <- unlist(names(formulas)[i])
  
  # Store the coefficients and SE's on the treatment variables
  model_results <- model |> 
    tidy() |>
    filter(term %in% c("any", "mailer", "assignmentMailer", "assignmentText", "assignmentBoth", "(Intercept)"))|>
    mutate(model = model_name)
  results_tibble <- bind_rows(results_tibble, model_results)
  
  assign(model_name,model)
  }

  # Whole sample results (just mailer)
  # Note that these are conventional 95% confidence intervals (without multiple hypothesis corrections)
  extracted_A1 <- texreg::extract(A1,  include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_B1 <- texreg::extract(B1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_C1 <- texreg::extract(C1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_D1 <- texreg::extract(D1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_E1 <- texreg::extract(E1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_F1 <- texreg::extract(F1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_G1 <- texreg::extract(G1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_H1 <- texreg::extract(H1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_I1 <- texreg::extract(I1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_J1 <- texreg::extract(J1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_K1 <- texreg::extract(K1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_L1 <- texreg::extract(L1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_M1 <- texreg::extract(M1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_N1 <- texreg::extract(N1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  extracted_O1 <- texreg::extract(O1,include.ci=TRUE, include.adjrs = FALSE, include.rmse=FALSE, include.fstatistic = FALSE)
  
  output <- texreg(c(extracted_A1,  extracted_E1, extracted_B1, extracted_F1, extracted_I1),
                   omit.coef =  c(":|make|state_ward|risk_tercile|match"),
                   caption = paste("Whole Sample Results: (Mailer vs. No Message): ", condition_pretty, sep = ""),
                   stars = c(0.1, 0.05, 0.01),
                   custom.model.names = c("3 months","12 months",  "3 months", "12 months", "12 months"),
                   custom.header  = list("Risky Citations" = 1:2, "All citations" = 3:4, "Crashes" = 5),
                   use.packages = FALSE,
                   custom.coef.names = c("Intercept", "Mailer"),
                   threeparttable = TRUE,
                   custom.note = c("\\item Lin Estimator. Car make and interacted coefficients on covariates omitted from display for brevity.
                             \\item Stars and confidence intervals reflect conventional 95\\% p-value"),
                   caption.above = TRUE
  )
  
  cat(output, file = paste("tables/het/wholesample_", condition_pretty, ".tex", sep = ""))
  
  
  # Matched sample 
  output_matched <- texreg(c(extracted_C1,  extracted_G1, extracted_D1, extracted_H1, extracted_J1),
                           omit.coef =  c(":|make|state_ward|risk_tercile|match"),
                           threeparttable = TRUE,
                           use.packages = FALSE,
                           caption = paste("Matched Sample Results: ", condition_pretty, sep = ""),
                           stars = c(0.1, 0.05, 0.01),
                           custom.model.names = c("3 months","12 months",  "3 months", "12 months", "12 months"),
                           custom.header  = list("Risky Citations" = 1:2, "All citations" = 3:4, "Crashes" = 5),
                           custom.coef.names = c("Intercept", "Mailer", "Text", "Both"),
                           custom.note = c("\\item Lin Estimator. Car make and interacted coefficients on covariates omitted from display for brevity.
                             \\item Stars and confidence intervals reflect conventional 95\\% p-value"),
                           caption.above = TRUE,
                           
                           
                        )
                         
  cat(output_matched, file = paste("tables/het/matched_sample_", condition_pretty, ".tex", sep = ""))
  
  # Matched sample 
  output_any <- texreg(c(extracted_K1,  extracted_M1, extracted_L1, extracted_N1, extracted_O1),
                           omit.coef =  c(":|make|state_ward|risk_tercile|match"),
                           threeparttable = TRUE,
                           use.packages = FALSE,
                           caption = paste("Any Message Results: ", condition_pretty, sep = ""),
                           stars = c(0.1, 0.05, 0.01),
                           custom.model.names = c("3 months","12 months",  "3 months", "12 months", "12 months"),
                           custom.header  = list("Risky Citations" = 1:2, "All citations" = 3:4, "Crashes" = 5),
                       custom.note = c("\\item Lin Estimator. Car make and interacted coefficients on covariates omitted from display for brevity.
                             \\item Stars and confidence intervals reflect conventional 95\\% p-value"),
                       custom.coef.names = c("(Intercept)", "Any Message"),
                       caption.above = TRUE
  )
  
  cat(output_any, file = paste("tables/het/any_treatment_", condition_pretty, ".tex", sep = ""))

  
  return(results_tibble)
}

results_va <- run_ate_regressions(subset_condition = "state_ward == 'VA'", 
                                drop_make = TRUE) |>
                                mutate(sample = "Virginia")
results_md <- run_ate_regressions(subset_condition = "state_ward == 'MD'", 
                                  drop_make = TRUE)|> mutate(sample= "Maryland")
results_dc <- run_ate_regressions(subset_condition = "state_ward %in% c('Ward 1','Ward 2', 'Ward 3', 'Ward 4', 'Ward 5', 'Ward 6', 'Ward 7', 'Ward 8')", 
                                  drop_make = TRUE)|>
                                  mutate(sample = "DC")

results_high_risk <- run_ate_regressions("risk_tercile == 'High'", 
                                  drop_make = TRUE)|>
                                  mutate(sample = "Highest risk")
results_medium_risk <- run_ate_regressions(subset_condition = "risk_tercile == 'Medium'", 
                                  drop_make = TRUE)|>
                                  mutate(sample = "Medium risk")
results_low_risk <- run_ate_regressions(subset_condition = "risk_tercile == 'Low'", 
                                   drop_make = TRUE)|>
                                   mutate(sample = "Low risk")

write.csv(results_dc, file = "tables/het/results_dc.csv")
write.csv(results_md, file = "tables/het/results_md.csv")
write.csv(results_va, file = "tables/het/results_va.csv")
write.csv(results_high_risk, file = "tables/het/results_high_risk.csv")
write.csv(results_medium_risk, file = "tables/het/results_medium_risk.csv")
write.csv(results_low_risk, file = "tables/het/results_low_risk.csv")

# -----------------------------------------------------------------------------
# Box and Whisker Plots 
# -----------------------------------------------------------------------------
#  State-Level Mailer (Whole Sample)--------------------------------------------
n_va_mailer <- nrow(ate[ate$state_ward== "VA" & ate$assignment %in% c("Control", "Mailer"),])
n_md_mailer <- nrow(ate[ate$state_ward== "MD"& ate$assignment %in% c("Control", "Mailer"),])
n_dc_mailer <- nrow(ate[ate$state_ward %in% c('DC', 'Ward 1','Ward 2', 'Ward 3', 'Ward 4', 'Ward 5', 'Ward 6', 'Ward 7', 'Ward 8')& ate$assignment %in% c("Control", "Mailer"),])

results_state_mailer <- bind_rows(results_dc, results_md, results_va)|>
  filter(term == 'mailer' )|>
  mutate(outcome = ifelse(outcome == "n_crashes", "n_crashes_12mo", outcome))|>
  mutate(sample = factor(sample, levels = c("DC", "Maryland", "Virginia"), 
                         labels = c(paste("DC: \n N = ", n_dc_mailer, sep = ""),
                                    paste("MD:\n N = ", n_md_mailer, sep = ""),
                                    paste("VA:\n N = ", n_va_mailer, sep = ""))))

(results_state_mailer |>
    mutate(outcome_name = str_split_fixed(outcome, "(?=\\d)",2)[,1],
           period = str_split_fixed(outcome, "(?=\\d)",2)[,2]) |>
    mutate(period = factor(period, levels = c( "3mo", "12mo"), 
                           labels = c( "3 months","12 months")))|>
    mutate(outcome_name = factor(outcome_name, 
                                 levels = c( "n_crashes_","riskiest_citations_", "num_citations_"), 
                                 labels = c( "Crashes", "Riskiest citations", 
                                             "Total citations" 
                                 ))
    )|>
    #labels = c(paste("Riskiest Citations:\n 12-month control mean = ", mean_risky, sep = ""),
    #          paste("Total Citations:\n 12-month control mean = ", mean_total, sep = ""),
    #         paste("Crashes:\n 12-month control mean = ", mean_crash, sep = "" ))))|>
    ggplot() + 
    geom_point(aes(x = sample, y= estimate, group = outcome_name, color = outcome_name), position=position_dodge(width=0.5)) + 
    geom_errorbar(aes(x = sample, ymin = conf.low, ymax = conf.high, group = outcome_name, color = outcome_name),  position=position_dodge(width=0.5), width = .5)+
    facet_wrap(~ period, ncol = 3) +
    coord_flip()+
    geom_hline(aes(yintercept = 0), color = cb_red, linetype = "dashed")+
    labs(#title = "Exploratory Analysis: Effect of mailer by vehicle registration state (Whole Sample)",
         x = "", y = "Estimate", 
         color = "Outcome",
         caption = paste("Coefficient estimates of effect of letter intervention in whole sample, using Lin Estimator.\n Confidence intervals for exploratory outcomes show conventional 95% confidence thresholds."), sep = "") + 
    theme_classic()  + 
    scale_color_manual(values = c(  cb_red,cb_purple,cb_green))+
    theme(plot.title =element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.caption = element_text(size = 10),
          legend.title = element_text(size =10),
          legend.text = element_text(size = 10),
          strip.text = element_text(size = 10))+
    theme(legend.position = "bottom") # Adjust this to control spacing
)|>
  ggsave(file = "figs/exploratory_state_mailer.png", width= 6.9, height = 5)

# State-Level, Matched sample -------------------------------------------------
n_va_matched <- nrow(ate[ate$state_ward== "VA" & ate$match==1,])
n_md_matched <- nrow(ate[ate$state_ward== "MD"& ate$match==1,])
n_dc_matched <- nrow(ate[ate$state_ward %in% c('DC', 'Ward 1','Ward 2', 'Ward 3', 'Ward 4', 'Ward 5', 'Ward 6', 'Ward 7', 'Ward 8')& ate$match==1,])

results_state_matched <- bind_rows(results_dc, results_md, results_va)|>
  filter(str_detect(term, 'assignment')==TRUE) |>
  mutate(outcome = ifelse(outcome == "n_crashes", "n_crashes_12mo", outcome))|>
  mutate(sample = factor(sample, levels = c("DC", "Maryland", "Virginia"), 
                         labels = c(paste("DC: \n N = ", n_dc_matched, sep = ""),
                                    paste("MD:\n N = ", n_md_matched, sep = ""),
                                    paste("VA:\n N = ", n_va_matched, sep = ""))))|>
  mutate(term = factor(term,
                       levels = c("assignmentMailer", "assignmentText", "assignmentBoth"),
                       labels = c("Letter", "Text Message", "Both")))

(results_state_matched |>
    mutate(outcome_name = str_split_fixed(outcome, "(?=\\d)",2)[,1],
           period = str_split_fixed(outcome, "(?=\\d)",2)[,2]) |>
    mutate(period = factor(period, levels = c( "3mo", "12mo"), 
                           labels = c( "3 months","12 months")))|>
    mutate(outcome_name = factor(outcome_name, 
                                 levels = c( "n_crashes_","riskiest_citations_", "num_citations_"), 
                                 labels = c( "Crashes", "Riskiest citations", 
                                             "Total citations" 
                                 ))
    )|>
    ggplot() + 
    geom_point(aes(x = sample, y= estimate, group = outcome_name, color = outcome_name), position=position_dodge(width=0.5)) + 
    geom_errorbar(aes(x = sample, ymin = conf.low, ymax = conf.high, group = outcome_name, color = outcome_name),  position=position_dodge(width=0.5), width = .5)+
    facet_wrap(~term + period, ncol = 2) +
    coord_flip()+
    geom_hline(aes(yintercept = 0), color = cb_red, linetype = "dashed")+
    labs(#title = "Exploratory Analysis: Effect of messages by vehicle registration state (Phone-Matched Sample)",
         x = "", y = "Estimate", 
         color = "Outcome",
         caption = paste("Coefficient estimates of effect of messaging interventions in phone-matched sample, using Lin Estimator.\n Confidence intervals for exploratory outcomes show conventional 95% confidence thresholds."), sep = "") + 
    theme_classic()  + 
    scale_color_manual(values = c(  cb_red,cb_purple,cb_green))+
    theme(plot.title =element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.caption = element_text(size = 10),
          legend.title = element_text(size =10),
          legend.text = element_text(size = 10),
          strip.text = element_text(size = 10))+
    theme(legend.position = "bottom") # Adjust this to control spacing
)|>
  ggsave(file = "figs/exploratory_state_matched.png", width= 6.9, height = 8)

# State-Level Any Discount ----------------------------------------------------
# Bind all state-level results
n_va_any <- nrow(ate[ate$state_ward== "VA",])
n_md_any <- nrow(ate[ate$state_ward== "MD",])
n_dc_any <- nrow(ate[ate$state_ward %in% c('DC', 'Ward 1','Ward 2', 'Ward 3', 'Ward 4', 'Ward 5', 'Ward 6', 'Ward 7', 'Ward 8'),])

# Any discount 
results_state_any <- bind_rows(results_dc, results_md, results_va)|>
  filter(term == 'any')|>
  mutate(outcome = ifelse(outcome == "n_crashes", "n_crashes_12mo", outcome))|>
  mutate(sample = factor(sample, levels = c("DC", "Maryland", "Virginia"), 
                         labels = c(paste("DC: \n N = ", n_dc_any, sep = ""),
                                    paste("MD:\n N = ", n_md_any, sep = ""),
                                    paste("VA:\n N = ", n_va_any, sep = ""))))

(results_state_any |>
    mutate(outcome_name = str_split_fixed(outcome, "(?=\\d)",2)[,1],
           period = str_split_fixed(outcome, "(?=\\d)",2)[,2]) |>
    mutate(period = factor(period, levels = c( "3mo", "12mo"), 
                           labels = c( "3 months","12 months")))|>
    mutate(outcome_name = factor(outcome_name, 
                                 levels = c( "n_crashes_","riskiest_citations_", "num_citations_"), 
                                 labels = c( "Crashes", "Riskiest citations", 
                                             "Total citations" 
                                 ))
    )|>
    #labels = c(paste("Riskiest Citations:\n 12-month control mean = ", mean_risky, sep = ""),
    #          paste("Total Citations:\n 12-month control mean = ", mean_total, sep = ""),
    #         paste("Crashes:\n 12-month control mean = ", mean_crash, sep = "" ))))|>
    ggplot() + 
    geom_point(aes(x = sample, y= estimate, group = outcome_name, color = outcome_name), position=position_dodge(width=0.5)) + 
    geom_errorbar(aes(x = sample, ymin = conf.low, ymax = conf.high, group = outcome_name, color = outcome_name),  position=position_dodge(width=0.5), width = .5)+
    facet_wrap(~ period, ncol = 3) +
    coord_flip()+
    geom_hline(aes(yintercept = 0), color = cb_red, linetype = "dashed")+
    labs(#title = "Exploratory Analysis: Effect of any message by vehicle registration state",
      x = "", y = "Estimate", 
      color = "Outcome",
      caption = paste("Coefficient estimates of effect of any messaging intervention in whole sample, using Lin Estimator.\n Confidence intervals for exploratory outcomes show conventional 95% confidence thresholds."), sep = "") + 
    theme_classic()  + 
    scale_color_manual(values = c(  cb_red,cb_purple,cb_green))+
    theme(plot.title =element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.caption = element_text(size = 10),
          legend.title = element_text(size =10),
          legend.text = element_text(size = 10),
          strip.text = element_text(size = 10))+
    theme(legend.position = "bottom")) |>
  ggsave(file = "figs/exploratory_state_any.png", width= 6.9, height = 5)

################################################################################
# Risk Tercile -----------------------------------------------------------------
################################################################################
# Mailer (Whole Sample) by Risk Tercile 
n_high_risk_mailer <- nrow(ate[ate$risk_tercile == "High" & ate$assignment %in% c("Control", "Mailer"),])
n_medium_risk_mailer <- nrow(ate[ate$risk_tercile== "Medium"& ate$assignment %in% c("Control", "Mailer"),])
n_low_risk_mailer <- nrow(ate[ate$risk_tercile == "Low"& ate$assignment %in% c("Control", "Mailer"),])

results_risk_tercile <- bind_rows(results_high_risk, results_medium_risk, results_low_risk)|>
  filter(term == 'mailer')|>
  mutate(outcome = ifelse(outcome == "n_crashes", "n_crashes_12mo", outcome))|>
  mutate(sample = factor(sample, levels = c("Highest risk", "Medium risk", "Low risk"), 
                         labels = c(paste("Highest risk: \n N = ", n_high_risk_mailer, sep = ""),
                                    paste("Medium risk:\n N = ", n_medium_risk_mailer, sep = ""),
                                    paste("Lower risk:\n N = ", n_low_risk_mailer, sep = ""))))

(results_risk_tercile |>
    mutate(outcome_name = str_split_fixed(outcome, "(?=\\d)",2)[,1],
           period = str_split_fixed(outcome, "(?=\\d)",2)[,2]) |>
    mutate(period = factor(period, levels = c( "3mo", "12mo"), 
                           labels = c( "3 months","12 months")))|>
    mutate(outcome_name = factor(outcome_name, 
                                 levels = c( "n_crashes_","riskiest_citations_", "num_citations_"), 
                                 labels = c( "Crashes", "Riskiest citations", 
                                             "Total citations" )
                                 ))|>
    ggplot() + 
    geom_point(aes(x = sample, y= estimate, group = outcome_name, color = outcome_name), position=position_dodge(width=0.5)) + 
    geom_errorbar(aes(x = sample, ymin = conf.low, ymax = conf.high, group = outcome_name, color = outcome_name),  position=position_dodge(width=0.5), width = .5)+
    facet_wrap(~ period, ncol = 3) +
    coord_flip()+
    geom_hline(aes(yintercept = 0), color = cb_red, linetype = "dashed")+
    labs(
         x = "", y = "Estimate", 
         color = "Outcome",
         caption = paste("Coefficient estimates of effect of letter intervention in whole sample, using Lin Estimator.\n Confidence intervals for exploratory outcomes show conventional 95% confidence thresholds."), sep = "") + 
    theme_classic()  + 
    scale_color_manual(values = c(  cb_red,cb_purple,cb_green))+
    theme(plot.title =element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.caption = element_text(size = 10),
          legend.title = element_text(size =10),
          legend.text = element_text(size = 10),
          strip.text = element_text(size = 10))+
    theme( legend.position = "bottom")
  )|>
      ggsave(file = "figs/exploratory_risk_tercile_mailer.png", width= 6.9, height = 5)


# Matched Sample by Risk Tercile ------------------------------------------------
n_high_risk_match <- nrow(ate[ate$risk_tercile == "High" & ate$match==1,])
n_medium_risk_match <- nrow(ate[ate$risk_tercile== "Medium"& ate$match==1,])
n_low_risk_match <- nrow(ate[ate$risk_tercile == "Low"& ate$match==1,])

results_risk_tercile <- bind_rows(results_high_risk, results_medium_risk, results_low_risk)|>
  filter(str_detect(term, 'assignment')==TRUE) |>
  mutate(outcome = ifelse(outcome == "n_crashes", "n_crashes_12mo", outcome))|>
  mutate(sample = factor(sample, levels = c("Highest risk", "Medium risk", "Low risk"), 
                         labels = c(paste("Highest risk: \n N = ", n_high_risk_match, sep = ""),
                                    paste("Medium risk:\n N = ", n_medium_risk_match, sep = ""),
                                    paste("Lower risk:\n N = ", n_low_risk_match, sep = ""))))|>
  mutate(term = factor(term,
                       levels = c("assignmentMailer", "assignmentText", "assignmentBoth"),
                       labels = c("Letter", "Text Message", "Both")))

(results_risk_tercile |>
    mutate(outcome_name = str_split_fixed(outcome, "(?=\\d)",2)[,1],
           period = str_split_fixed(outcome, "(?=\\d)",2)[,2]) |>
    mutate(period = factor(period, levels = c( "3mo", "12mo"), 
                           labels = c( "3 months","12 months")))|>
    mutate(outcome_name = factor(outcome_name, 
                                 levels = c( "n_crashes_","riskiest_citations_", "num_citations_"), 
                                 labels = c( "Crashes", "Riskiest citations", 
                                             "Total citations" )
    ))|>
    ggplot() + 
    geom_point(aes(x = sample, y= estimate, group = outcome_name, color = outcome_name), position=position_dodge(width=0.5)) + 
    geom_errorbar(aes(x = sample, ymin = conf.low, ymax = conf.high, group = outcome_name, color = outcome_name),  position=position_dodge(width=0.5), width = .5)+
    facet_wrap(~ term + period, ncol = 2) +
    coord_flip()+
    geom_hline(aes(yintercept = 0), color = cb_red, linetype = "dashed")+
    labs(
      x = "", y = "Estimate", 
      color = "Outcome",
      caption = paste("Coefficient estimates of effect of messaging interventions in phone-matched sample, using Lin Estimator.\n Confidence intervals for exploratory outcomes show conventional 95% confidence thresholds."), sep = "") + 
    theme_classic()  + 
    scale_color_manual(values = c(  cb_red,cb_purple,cb_green))+
    theme(plot.title =element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.caption = element_text(size = 10),
          legend.title = element_text(size =10),
          legend.text = element_text(size = 10),
          strip.text = element_text(size = 10))+
    theme( legend.position = "bottom")
)|>
  ggsave(file = "figs/exploratory_risk_tercile_match.png", width= 6.9, height = 8)

# Any treatment by Risk Tercile ------------------------------------------------
n_high_risk_any <- nrow(ate[ate$risk_tercile == "High",])
n_medium_risk_any <- nrow(ate[ate$risk_tercile== "Medium",])
n_low_risk_any <- nrow(ate[ate$risk_tercile == "Low",])

results_risk_tercile <- bind_rows(results_high_risk, results_medium_risk, results_low_risk)|>
  filter(term == 'any')|>
  mutate(outcome = ifelse(outcome == "n_crashes", "n_crashes_12mo", outcome))|>
  mutate(sample = factor(sample, levels = c("Highest risk", "Medium risk", "Low risk"), 
                         labels = c(paste("Highest risk: \n N = ", n_high_risk_any, sep = ""),
                                    paste("Medium risk:\n N = ", n_medium_risk_any, sep = ""),
                                    paste("Lower risk:\n N = ", n_low_risk_any, sep = ""))))

(results_risk_tercile |>
    mutate(outcome_name = str_split_fixed(outcome, "(?=\\d)",2)[,1],
           period = str_split_fixed(outcome, "(?=\\d)",2)[,2]) |>
    mutate(period = factor(period, levels = c( "3mo", "12mo"), 
                           labels = c( "3 months","12 months")))|>
    mutate(outcome_name = factor(outcome_name, 
                                 levels = c( "n_crashes_","riskiest_citations_", "num_citations_"), 
                                 labels = c( "Crashes", "Riskiest citations", 
                                             "Total citations" )
    ))|>
    ggplot() + 
    geom_point(aes(x = sample, y= estimate, group = outcome_name, color = outcome_name), position=position_dodge(width=0.5)) + 
    geom_errorbar(aes(x = sample, ymin = conf.low, ymax = conf.high, group = outcome_name, color = outcome_name),  position=position_dodge(width=0.5), width = .5)+
    facet_wrap(~ period, ncol = 3) +
    coord_flip()+
    geom_hline(aes(yintercept = 0), color = cb_red, linetype = "dashed")+
    labs(
      x = "", y = "Estimate", 
      color = "Outcome",
      caption = paste("Coefficient estimates of effect of any message in whole sample, using Lin Estimator.\n Confidence intervals for exploratory outcomes show conventional 95% confidence thresholds."), sep = "") + 
    theme_classic()  + 
    scale_color_manual(values = c(  cb_red,cb_purple,cb_green))+
    theme(plot.title =element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          plot.caption = element_text(size = 10),
          legend.title = element_text(size =10),
          legend.text = element_text(size = 10),
          strip.text = element_text(size = 10))+
    theme( legend.position = "bottom")
)|>
  ggsave(file = "figs/exploratory_risk_tercile_any.png", width= 6.9, height = 5)


