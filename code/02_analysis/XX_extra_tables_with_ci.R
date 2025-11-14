# Tables with confidence intervals- for checking numbers in the text
# Note that this is the standard CI, not the one with the simulated pvalues 
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
                             \\item Stars: * = .017 (Simulated crash p-value) ** = .004 (Simulated citation p-value) "),
                  caption.above = TRUE, 
                  file =  "tables/2024-10/Confirmatory/confirmatory_wholesample_with_ci.doc"
)
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
                                                 "risk_tercileLow_c" = "Low", 
                                                 "risk_tercileMedium_c" = "Medium",
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
                          
                          #custom.coef.names = c("\\textbf{Mailer}", "\\textbf{Text Message}", "\\textbf{Both}"),
                          
                          custom.note = c("\\item Lin Estimator. Car make, intercept and interacted coefficients on covariates omitted from display for brevity.
                             \\item Stars: * = .017 (Simulated crash p-value) ** = .004 (Simulated citation p-value) "),
                          file = "tables/2024-10/Confirmatory/confirmatory_matched_sample_with_ci.docx"
)