library(estimatr)
library(randomizr)
library(tidyverse)
library(broom)
library(here)

########################################################################################################################################
# This code 
  # uses simulation analysis to determine the threshold value against which we should measure levels of statsistical significance for the ATE project 
  # is based loosely on https://egap.org/resource/10-things-to-know-about-multiple-comparisons/
  # uses real, pre-treatment outcome and covariate data to build in the correlation structure we anticipate in our outcome data
# Preanalysis plan can be found at https://osf.io/px8ew
########################################################################################################################################


# Identify the four outcomes we're interested in testing for citations 
outcomes <- c("total_cites_3mo", "total_cites_12mo", "risky_cites_3mo", "risky_cites_12mo") # Grab our four outcome columns of interest
# Print the correlation matrix across these outcomes 
cor(citation_summary_pretreat[,outcomes])
    
    #############
    # Simulations
    #############
      # Start by simulating all the permuted treatments and p values 
    
      # Function to simulate treatment and test against threshold
      simulate_significance <- function(simdata = preanalysis_citation_summary_pretreat){
      # Assign treatment probabilities: Note that the probability is the same within each block, but RA doesn't necessarily know that, so we need to give it the full matrix 
      # block_prob_each must be a matrix with the same number of rows as blocks and the same number of columns as treatment arms.
      block_probs <- tibble("control" = rep(.523,72),
                            "mailer" = rep(.159, 72),
                            "sms" = rep(.159, 72),
                            "both" = rep(.159, 72))

      # 1: Randomly assign treatment status using block randomization
      simdata$treatment <- block_ra(block_prob_each=block_probs, blocks = simdata$block, conditions = c("control", "mailer", "sms", "both")) # Generate fake treatment status in one of the four categories (mailer only, both, text only, none)
      simdata <- simdata |> mutate(
        mailer = ifelse(treatment == "mailer", 1, 0),
        sms = ifelse(treatment == "sms", 1, 0),
        both = ifelse(treatment == "both", 1, 0)
      )
      
      # 2: Run our three regressions of interest and store all p values 
      ps_sim <- c()
      for (i in outcomes){
        model1 <- lm_lin(formula = as.formula(paste(i, "~ mailer", sep = "")), covariates = ~ state_ward + risk_tercile + match, data = simdata) |> tidy()
        model2 <- lm_lin(formula = as.formula(paste(i, "~ sms", sep = "")), covariates = ~ state_ward + risk_tercile, data = simdata[simdata$match==1, ]) |> tidy()
        model3 <- lm_lin(formula = as.formula(paste(i, "~ both", sep = "")), covariates = ~ state_ward + risk_tercile, data = simdata[simdata$match==1, ]) |> tidy()
        
        regression_ps <- c(model1[model1$term == "mailer", "p.value"],
                           model2[model2$term == "sms", "p.value"],
                           model3[model3$term == "both", "p.value"])
        ps_sim <- c(ps_sim,regression_ps)
      }
      # We ask R if any of our simulated p values would be considered significant at our chosen threshold
    return(ps_sim)
    }
  
    
    # Run the simulation nsim times 
    nsim <- 20000
    set.seed(23111712)
    # ps_from_sims <- replicate(simulate_significance(simdata = citation_summary_pretreat),n=nsim)
    # do not overwrite this unless you have the time/bandwidth to run with 10,000 simulations
     file_name <- paste("data/ps-from-simulations-", nsim, "-sims.csv", sep = "")
    # write_csv(as.data.frame(ps_from_sims), here(file_name))
    ps_from_sims <- read_csv(here(file_name))
    
    # We are trying to set a "threshold" alpha (ie standard to test any individual coefficient against) that leads us to an *overall*
    # type 1 error rate of .05. So for each threshold, we test whether we'd get a type 1 error against it, and then we 
    # average across all simulations to see the average type 1 error
    # For each potential value of the threshold, we'll see what share of simulations give us back false significant results
    # and we'll pick the *highest* threshold that returns <.05 spurious significant results 
    
    thresholds <- seq(0, 0.05, length.out = 1000) # sequence of p-values to test
    threshold_finder <- function(threshold){
      mean(apply(ps_from_sims, 2, y <- function(x) sum(x <= threshold) > 0 ))
    }
    type_I_rate <- sapply(thresholds, threshold_finder)
    
    # Find the largest threshold that yields an alpha type I error rate
    thresholds_with_type_I <- tibble(
      threshold = thresholds, 
      type_I_rate = type_I_rate
    )
    target_p_value <- thresholds_with_type_I |> 
      filter(type_I_rate <= .05) |> 
      arrange(desc(type_I_rate)) |>
      slice(1) |>
      select(threshold)

   target_p_value  
    # 0.004704705
    
