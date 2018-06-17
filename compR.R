

#### Compute power for mixed-effects logistic regression #####
# Takes into account only one effect at a time.
# To be specified in input

# J is nr Subj
# K is nr trials
# C is nr conditions
# beta is vector of population parameters

power <- function (J, K, C, effect_of_interest, beta, dir_beta = 1, n.sims=100) 
{
  
  # Create vector for p-values
  signif <- rep(NA,n.sims)
  
  # For each simulation
  for (s in 1:n.sims){
    
    # Take samples from population
    # Calls function from SimData.R
    df_used <- gen_data(J, K, C, beta)
    
    # Fit Model
    model <- glmer(Response ~ RelY + RelX + RelY : RelX + RelY : RelX : Condition + (1 + RelY|Subj_ID), data = df_used, family = binomial(logit))
    
    # Retrieve p-value
    pval <- summary(model)$coefficients[effect_of_interest,4]
    # Retrieve beta
    eff_size <- summary(model)$coefficients[effect_of_interest,1]
    # Check sign
    dir_eff <- eff_size / abs(eff_size)
    
    # Store Boolean for pvalue
    #signif[s]<-(pval)<0.05
    signif[s] <- ifelse(pval<0.05 & dir_eff == dir_beta, TRUE, FALSE)
    
    # Check-print
    print(paste("Outcome sim:", signif[s]))
    print(paste("P-value:", pval))
    print(paste("Effect_size:", eff_size))
    print(paste("Done with simulation", s))
    
  }
  
  # How many times did it turn out significant? 
  power<-mean(signif)
  
  # Compute confidence intervals
  n_succ <- length(signif [signif ==  TRUE] )
  n_trials <- n.sims
  
  conf_int <- prop.test(n_succ, n_trials, conf.level = 0.95, correct = FALSE)$conf.int
  lower <- conf_int[1]
  upper <- conf_int[2]
  
  # Outcome of power analysis 
  outcome <- data.frame('Power' = power, 'N_Succ' = n_succ, 'N_trials' = n_trials, 'Lower' = lower, 'Upper' = upper)
  
  #print(outcome)
  
  return(outcome)
  
}

