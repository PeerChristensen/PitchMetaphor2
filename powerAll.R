

#### Power function for mixed-effects logistic regression #####
# Looks at all effects of interest at a time

# Again: J is nr Subj
# K is nr trials
# C is nr conditions

power_all <- function (J, K, C, beta, n.sims=100) 
{
  
  # Effect directions
  effect_dir <- c((beta[2] / abs(beta[2])), 
                  (beta[3] / abs(beta[3])), 
                  (beta[4] / abs(beta[4])), 
                  (beta[5] / abs(beta[5])))
  
  # Create vector for p-values
  signif_relY <- rep(NA,n.sims)
  signif_relYrelX <- rep(NA,n.sims)
  signif_relYrelXcondcomp <- rep(NA,n.sims)
  signif_relYrelXcondcoll <- rep(NA,n.sims)
  
  # For each simulation
  for (s in 1:n.sims){
    
    # Take samples from population
    df_used <- gen_data(J, K, C, beta)
    
    # Fit Model
    model <- glmer(Response ~ RelY + RelX + RelY:RelX + RelY:RelX:Condition + (1 + RelY|Subj_ID), data = df_used, family = binomial(logit))
    
    # Retrieve p-value
    pval_relY <- summary(model)$coefficients[2,4]
    pval_relYrelX <- summary(model)$coefficients[4,4]
    pval_relYrelXcondcomp <- summary(model)$coefficients[5,4]
    pval_relYrelXcondcoll <- summary(model)$coefficients[6,4]
    
    # Retrieve est param
    eff_size_relY <- summary(model)$coefficients[2,1]
    eff_size_relYrelX <- summary(model)$coefficients[4,1]
    eff_size_relYrelXcondcomp <- summary(model)$coefficients[5,1]
    eff_size_relYrelXcondcoll <- summary(model)$coefficients[6,1]
    
    # Check sign
    dir_eff_relY <- eff_size_relY / abs(eff_size_relY)
    dir_eff_relYrelX <- eff_size_relYrelX / abs(eff_size_relYrelX)
    dir_eff_relYrelXcondcomp <- eff_size_relYrelXcondcomp / abs(eff_size_relYrelXcondcomp)
    dir_eff_relYrelXcondcoll <- eff_size_relYrelXcondcoll / abs(eff_size_relYrelXcondcoll)
    
    # Store Boolean for pvalue
    #signif[s]<-(pval)<0.05
    signif_relY[s] <- ifelse(pval_relY<0.05 & dir_eff_relY == effect_dir[1], TRUE, FALSE)
    signif_relYrelX[s] <- ifelse(pval_relYrelX<0.05 & dir_eff_relYrelX == effect_dir[2], TRUE, FALSE)
    signif_relYrelXcondcomp[s] <- ifelse(pval_relYrelXcondcomp<0.05 & dir_eff_relYrelXcondcomp == effect_dir[3], TRUE, FALSE)
    signif_relYrelXcondcoll[s] <- ifelse(pval_relYrelXcondcoll<0.05 & dir_eff_relYrelXcondcoll == effect_dir[4], TRUE, FALSE)
    
    # Check-print
    print(paste("Done with simulation", s))
    print(paste('P-values', pval_relY, pval_relYrelX, pval_relYrelXcondcomp, pval_relYrelXcondcoll))
    print(paste('Eff-size', eff_size_relY, eff_size_relYrelX, eff_size_relYrelXcondcomp, eff_size_relYrelXcondcoll))
    print(paste('Exp_sign:', effect_dir[1], effect_dir[2], effect_dir[3],effect_dir[4]))
    print(paste("Signif", signif_relY[s], signif_relYrelX[s], signif_relYrelXcondcomp[s], signif_relYrelXcondcoll[s]))
  }
  
  # How many times did it turn out significant? 
  power_relY<-mean(signif_relY)
  power_relYrelX<-mean(signif_relYrelX)
  power_relYrelXcondcomp<-mean(signif_relYrelXcondcomp)
  power_relYrelXcondcoll<-mean(signif_relYrelXcondcoll)
  
  # Compute confidence intervals for all effects
  n_succ_relY <- length(signif_relY [signif_relY ==  TRUE] )
  n_succ_relYrelX <- length(signif_relYrelX [signif_relYrelX ==  TRUE] )
  n_succ_relYrelXcondcomp <- length(signif_relYrelXcondcomp [signif_relYrelXcondcomp ==  TRUE] )
  n_succ_relYrelXcondcoll <- length(signif_relYrelXcondcoll [signif_relYrelXcondcoll ==  TRUE] )
  
  n_trials <- n.sims
  
  conf_int_relY <- prop.test(n_succ_relY, n_trials, conf.level = 0.95, correct = FALSE)$conf.int
  conf_int_relYrelX <- prop.test(n_succ_relYrelX, n_trials, conf.level = 0.95, correct = FALSE)$conf.int
  conf_int_relYrelXcondcoll <- prop.test(n_succ_relYrelXcondcoll, n_trials, conf.level = 0.95, correct = FALSE)$conf.int
  conf_int_relYrelXcondcomp <- prop.test(n_succ_relYrelXcondcomp, n_trials, conf.level = 0.95, correct = FALSE)$conf.int
  
  lower_relY <- conf_int_relY[1]
  upper_relY <- conf_int_relY[2]
  
  lower_relYrelX <- conf_int_relYrelX[1]
  upper_relYrelX <- conf_int_relYrelX[2]
  
  lower_relYrelXcondcomp <- conf_int_relYrelXcondcomp[1]
  upper_relYrelXcondcomp <- conf_int_relYrelXcondcomp[2]
  
  lower_relYrelXcondcoll <- conf_int_relYrelXcondcoll[1]
  upper_relYrelXcondcoll <- conf_int_relYrelXcondcoll[2]
  
  # Outcome of power analysis 
  outcome <- data.frame('Power_relY' = power_relY, 'N_Succ_relY' = n_succ_relY, 'N_trials' = n_trials, 'Lower_relY' = lower_relY, 'Upper_relY' = upper_relY,
                        'Power_relYrelX' = power_relYrelX, 'N_Succ_relYrelX' = n_succ_relYrelX, 'N_trials' = n_trials, 'Lower_relYrelX' = lower_relYrelX, 'Upper_relYrelX' = upper_relYrelX,
                        'Power_relYrelXcondcomp' = power_relYrelXcondcomp, 'N_Succ_relYrelXcondcomp' = n_succ_relYrelXcondcomp, 'N_trials' = n_trials, 'Lower_relYrelXcondcomp' = lower_relYrelXcondcomp, 'Upper_relYrelXcondcomp' = upper_relYrelXcondcomp,
                        'Power_relYrelXcondcoll' = power_relYrelXcondcoll, 'N_Succ_relYrelXcondcoll' = n_succ_relYrelXcondcoll, 'N_trials' = n_trials, 'Lower_relYrelXcondcoll' = lower_relYrelXcondcoll, 'Upper_relYrelXcondcoll' = upper_relYrelXcondcoll
  )
  
  #print(outcome)
  
  return(outcome)
  
}

