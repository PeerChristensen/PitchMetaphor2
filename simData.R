

### Define function to generate our data based on regression model

# J: Nr of subjects
# K: Nr of trials/combinations
# C : Nr of conditions
# beta is a vector of 5 parameters

gen_data <- function(J, K, C, beta)
  
{
  
  ################ GENERATE SUBJECT, TRIAL AND CONDITION ###############################################
  
  # Number of datapoints
  DP <- J*K*C
  
  # Generate subj ID
  subj <- 1: J
  trial <- 1: K
  condition <- 1: C
  
  df <- expand.grid (Subj_ID = subj, Trial = trial, Condition = condition)
  df <- df[ order(df$Subj_ID), ]
  
  
  ################ GENERATE VALUES FOR INDEPENDENT VARIABLES ##########################################  
  
  library(gtools)
  
  # All possible X and Y coordinates
  X_coord <- seq(-0.7, 0.7, by = 0.2)
  Y_coord <- seq(-0.375, 0.375, by = 0.150) 
  
  # Create table with all possible set of coordinates
  all_comb <- expand.grid(X = X_coord, Y = Y_coord)
  
  # Create index for table with all possible pair of targets 
  all_tuples <- data.frame(permutations(n = nrow(all_comb), r = 2, rownames(all_comb)))
  colnames(all_tuples) <- c("index_1", "index_2")
  
  # Extract values from indices
  all_tuples <- do.call(
    rbind, apply(all_tuples, 1, function(x) {
      cbind(all_comb[x["index_1"], ], all_comb[x["index_2"], ])
    }
    )
  )
  
  # Rename Coordinates
  colnames(all_tuples) <- c("X1", "Y1", "X2", "Y2")
  
  # Compute relative distances
  all_tuples$relX <- all_tuples$X1 - all_tuples$X2
  all_tuples$relY <- all_tuples$Y1 - all_tuples$Y2
  
  # Only leave relative distances
  #all_tuples <- all_tuples[, c("relX", "relY")]
  
  
  
  ############### NOW EXTRACT ONE RANDOM COMBINATION PER DATAPOINT ##################################
  
  # Create vectors with length nr datapoints to store values
  df$RelX <- rep(NA, nrow(df))
  df$RelY <- rep(NA, nrow(df))
  
  ######### SAMPLING TYPE 1 #############
  # Sample coordinates for each datapoint
  for (i in 1:DP) {
    
    # Sample two random points
    idx <- sample(nrow(all_tuples), 1)
    
    # First set of coordinates
    df[i, "RelX"] <- all_tuples[idx, 1]
    df[i, "RelY"] <- all_tuples[idx, 2] 
  }
  
  ######### SAMPLING TYPE 2 - BALANCED ##############
  #both_pos <- sample(nrow( subset ( all_tuples, relX >= 0 & relY >=0 ) ),  K/4)
  #both_neg <- sample(nrow( subset ( all_tuples, relX < 0 & relY < 0 ) ),  K/4)
  #y_pos <- sample(nrow( subset ( all_tuples, relX < 0 & relY >= 0 ) ),  K/4)
  #x_pos <- sample(nrow( subset ( all_tuples, relX >= 0 & relY < 0 ) ),  K/4)
  
  
  ########### LET'S DEFINE POPULATION PARAMETERS AND MODEL TO GENERATE DATA #######################################
  
  # To specify:
  # - betas (for main effects and interactions)
  # - Standard deviations for random effects (here: intercept only)
  # - Correlations for random effects only in case we add random slopes
  # - Variance of residuals, as in sigma_e = 0.51
  
  ########## POPULATION PARAMETERS #############################
  #beta <- c(0, # Population intercept,
  #          0, # RelY
  #          0, # Interaction RelXRelY
  #          0, # Interaction RelXRelYCondComp, 
  #          0) # Interaction RelXRelYCondColl
  
  #beta <- rep(minbeta, 4)
  #beta <- c(0, minbeta)
  
  rint_sd <- 0.6 # SD random intercepts
  rslope_sd <- 0.6 # SD random slopes
  rand_corr <- 0.2 # Correlation between random effects
  
  # Mean and vcov matrix
  rand_mu <- c(0,0)
  rand_vcov <- matrix(c(rint_sd^2, rand_corr^rint_sd^rslope_sd, 
                        rand_corr^rint_sd^rslope_sd, rslope_sd^2),
                      ncol = 2)
  
  sigma_e <- 0.1 # Necessary?
  
  ########## GET RANDOM INTERCEPTS AND SLOPES ##################
  rand_par <- mvrnorm(J, rand_mu, rand_vcov)
  rand_int <- rand_par[,1]
  rand_slope <- rand_par[,2]
  df$Rand_Int <- rep(rand_int, each = K*C)
  df$Rand_Slope <- rep(rand_slope, each = K*C)
  
  ########## GENERATE RESIDUALS #####################
  df$Error <- rnorm(DP, 0, sigma_e)
  
  ########## FORMAT AND INITIALIZE VARIABLES ###################
  # Condition as factor 
  # -1 Baseline
  # 0 Complementary
  # 1 Collaborative
  df$Condition <- as.factor(df$Condition)
  levels(df$Condition) <- c(-1, 0, 1)
  
  # Recode condition into different columns
  df$Baseline <- ifelse(df$Condition == -1, 1, 0)
  df$Complementary <- ifelse(df$Condition == 0, 1, 0)
  df$Collaborative <- ifelse(df$Condition == 1, 1 ,0)
  
  # Create outcome variable column
  df$Response <- rep(NA, DP)
  df$Resp_Prob <- rep(NA, DP)
  df$Resp_Prob_Log <- rep(NA, DP)
  
  ############## GENERATE DATA! #####################################################
  # Generate as vector (remove the i and add rand int to df)
  # Change binom and take prob_log
  # Simulate main effects only
  
  df$Resp_Prob <- beta[1] + df$Rand_Int + # PopIntercept + RandomIntercept
    (beta[2] + df$Rand_Slope) * (df$RelY) +
    beta[3] * (df$RelY * df$RelX)   +
    beta[4] * (df$RelY * df$RelX * df$Complementary) +
    beta[5] * (df$RelY * df$RelX * df$Collaborative) +
    df$Error
  
  # Link function
  df$Resp_Prob_Log <- exp(df$Resp_Prob) / (1 + exp(df$Resp_Prob))
  
  # Generate response
  df$Response <- rbinom(DP, 1, df$Resp_Prob_Log)
  
  # Just make it nicer
  df <- df[, c("Subj_ID", "Trial", "Condition", "Baseline", "Complementary", "Collaborative", "RelX", "RelY", "Rand_Int", "Rand_Slope", "Error", "Resp_Prob", "Resp_Prob_Log", "Response")]
  
  # alternative way to simulate data from model
  # y <- simulate(model, 1, newparams = list(theta = 1.54, beta = beta)))
  
  # Visualize and return
  head(df)
  
  # Return multiple arguments
  return(df)
  
}

