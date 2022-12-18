library(data.table)
library(ggplot2)
library(grDevices)
library(gridExtra)
library(reshape2)
library(grid)
library(rstan)
library(scales)
library(boot)
library(tidyverse)
theme_set(theme_bw())

gen_estimates_from_one_draw_f <- function(alpha, beta, tao_sqr, model_input) {
  polls_p <- inv.logit( logit(model_input$v[model_input$r]) + alpha[model_input$r] + model_input$t * beta[model_input$r] )
  polls_sigma_sqr <- (polls_p*(1-polls_p)/model_input$n) * tao_sqr[model_input$r]
  polls_results <- data.table(r = model_input$r, p = polls_p, v = model_input$v[model_input$r], sigma = sqrt(polls_sigma_sqr))
  
  elections_results <- polls_results[, list(
    b_r = mean(p-v),
    sigma_r = mean(sigma)
  ), by = r]
  elections_q <- inv.logit( logit(model_input$v) + alpha )
  elections_results <- cbind(elections_results[order(r)], b_r_0 = (elections_q-model_input$v))
  
  agg_results <- elections_results[, list(
    mu_b = mean(abs(b_r)),
    mu_b_0 = mean(abs(b_r_0)),
    mu_sigma = mean(sigma_r)
  )]
  
  draw_results <- vector(mode="list", length=2)
  names(draw_results) <- c("elections", "overall")
  draw_results[["elections"]] <- elections_results
  draw_results[["overall"]] <- agg_results
  return(draw_results)
}

## ORIGINAL MODEL PRESIDENTIAL
  
model_input_data_file_both <- c("~/STAT 2103 Final Project Desktop/stan_data_presidential.RData")
load(model_input_data_file_both)

model_result_file_original_presidential <- c("~/stan_final_model_result_presidential.RData")
load(model_result_file_original_presidential)

actual_polls_fitted_params_original_presidential <- rstan::extract(actual_polls_fit)


election_sim_results_original_presidential <- data.table()
election_type_sim_results_original_presidential <- data.table()
# Gather quantities of interest in each draw for real data
n_sim <- dim(actual_polls_fitted_params_original_presidential$alpha)[1]
for (sim in seq(1, n_sim)) {
  draw_results <- gen_estimates_from_one_draw_f(
    actual_polls_fitted_params_original_presidential$alpha[sim,],
    actual_polls_fitted_params_original_presidential$beta[sim,],
    actual_polls_fitted_params_original_presidential$tao_sqr[sim,],
    actual_polls_data
  )
  election_sim_results_original_presidential <- rbind(election_sim_results_original_presidential, draw_results[["elections"]])
  election_type_sim_results_original_presidential <- rbind(election_type_sim_results_original_presidential, cbind(src = "Actual", draw_results[["overall"]]))
}

original_presidential <- election_sim_results_original_presidential[, list(
  bias = mean(b_r),
  sd = mean(sigma_r),
  list(b_r)
), by = c("r")]

  
## MULTIPLICATIVE MODEL PRESIDENTIAL

model_input_data_file_both <- c("~/STAT 2103 Final Project Desktop/stan_data_presidential.RData")
load(model_input_data_file_both)

model_result_file_multiplicative_presidential <- c("~/stan_final_model_multiplicative_result_presidential.RData")
load(model_result_file_multiplicative_presidential)

actual_polls_fitted_params_multiplicative_presidential <- rstan::extract(actual_polls_fit)


election_sim_results_multiplicative_presidential <- data.table()
election_type_sim_results_multiplicative_presidential <- data.table()
# Gather quantities of interest in each draw for real data
n_sim <- dim(actual_polls_fitted_params_multiplicative_presidential$alpha)[1]
for (sim in seq(1, n_sim)) {
  draw_results <- gen_estimates_from_one_draw_f(
    actual_polls_fitted_params_multiplicative_presidential$alpha[sim,],
    actual_polls_fitted_params_multiplicative_presidential$beta[sim,],
    actual_polls_fitted_params_multiplicative_presidential$tao_sqr[sim,],
    actual_polls_data
  )
  election_sim_results_multiplicative_presidential <- rbind(election_sim_results_multiplicative_presidential, draw_results[["elections"]])
  election_type_sim_results_multiplicative_presidential <- rbind(election_type_sim_results_multiplicative_presidential, cbind(src = "Actual", draw_results[["overall"]]))
}

multiplicative_presidential <- election_sim_results_multiplicative_presidential[, list(
  bias = mean(b_r),
  sd = mean(sigma_r),
  list(b_r)
), by = c("r")]

# Calculating the percentage of elections that had a non-significant confidence interval
count <- 0
for(i in seq_along(unique(election_sim_results_original_presidential$r))){
  original_boot <- sample(original_presidential$V3[[i]],size=40000,replace=T)
  multiplicative_boot <- sample(multiplicative_presidential$V3[[i]],size=40000,replace=T)
  difference_in_mods <- multiplicative_boot-original_boot
  sorted_diff <- sort(difference_in_mods)
  lowerbound <- quantile(sorted_diff,c(0.025,0.975))[1]
  upperbound <- quantile(sorted_diff, c(0.025,0.975))[2]
  if(lowerbound < 0 & upperbound > 0){
    count <- count + 1
  }
}

percentage_mods_same_presidential <- count/length(unique(election_sim_results_original_presidential$r))



## ORIGINAL MODEL GUBERNATORIAL

model_input_data_file_both <- c("~/STAT 2103 Final Project Desktop/stan_data_gubernatorial.RData")
load(model_input_data_file_both)

model_result_file_original_gubernatorial <- c("~/stan_final_model_result_gubernatorial.RData")
load(model_result_file_original_gubernatorial)

actual_polls_fitted_params_original_gubernatorial <- rstan::extract(actual_polls_fit)


election_sim_results_original_gubernatorial <- data.table()
election_type_sim_results_original_gubernatorial <- data.table()
# Gather quantities of interest in each draw for real data
n_sim <- dim(actual_polls_fitted_params_original_gubernatorial$alpha)[1]
for (sim in seq(1, n_sim)) {
  draw_results <- gen_estimates_from_one_draw_f(
    actual_polls_fitted_params_original_gubernatorial$alpha[sim,],
    actual_polls_fitted_params_original_gubernatorial$beta[sim,],
    actual_polls_fitted_params_original_gubernatorial$tao_sqr[sim,],
    actual_polls_data
  )
  election_sim_results_original_gubernatorial <- rbind(election_sim_results_original_gubernatorial, draw_results[["elections"]])
  election_type_sim_results_original_gubernatorial <- rbind(election_type_sim_results_original_gubernatorial, cbind(src = "Actual", draw_results[["overall"]]))
}

original_gubernatorial <- election_sim_results_original_gubernatorial[, list(
  bias = mean(b_r),
  sd = mean(sigma_r),
  list(b_r)
), by = c("r")]


## MULTIPLICATIVE MODEL GUBERNATORIAL

model_input_data_file_both <- c("~/STAT 2103 Final Project Desktop/stan_data_gubernatorial.RData")
load(model_input_data_file_both)

model_result_file_multiplicative_gubernatorial <- c("~/stan_final_model_multiplicative_result_gubernatorial.RData")
load(model_result_file_multiplicative_gubernatorial)

actual_polls_fitted_params_multiplicative_gubernatorial <- rstan::extract(actual_polls_fit)


election_sim_results_multiplicative_gubernatorial <- data.table()
election_type_sim_results_multiplicative_gubernatorial <- data.table()
# Gather quantities of interest in each draw for real data
n_sim <- dim(actual_polls_fitted_params_multiplicative_gubernatorial$alpha)[1]
for (sim in seq(1, n_sim)) {
  draw_results <- gen_estimates_from_one_draw_f(
    actual_polls_fitted_params_multiplicative_gubernatorial$alpha[sim,],
    actual_polls_fitted_params_multiplicative_gubernatorial$beta[sim,],
    actual_polls_fitted_params_multiplicative_gubernatorial$tao_sqr[sim,],
    actual_polls_data
  )
  election_sim_results_multiplicative_gubernatorial <- rbind(election_sim_results_multiplicative_gubernatorial, draw_results[["elections"]])
  election_type_sim_results_multiplicative_gubernatorial <- rbind(election_type_sim_results_multiplicative_gubernatorial, cbind(src = "Actual", draw_results[["overall"]]))
}

multiplicative_gubernatorial <- election_sim_results_multiplicative_gubernatorial[, list(
  bias = mean(b_r),
  sd = mean(sigma_r),
  list(b_r)
), by = c("r")]

# Calculating the percentage of elections that had a non-significant confidence interval
count <- 0
for(i in seq_along(unique(election_sim_results_original_gubernatorial$r))){
  original_boot <- sample(original_gubernatorial$V3[[i]],size=10000,replace=T)
  multiplicative_boot <- sample(multiplicative_gubernatorial$V3[[i]],size=10000,replace=T)
  difference_in_mods <- multiplicative_boot-original_boot
  sorted_diff <- sort(difference_in_mods)
  lowerbound <- quantile(sorted_diff,c(0.025,0.975))[1]
  upperbound <- quantile(sorted_diff, c(0.025,0.975))[2]
  if(lowerbound < 0 & upperbound > 0){
    count <- count + 1
  }
}

percentage_mods_same_gubernatorial <- count/length(unique(election_sim_results_original_gubernatorial$r))

