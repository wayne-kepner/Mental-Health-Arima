#load packages
library(forecast)
library(ggplot2)
library(dplyr)
library(xts)

#here is my real code

#read in original data

setwd("/Users/waynekepner/Desktop/Nobles/MH/OGData")

monthly <- read.csv("final_predictors_for_model_alldata_for_wayne.csv")

# initialize lists for simulated aggregates and differences
#eventually this TWO will have to be changed to the toal number of search terms
# which is 219. the TWO is the number of elements that R is creating as a
#placeholder for us to input stuff in

agg_val_mean_1mo = numeric(219)
agg_val_lower_1mo = numeric(219)
agg_val_upper_1mo = numeric(219)

agg_diff_mean_1mo = numeric(219)
agg_diff_lower_1mo = numeric(219)
agg_diff_upper_1mo = numeric(219)

agg_val_mean_3mo = numeric(219)
agg_val_lower_3mo = numeric(219)
agg_val_upper_3mo = numeric(219)

agg_diff_mean_3mo = numeric(219)
agg_diff_lower_3mo = numeric(219)
agg_diff_upper_3mo = numeric(219)

agg_val_mean_6mo = numeric(219)
agg_val_lower_6mo = numeric(219)
agg_val_upper_6mo = numeric(219)

agg_diff_mean_6mo = numeric(219)
agg_diff_lower_6mo = numeric(219)
agg_diff_upper_6mo = numeric(219)

agg_val_mean_12mo = numeric(219)
agg_val_lower_12mo = numeric(219)
agg_val_upper_12mo = numeric(219)

agg_diff_mean_12mo = numeric(219)
agg_diff_lower_12mo = numeric(219)
agg_diff_upper_12mo = numeric(219)

agg_val_mean_all = numeric(219)
agg_val_lower_all = numeric(219)
agg_val_upper_all = numeric(219)

agg_diff_mean_all = numeric(219)
agg_diff_lower_all = numeric(219)
agg_diff_upper_all = numeric(219)

counter = 1

#loop through querys build model forecast predictions save predictions
#calculate aggregated sum of predictions and difference between aggregated predictions
#and observed values

for (i in names(monthly)[-1]){
  print(counter)
  print(i)
  
  #create univariate time series
  qfts_monthly <- ts(monthly[,i], frequency = 12, start = c(2008, 1))
  
  # create training (pre-COVID) and test (post-COVID) sets
  #the training set will be the time series from START TRAIN = January 2008 to END   TRAIN = February 2020. 
  #the test set will be from START TEST = March 2020 to the END TEST = October 2021
  training_monthly <- window(qfts_monthly, end = c(2020, 02))
  test_monthly <- window(qfts_monthly, start = c(2020, 03))
  
  # fit training data
  fit_monthly <- auto.arima(training_monthly)
  #checkresiduals(fit_monthly, plot = TRUE)
  
  #forecast
  preds_monthly <- forecast(fit_monthly, length(test_monthly))
  
  #extract prediction values to a DF
  date_values = index(test_monthly)
  point_values = preds_monthly$mean
  lower_95_pi_values = preds_monthly$lower[,2]
  upper_95_pi_values = preds_monthly$upper[,2]
  predictions_df = data.frame(
    date_values, point_values, lower_95_pi_values, upper_95_pi_values
  )
  
  #create pathway to save out to DF
  file_path = paste("/Users/waynekepner/Desktop/Nobles/MH/ProcessData", i, sep="/")
  file_path= paste(file_path, ".csv", sep="")
  file_name = sub("\\.", "_",i)
  write.csv(predictions_df,file_path, row.names = FALSE)
  
  #simulations (n=100)
  nsim = 1000
  sim = numeric(nsim)
  
  
  ##months
  h = 1
  
  ##for loop for SIM
  for (i in seq_len(nsim))
    sim[i] <- sum(simulate(fit_monthly, future = TRUE, nsim = h))
  
  ##aggregated values
  agg_val_mean_1mo[counter] <-mean(sim)
  lower_upper_1mo = quantile(sim, prob=c(0.025, 0.975))
  agg_val_lower_1mo[counter] = lower_upper_1mo[1]
  agg_val_upper_1mo[counter] = lower_upper_1mo[2]
  
  ### differences
  agg_diff_mean_1mo[counter] = agg_val_mean_1mo[counter] - test_monthly[1]
  agg_diff_lower_1mo[counter] = agg_val_lower_1mo [counter] - test_monthly[1]
  agg_diff_upper_1mo[counter] = agg_val_upper_1mo[counter] - test_monthly[1]
  
  
  
  ## 3 months
  h = 3
  
  ##for loop for SIM
  for (i in seq_len(nsim))
    sim[i] <- sum(simulate(fit_monthly, future = TRUE, nsim = h))
  
  ##aggregated values
  agg_val_mean_3mo[counter] <-mean(sim)
  lower_upper_3mo = quantile(sim, prob=c(0.025, 0.975))
  agg_val_lower_3mo[counter] = lower_upper_3mo[1]
  agg_val_upper_3mo[counter] = lower_upper_3mo[2]
  
  ### differences
  agg_diff_mean_3mo[counter] = agg_val_mean_3mo[counter] - sum(test_monthly[1:3])
  agg_diff_lower_3mo[counter] = agg_val_lower_3mo [counter] - sum(test_monthly[1:3])
  agg_diff_upper_3mo[counter] = agg_val_upper_3mo[counter] - sum(test_monthly[1:3])
  
  
  
  ## 6 months
  h = 6
  
  ##for loop for SIM
  for (i in seq_len(nsim))
    sim[i] <- sum(simulate(fit_monthly, future = TRUE, nsim = h))
  
  ##aggregated values
  agg_val_mean_6mo[counter] <-mean(sim)
  lower_upper_6mo = quantile(sim, prob=c(0.025, 0.975))
  agg_val_lower_6mo[counter] = lower_upper_6mo[1]
  agg_val_upper_6mo[counter] = lower_upper_6mo[2]
  
  ### differences
  agg_diff_mean_6mo[counter] = agg_val_mean_6mo[counter] - sum(test_monthly[1:6])
  agg_diff_lower_6mo[counter] = agg_val_lower_6mo [counter] - sum(test_monthly[1:6])
  agg_diff_upper_6mo[counter] = agg_val_upper_6mo[counter] - sum(test_monthly[1:6])
  
  
  ## 12 months
  h = 12
  
  ##for loop for SIM
  for (i in seq_len(nsim))
    sim[i] <- sum(simulate(fit_monthly, future = TRUE, nsim = h))
  
  ##aggregated values
  agg_val_mean_12mo[counter] <-mean(sim)
  lower_upper_12mo = quantile(sim, prob=c(0.025, 0.975))
  agg_val_lower_12mo[counter] = lower_upper_12mo[1]
  agg_val_upper_12mo[counter] = lower_upper_12mo[2]
  
  ### differences
  agg_diff_mean_12mo[counter] = agg_val_mean_12mo[counter] - sum(test_monthly[1:12])
  agg_diff_lower_12mo[counter] = agg_val_lower_12mo [counter] - sum(test_monthly[1:12])
  agg_diff_upper_12mo[counter] = agg_val_upper_12mo[counter] - sum(test_monthly[1:12])
  
  
  ## all months
  h = length(test_monthly)
  
  ##for loop for SIM
  for (i in seq_len(nsim))
    sim[i] <- sum(simulate(fit_monthly, future = TRUE, nsim = h))
  
  ##aggregated values
  agg_val_mean_all[counter] <-mean(sim)
  lower_upper_all = quantile(sim, prob=c(0.025, 0.975))
  agg_val_lower_all[counter] = lower_upper_all[1]
  agg_val_upper_all[counter] = lower_upper_all[2]
  
  ### differences
  agg_diff_mean_all[counter] = agg_val_mean_all[counter] - sum(test_monthly)
  agg_diff_lower_all[counter] = agg_val_lower_all[counter] - sum(test_monthly)
  agg_diff_upper_all[counter] = agg_val_upper_all[counter] - sum(test_monthly)
  
  
  #this increases the counter for each loop [LAST THING]
  counter = counter + 1
  print(counter)
  
}



#so the loop is completed with agg differences. so now we will create DF to house
#these data.


queries = names(monthly)[-1]

aggregated_df = data.frame (
  queries,
  agg_val_mean_1mo,
  agg_val_lower_1mo,
  agg_val_upper_1mo,
  agg_diff_mean_1mo,
  agg_diff_lower_1mo, 
  agg_diff_upper_1mo, 
  agg_val_mean_3mo,
  agg_val_lower_3mo,
  agg_val_upper_3mo,
  agg_diff_mean_3mo,
  agg_diff_lower_3mo, 
  agg_diff_upper_3mo, 
  agg_val_mean_6mo,
  agg_val_lower_6mo,
  agg_val_upper_6mo,
  agg_diff_mean_6mo,
  agg_diff_lower_6mo, 
  agg_diff_upper_6mo, 
  agg_val_mean_12mo,
  agg_val_lower_12mo, 
  agg_val_upper_12mo, 
  agg_diff_mean_12mo, 
  agg_diff_lower_12mo,
  agg_diff_upper_12mo,
  agg_val_mean_all, 
  agg_val_lower_all,
  agg_val_upper_all,
  agg_diff_mean_all,
  agg_diff_lower_all, 
  agg_diff_upper_all,
  stringsAsFactors = FALSE
)


write.csv(aggregated_df,"/Users/waynekepner/Desktop/Nobles/MH/ProcessData/aggregated_vals_diffs/aggregated_df.csv", row.names = FALSE)


#here is my test code:





