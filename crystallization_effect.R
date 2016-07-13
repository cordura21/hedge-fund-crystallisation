library(yaml)
# Load parameters
params <- yaml.load_file('params.yaml')

# An Assets Under Management object to keep track
# Try to remove it if it already exists

try(rm(aums))
try(rm(aums_hist))

aums <- list()
aums_hist <- list()


# Set initial portfolio state
aums$high.watermark <- as.double(params$aum)
aums$end.period.aum <- as.double(params$aum)

# Convert returns to higher frequencies if necesary
params$returns <- as.numeric(params$returns)
params$returns <-
  (1 + params$returns) ^ (1 / params$payments_in_each_period) - 1
params$returns <-
  c(rep(params$returns, each = params$payments_in_each_period))

# Convert also the management fee rate to higher frequency
params$management_fee <- params$management_fee / params$payments_in_each_period
# Finished conerting returns


# This is just For Debugging
iLoop <- 1

# Go thru each period. The num of periods depend on
# the crystallization frequency

for (iLoop in 1:length(params$returns)) {
  # Reset the starring aum
  aums$starting.period.aum <- aums$end.period.aum
  # Get current return
  aums$current_rate_of_return <- as.double(params$returns[iLoop])
  # Calculate pnl for the period
  aums$current_pnl <- aums$starting.period.aum * aums$current_rate_of_return
  
  # Calculate aum before fees
  aums$gross_before_fees <- aums$starting.period.aum + aums$current_pnl
  # aum after management fee
  aums$management_fee <- ((aums$starting.period.aum+aums$gross_before_fee) / 2) *
    params$management_fee
  
  aums$after_management_fee <- aums$gross_before_fee - aums$management_fee
  
  # check if watermark has been passed
  aums$new_watermark <- aums$high.watermark < aums$after_management_fee
  
  if (aums$new_watermark) {
    # Collect the fee
    # Reduce aum by the performance fee
    # which is charged against the aum net of management fee
    aums$performance_fee <-
      (aums$current_pnl - aums$management_fee) * params$performance_fee
    
    # Move the watermark up, and before management fees
    aums$high.watermark <- aums$gross_before_fees
  } else {
    aums$performance_fee <- 0
    aums$high.watermark <- aums$high.watermark
  }
  
  aums$net_of_everything <-
    aums$gross_before_fees - aums$management_fee - aums$performance_fee
  aums$end.period.aum <- aums$net_of_everything
  
  aums_hist[[iLoop]] <- aums
}

library(data.table)
historical_results <- rbindlist(aums_hist, fill = TRUE)

View(historical_results)

# Calculate direct net performance, with crystallization at the end
# ec = end crystallization
ec <- list()
# Create a profit factor for each period,
# less the management fee. I think
# it is a valid approximation

write.csv(historical_results,file.path('results','results.csv'))
ec$period_return <- (1 + (params$returns - params$management_fee))
ec$cum_return <- prod(ec$period_return)
# hasta acá estoy bien
ec$final_aum_gross <- params$aum * ec$cum_return
ec$pnl <- ec$final_aum_gross - params$aum
ec$perf_fee <- ifelse(ec$pnl < 0,0, ec$pnl * params$performance_fee)
ec$final_aum_net <- ifelse(ec$perf_fee == 0, ec$final_aum_gross,ec$final_aum_gross - ec$perf_fee )

print('Final AUM with end of contract crystallization:')
print(historical_results$end.period.aum[NROW(historical_results)])

print('Final AUM WITHOUT crystallization:')
print(ec$final_aum_net)


print('Difference:')
print(historical_results$end.period.aum[NROW(historical_results)] - ec$final_aum_net)






