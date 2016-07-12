library(yaml)
# Load parameters
params <- yaml.load_file('params.yaml')

# An Assets Under Management object to keep track
aums <- list()
aums_hist <- list()


# Set initial portfolio state
aums$high.watermark<- as.double(params$aum)
aums$last.period.aum <- as.double(params$aum)
params$returns <- as.numeric(params$returns)
params$returns <- (1+ params$returns) ^ (1 / params$payments_in_each_period) -1
params$returns <- c(rep(params$returns,each = params$payments_in_each_period))
# For Debugging
iLoop <- 1

# Go thru each period
for(iLoop in 1:length(params$returns)) {
  # Reset the starring aum
  aums$starting.period.aum <- aums$last.period.aum 
  # Get current return
  aums$current_return <- as.double(params$returns[iLoop])
  # Calculate pnl for the period
  aums$current_pnl <- aums$starting.period.aum * aums$current_return
  
  # Calculate aum before fees
  aums$gross_before_fees <- aums$starting.period.aum + aums$current_pnl
  # aum after management fee
  aums$after_management_fee <- mean(aums$gross_before_fees,aums$starting.period.aum) * 
    (1-params$management_fee)
  # check if watermark has been passed
  aums$new_watermark <- aums$high.watermark < aums$after_management_fee

  # Collect the fee
  if(aums$new_watermark){
    # Reduce aum by the performance fee
    # which is charged against the aum net of management fee
    aums$pnl_net_of_pfee <- aums$current_pnl * (1-params$performance_fee)
    aums$net_of_everything <- aums$starting.period.aum * (1-params$management_fee) + 
      aums$pnl_net_of_pfee
    aums$last.period.aum <- aums$net_of_everything
    # Move the watermark up, and before management fees
    aums$high.watermark <- aums$gross_before_fees
  } else 
  # the new aum is the aum net of management fee
    aums$last.period.aum <- aums$after_management_fee
  # the watermark remains the same
  aums$high.watermark <- aums$high.watermark
  aums_hist[[iLoop]] <- aums
}
rbindlist(aums_hist)