#' Simulate an Monetary Policy
#'
#' @param periods Number of periods to simulate (Default is 50)
#' @param gamma
#' @param alpha
#' @param rule_pi
#' @param rule_y
#' @param delta
#' @param adaptive
#' @param cost_push_shock_persistance
#' @param demand_shock_persistance
#' @param monetary_shock_persistance
#' @param financial_shock_persistance
#' @param demand_shocks
#' @param cost_push_shocks
#' @param monetary_shocks
#' @param financial_shocks
#' @param demand_shock_magnitude
#' @param cost_push_shock_magnitude
#' @param monetary_shock_magnitude
#' @param financial_shock_magnitude
#'
#' @return
#' @export
#'
#' @examples


simulate_economy <- function(periods = 50,
                             gamma = 0.75, # PC parameters
                             alpha = 0.8,  # IS curve parameters
                             rule_pi = 1.5, # Response to inflation (relative to target)
                             rule_y = 0.5, # Response to output gap
                             delta = 1,   # Financial Markets frictions
                             adaptive = 1, # Weight on past inflation (1=adaptive expectations)
                             cost_push_shock_persistance = 0.5,
                             demand_shock_persistance = 0.5,
                             monetary_shock_persistance = 0.5,
                             financial_shock_persistance = 0.5,
                             demand_shocks = c(), # Periods where demand shocks occur
                             cost_push_shocks = c(), # Periods where cost-push shocks occur
                             monetary_shocks = c(), # Periods where monetary shocks occur
                             financial_shocks = c(), # Periods where financial shocks occur
                             demand_shock_magnitude = 1, # Magnitude of demand shocks
                             cost_push_shock_magnitude = 1, # Magnitude of cost-push shocks
                             monetary_shock_magnitude = 1, # Magnitude of monetary shocks
                             financial_shock_magnitude = 1  # Magnitude of financial shocks
) {

  # Calculate theta based on the formula provided
  theta <- (1 + alpha * rule_y - alpha * delta) /
    (1 + alpha * rule_y - alpha * delta + alpha * gamma * (rule_pi - 1))

  credible <- 1 - adaptive # Weight on inflation target (1=perfect credibility)

  # Initialize shock vectors
  cost_push_shock <- rep(0, periods)
  demand_shock <- rep(0, periods)
  monetary_shock <- rep(0, periods)
  financial_shock <- rep(0, periods)
  pi_t_change <- rep(0, periods)

  # Initialize variables for simulation
  pie <- rep(2.0, periods)  # Inflation expectations
  pit <- rep(2.0, periods)  # Inflation target
  change_lending_rate <- rep(0.0, periods) # Change in lending rate

  # Set shocks based on input periods and magnitudes
  demand_shock[demand_shocks] <- demand_shock_magnitude
  cost_push_shock[cost_push_shocks] <- cost_push_shock_magnitude
  monetary_shock[monetary_shocks] <- monetary_shock_magnitude
  financial_shock[financial_shocks] <- financial_shock_magnitude

  # Apply persistence for shocks
  for (t in 2:periods) {
    demand_shock[t] <- ifelse(t %in% demand_shocks, demand_shock[t], demand_shock[t-1] * demand_shock_persistance)
    cost_push_shock[t] <- ifelse(t %in% cost_push_shocks, cost_push_shock[t], cost_push_shock[t-1] * cost_push_shock_persistance)
    monetary_shock[t] <- ifelse(t %in% monetary_shocks, monetary_shock[t], monetary_shock[t-1] * monetary_shock_persistance)
    financial_shock[t] <- ifelse(t %in% financial_shocks, financial_shock[t], financial_shock[t-1] * financial_shock_persistance)
  }

  # Calculate output gap
  output_gap <- numeric(periods)
  for (t in 1:periods) {
    output_gap[t] <- -theta * alpha * (rule_pi - 1) /
      (1 + alpha * rule_y - alpha * delta) *
      (pie[t] - pit[t] + cost_push_shock[t]) +
      1 / (1 + alpha * rule_y - alpha * delta) *
      (1 - theta * alpha * gamma * (rule_pi - 1) /
         (1 + alpha * rule_y - alpha * delta)) *
      (demand_shock[t] - alpha * (monetary_shock[t] + financial_shock[t]))
  }

  # Initialize inflation (pi) vector
  pi <- numeric(periods)

  for (t in 2:periods) {
    pi[t] <- 2.0  # Placeholder value for the inflation rate; adjust as per your model

    if (t >= 3) {
      pie[t] <- credible * pit[t-1] + adaptive * pi[t-1]
      pit[t] <- pit[t-1] + pi_t_change[t]
    }
  }

  # Calculate next period inflation expectations and target
  for (t in 1:periods) {
    pi[t] <- theta * pie[t] +
      (1 - theta) * pit[t] +
      theta * (cost_push_shock[t] +
                 gamma / (1 + alpha * rule_y - alpha * delta) * demand_shock[t] -
                 alpha * gamma / (1 + alpha * rule_y - alpha * delta) * (monetary_shock[t] + financial_shock[t]))
  }

  # Calculate central bank interest rate
  policy_rate <- numeric(periods)
  for (t in 1:periods) {
    policy_rate[t] <- rule_pi * (pi[t] - pit[t]) +
      rule_y * output_gap[t] +
      monetary_shock[t] +
      pit[t] +
      2
  }

  # Calculate real interest rate
  real_rate <- numeric(periods)
  for(t in 1:periods){
    real_rate[t] <- policy_rate[t] - pie[t]
  }

  # Calculate change in lending rate
  for (t in 3:periods) {
    change_lending_rate[t] <- -delta * (output_gap[t] - output_gap[t-1]) +
      (financial_shock[t] - financial_shock[t-1]) +
      (policy_rate[t] - policy_rate[t-1])
  }

  # Calculate lending rate
  lending_rate <- numeric(periods)
  for (t in 1:periods) {
    lending_rate[t] <- policy_rate[t] + 1 - delta * output_gap[t] + financial_shock[t]
  }

  # Calculate spread
  spread <- numeric(periods)
  for (t in 1:periods) {
    spread[t] <- lending_rate[t] - policy_rate[t] - 1
  }

  # Return results as a list
  return(list(
    output_gap = output_gap,
    inflation = pi,
    policy_rate = policy_rate,
    real_rate = real_rate,
    change_lending_rate = change_lending_rate,
    lending_rate = lending_rate,
    spread = spread,
    pie = pie
  ))
}





#' PLot Economy
#'
#' @param results
#' @param periods
#'
#' @return
#' @export
#'
#' @examples
plot_economy <- function(results, periods = 50) {
  # Extract results
  output_gap <- results$output_gap
  inflation <- results$inflation
  pie <- results$pie
  policy_rate <- results$policy_rate
  real_rate <- results$real_rate
  lending_rate <- results$lending_rate
  spread <- results$spread

  # Set up the plotting area: 2 rows and 2 columns
  par(mfrow = c(2, 2))

  # Plot Output Gap
  plot(1:periods, output_gap, type = "l", col = "blue", lwd = 2,
       ylab = "Output Gap", xlab = "Time", main = "Output Gap")

  # Plot Inflation Expectations (pie) and Inflation (pi) in the same graph
  plot(1:periods, pie, type = "l", col = "red", lwd = 2, lty = 2,
       ylab = "Inflation and Inflation Expectations", xlab = "Time", main = "Inflation and Expectations")
  lines(1:periods, inflation, col = "blue", lwd = 2, lty = 1)
  legend("topright", legend = c("Inflation Expectations (pie)", "Inflation"),
         col = c("red", "blue"), lty = c(2, 1), lwd = 2, cex = 0.8)

  # Combine Policy Rate, Real Rate, and Lending Rate into one graph
  # Calculate the y-axis limits based on the range of the three series
  ylim_range <- range(c(policy_rate, real_rate, lending_rate))

  plot(1:periods, policy_rate, type = "l", col = "purple", lwd = 2, lty = 1,
       ylab = "Rates", xlab = "Time", main = "Policy, Real, and Lending Rates",
       ylim = ylim_range)
  lines(1:periods, real_rate, col = "orange", lwd = 2, lty = 2)
  lines(1:periods, lending_rate, col = "brown", lwd = 2, lty = 3)
  legend("topright", legend = c("Policy Rate", "Real Rate", "Lending Rate"),
         col = c("purple", "orange", "brown"), lty = c(1, 2, 3), lwd = 2, cex = 0.8)

  # Plot Spread
  plot(1:periods, spread, type = "l", col = "black", lwd = 2,
       ylab = "Spread", xlab = "Time", main = "Spread ")
}


# Example usage:

# Example of how to call the function with custom parameters
#econ1 <- simulate_economy(adaptive = 0,demand_shocks = c(3), cost_push_shocks = c(3), monetary_shocks = c(3), financial_shocks = c(5), cost_push_shock_magnitude = 4)
#plot_simulation_results(econ1, periods = 50)
