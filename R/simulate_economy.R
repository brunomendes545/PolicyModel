#' Simulate an Monetary Policy
#'
#' @param periods - Number of periods to simulate (Default is 50)
#' @param gamma - Parameters of the Phillips curve (Default is 0.75)
#' @param alpha - Parameters of the IS curve (Default is 0.8)
#' @param rule_pi - Parameters of the interest rate rule; (Response to inflation
#'   (relative to target)) (Default is 1.5)
#' @param rule_y - Parameters of the interest rate rule; (Response to output gap
#'   ) (Default is 0.5)
#' @param delta - Financial Markets frictions (Default is 1)
#' @param adaptive - Weight on past inflation (1=adaptive expectations;
#'   0=rational expectations) (Default is 1)
#' @param cost_push_shock_persistance - Persistence of cost push shocks; Range
#'   from 0 to 1; (Default is 0.5)
#' @param demand_shock_persistance - what is it ? what is the range?
#' @param monetary_shock_persistance - what is it ? what is the range?
#' @param financial_shock_persistance - what is it ? what is the range?
#' @param demand_shocks - what is it ? what is the range?
#' @param cost_push_shocks - what is it ? what is the range?
#' @param monetary_shocks - what is it ? what is the range?
#' @param financial_shocks - what is it ? what is the range?
#' @param demand_shock_magnitude - what is it ? what is the range?
#' @param cost_push_shock_magnitude - what is it ? what is the range?
#' @param monetary_shock_magnitude - what is it ? what is the range?
#' @param financial_shock_magnitude - what is it ? what is the range?
#'
#' @return a named list
#' @export
#'
#' @examples
#' econ1 <- simulate_economy(
#'   cost_push_shock_persistance = 0.5,
#'   demand_shock_persistance = 0.75,
#'   monetary_shock_persistance = 0.5,
#'   financial_shock_persistance = 0.5,
#'   demand_shocks = c(3, 10),
#'   cost_push_shocks = c(5, 15),
#'   monetary_shocks = c(7),
#'   financial_shocks = c(9),
#'   demand_shock_magnitude = c(-2, 1),
#'   cost_push_shock_magnitude = c(1.5, -0.5),
#'   monetary_shock_magnitude = c(1),
#'   financial_shock_magnitude = (0.5)
#' )

simulate_economy <- function(
    periods = 50,
    gamma = 0.75,  # PC parameters
    alpha = 0.8,   # IS curve parameters
    rule_pi = 1.5, # Response to inflation (relative to target)
    rule_y = 0.5,  # Response to output gap
    delta = 1,     # Financial Markets frictions
    adaptive = 1,  # Weight on past inflation (1=adaptive expectations)
    cost_push_shock_persistance = 0.5,
    demand_shock_persistance = 0.5,
    monetary_shock_persistance = 0.5,
    financial_shock_persistance = 0.5,
    demand_shocks = c(),  # Periods where demand shocks occur
    cost_push_shocks = c(),  # Periods where cost-push shocks occur
    monetary_shocks = c(),  # Periods where monetary shocks occur
    financial_shocks = c(),  # Periods where financial shocks occur
    demand_shock_magnitude = c(1),  # Magnitude of demand shocks
    cost_push_shock_magnitude = c(1),  # Magnitude of cost-push shocks
    monetary_shock_magnitude = c(1),  # Magnitude of monetary shocks
    financial_shock_magnitude = c(1)  # Magnitude of financial shocks
) {

  # Check that periods is an integer greater than 3
  if (floor(periods) != periods) {
    stop("periods must be an integer.")
  }
  if (periods <= 3) {
    stop("periods must be greater than 3.")
  }

  # Calculate theta based on the formula provided
  theta <- (1 + alpha * rule_y - alpha * delta) /
    (1 + alpha * rule_y - alpha * delta + alpha * gamma * (rule_pi - 1))

  credible <- 1 - adaptive  # Weight on inflation target (1=perfect credibility)

  # Initialize shock vectors
  cost_push_shock <- rep(0, periods)
  demand_shock <- rep(0, periods)
  monetary_shock <- rep(0, periods)
  financial_shock <- rep(0, periods)
  pi_t_change <- rep(0, periods)

  # Initialize variables for simulation
  pie <- rep(2.0, periods)  # Inflation expectations
  pit <- rep(2.0, periods)  # Inflation target
  # Initialize inflation (pi) vector
  pi <- numeric(periods)
  pi <- rep(2.0, periods)   # Inflation
  change_lending_rate <- rep(0.0, periods)  # Change in lending rate

  # Set shocks based on input periods and magnitudes
  if (length(demand_shocks) > 0) {
    demand_shock[demand_shocks] <- demand_shock_magnitude
  }
  if (length(cost_push_shocks) > 0) {
    cost_push_shock[cost_push_shocks] <- cost_push_shock_magnitude
  }
  if (length(monetary_shocks) > 0) {
    monetary_shock[monetary_shocks] <- monetary_shock_magnitude
  }
  if (length(financial_shocks) > 0) {
    financial_shock[financial_shocks] <- financial_shock_magnitude
  }

  # Apply persistence for shocks
  for (t in 2:periods) {
    demand_shock[t] <- ifelse(demand_shock[t] == 0, demand_shock[t-1] * demand_shock_persistance, demand_shock[t])
    cost_push_shock[t] <- ifelse(cost_push_shock[t] == 0, cost_push_shock[t-1] * cost_push_shock_persistance, cost_push_shock[t])
    monetary_shock[t] <- ifelse(monetary_shock[t] == 0, monetary_shock[t-1] * monetary_shock_persistance, monetary_shock[t])
    financial_shock[t] <- ifelse(financial_shock[t] == 0, financial_shock[t-1] * financial_shock_persistance, financial_shock[t])
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


  # Loop through periods and update inflation, expectations, and targets
  for (t in 1:periods) {
    # Update inflation with financial frictions
    pi[t] <- theta * pie[t] +
      (1 - theta) * pit[t] +
      theta * (cost_push_shock[t] +
                 gamma / (1 + alpha * rule_y - alpha * delta) * demand_shock[t] -
                 alpha * gamma / (1 + alpha * rule_y - alpha * delta) * (monetary_shock[t] + financial_shock[t]))

    # Update inflation expectations and targets
    if (t > 2) {  # Skip
      pie[t] <- credible * pit[t] + adaptive * pi[t-1]
      pit[t] <- pit[t-1] + pi_t_change[t]
    }
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

  results <- list(
    output_gap = output_gap,
    inflation = pi,
    policy_rate = policy_rate,
    real_rate = real_rate,
    change_lending_rate = change_lending_rate,
    lending_rate = lending_rate,
    spread = spread,
    pie = pie,
    periods = periods
  )

  class(results) <- c("simulate_economy",class(results))

  # Return results as a list
  return(results)
}





#' Plot Economy
#'
#' plot generic for simulate_economy class
#'
#' @param x - an object of class simulate_economy
#' @param ... used for future expansions
#'
#' @importFrom graphics legend lines par
#'
#' @return a plot
#' @export
#'
#' @examples
#' econ1 <- simulate_economy(
#'   cost_push_shock_persistance = 0.5,
#'   demand_shock_persistance = 0.75,
#'   monetary_shock_persistance = 0.5,
#'   financial_shock_persistance = 0.5,
#'   demand_shocks = c(3, 10),
#'   cost_push_shocks = c(5, 15),
#'   monetary_shocks = c(7),
#'   financial_shocks = c(9),
#'   demand_shock_magnitude = c(-2, 1),
#'   cost_push_shock_magnitude = c(1.5, -0.5),
#'   monetary_shock_magnitude = c(1),
#'   financial_shock_magnitude = c(0.5)
#' )
#'
#' plot(econ1)
#'
plot.simulate_economy <- function(x, ...) {

  results <- x

  # Extract results
  output_gap <- results$output_gap
  inflation <- results$inflation
  pie <- results$pie
  policy_rate <- results$policy_rate
  real_rate <- results$real_rate
  lending_rate <- results$lending_rate
  spread <- results$spread
  periods <- results$periods

  # Set up the plotting area: 2 rows and 2 columns
  par(mfrow = c(2, 2))

  # Plot Output Gap
  plot(1:periods, output_gap, type = "l", col = "blue", lwd = 2,
       ylab = "Output Gap", xlab = "Period", main = "Output Gap")

  # Plot Inflation Expectations (pie) and Inflation (pi) in the same graph
  ylim_range1 <- range(c(pie,inflation))
  plot(1:periods, pie, type = "l", col = "red", lwd = 2, lty = 2,
       ylab = "Inflation and Inflation Expectations", xlab = "Period", main = "Inflation and Expectations",
       ylim = ylim_range1)
  lines(1:periods, inflation, col = "blue", lwd = 2, lty = 1)
  legend("topright", legend = c("Inflation Expectations (pie)", "Inflation"),
         col = c("red", "blue"), lty = c(2, 1), lwd = 2, cex = 0.8)

  # Combine Policy Rate, Real Rate, and Lending Rate into one graph
  # Calculate the y-axis limits based on the range of the three series
  ylim_range <- range(c(policy_rate, real_rate, lending_rate))
  plot(1:periods, policy_rate, type = "l", col = "purple", lwd = 2, lty = 1,
       ylab = "Rates", xlab = "Period", main = "Policy, Real, and Lending Rates",
       ylim = ylim_range)
  lines(1:periods, real_rate, col = "orange", lwd = 2, lty = 2)
  lines(1:periods, lending_rate, col = "brown", lwd = 2, lty = 3)
  legend("topright", legend = c("Policy Rate", "Real Rate", "Lending Rate"),
         col = c("purple", "orange", "brown"), lty = c(1, 2, 3), lwd = 2, cex = 0.8)

  # Plot Spread
  plot(1:periods, spread, type = "l", col = "black", lwd = 2,
       ylab = "Spread", xlab = "Time", main = "Spread ")
}
