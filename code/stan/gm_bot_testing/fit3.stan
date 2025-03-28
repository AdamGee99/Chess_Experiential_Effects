// March, 26 2025 -- FIT 3
// same as fit2 but adding correlation between winner effect and rating effect

data {
  int<lower=0> N;                      // the number of games
  int<lower=0> J;                      // the number of focal players
  array[N] int<lower=0, upper=1> y;    // the outcome of each game
  array[N] int<lower=1, upper = J> id; // indicating focal player involved
  vector[N] colour;                    // the colour of the focal player
  vector[N] elo;                       // diff in elo scores between players
  vector[N] win_prop;                  // current win ratio for focal
}

parameters {
  /// first layer ///
  real<lower=0> sigma_1;               // sd of mu_beta
  
  /// second layer ///
  real mu_beta;                        // population average winner effect
  
  /// third layer ///
  vector[3] nu;                        // location of beta[ , j]
  vector<lower=0>[3] tau;              // sd of alpha, beta, gamma2 (rating) effect, scale of beta[ , j]
  cholesky_factor_corr[3] L_Omega;     // Cholesky of correlation of beta[ , j]
  real<lower=0> sigma_g1;              // sd of gamma1
  
  /// fourth layer ///
  real gamma1;                         // colour effect
  matrix[3, J] beta_std;               // standard beta (beta - nu) / Sigma
}

transformed parameters {
  matrix[3, J] beta = rep_matrix(nu, J) + quad_form_diag(L_Omega, tau) * beta_std;
}

model {
  /// first layer ///
  sigma_1 ~ normal(0, 1);              // prior for sd of mu_beta
  
  /// second layer ///
  mu_beta ~ normal(0, sigma_1);        // prior for population winner effect
  
  /// third layer ///
  nu[1] ~ normal(0, 1);                // prior for mean of player effect
  nu[2] ~ normal(mu_beta, 1);          // prior for mean of winner effect
  nu[3] ~ normal(0, 1);                // prior for mean of rating effect
  tau ~ inv_gamma(1, 1);               // prior for sd of player, colour, rating effects
  L_Omega ~ lkj_corr_cholesky(2);      // prior for correlation matrix
  sigma_g1 ~ normal(0, 1);             // prior for sd of colour effect
  
  /// fourth layer ///
  gamma1 ~ normal(0, sigma_g1);        // prior for gamma1
  to_vector(beta_std) ~ normal(0, 1);  // beta[ , j] ~ multi_normal(nu, Sigma)
  
  vector[N] pred;
  for(i in 1:N){
    pred[i] = beta[1, id[i]] + beta[2, id[i]] * win_prop[i] +
    gamma1 * colour[i] + beta[3, id[i]] * elo[i];
  }
  y ~ bernoulli_logit(pred);
}

