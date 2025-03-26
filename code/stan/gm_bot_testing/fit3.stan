// Model March, 26 2025 -- FIT 3
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
  // bottom layer
  real<lower=0> sigma_1;               // sd of mu_beta
  real<lower=0> sigma_g1;              // sd of gamma1
  
  // middle layer
  real mu_beta;                        // population average winner effect
  vector<lower=0>[3] tau;              // sd of alpha, beta, gamma2 (rating) effect, scale of beta[ , j]
  cholesky_factor_corr[2] L_Omega;     // Cholesky of correlation of beta[ , j]
  
  //top layer
  real gamma1;                         // colour effect
  real gamma2;                         // rating effect
  vector[3] nu;                        // location of beta[ , j]
  matrix[3, J] beta_std;               // standard beta (beta - nu) / Sigma
}

transformed parameters {
  matrix[3, J] beta = rep_matrix(nu, J)
                      + diag_pre_multiply(tau, L_Omega) * beta_std;
}

model {
  // bottom layer
  sigma_1 ~ normal(0, 1);              // prior for sd of sigma_1
  sigma_g1 ~ normal(0, 1);             // prior for sd of gamma1
  
  // middle layer
  mu_beta ~ normal(0, sigma_1);        // prior for population winner effect
  
  // top layer
  nu[1] ~ normal(0, 1);
  nu[2] ~ normal(mu_beta, 1);          // standardized so sds here fixed
  nu[3] ~ normal(0, 1); 
  tau ~ inv_gamma(1, 1);               // prior for sd of player, colour, rating effects
  
  gamma1 ~ normal(0, sigma_g1);        // prior for gamma1
  gamma2 ~ normal(0, tau[3]);          // prior for gamma2

  
  L_Omega ~ lkj_corr_cholesky(2);      // prior for correlation matrix
  to_vector(beta_std) ~ normal(0, 1);  // beta[ , j] ~ multi_normal(nu, Sigma)
  vector[N] pred;
  for(i in 1:N){
    pred[i] = beta[1, id[i]] + beta[2, id[i]] * win_prop[i] +
    gamma1 * colour[i] + gamma2 * elo[i];
  }
  y ~ bernoulli_logit(pred);
}

