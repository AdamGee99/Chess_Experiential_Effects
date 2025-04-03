// April 1, 2025 -- FIT 2 TEST
// extention to model fit in paper (fit 2)
// removing extra mu_beta hiearchy
// adding a parameter for mean of alphas (mu_alpha) and a prior for its variance (sigma_a)
// renaming sigma_1 to sigma_b

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
  real<lower=0> sigma_a;               // sd of mu_alpha
  real<lower=0> sigma_b;               // sd of mu_beta
  
  /// second layer /// 
  real mu_alpha;                       // mean of alphas
  real mu_beta;                        // mean of betas
  
  vector<lower=0>[2] tau;              // scale of alphas and betas
  cholesky_factor_corr[2] L_Omega;     // Cholesky of correlation between alphas and betas
  real<lower=0> sigma_g1;              // sd of gamma1
  real<lower=0> sigma_g2;              // sd of gamma2
  
  /// third layer ///
  matrix[2, J] beta_std;               // standard beta (beta - nu) / Sigma - why are we standardizing here...
  real gamma1;                         // effect of colour
  real gamma2;                         // effect of elo difference
}

transformed parameters {
  vector[2] nu = [mu_alpha, mu_beta]'; //mean vector of MVN prior of alpha and betas
  matrix[2, J] beta = rep_matrix(nu, J) + diag_pre_multiply(tau, L_Omega) * beta_std; // use diag_pre_multiply() so it's sd instead of variance
}

model {
  /// first layer ///
  sigma_a ~ normal(0, 1);              // prior for sd of mu_alpha
  sigma_b ~ normal(0, 1);              // prior for sd of mu_beta
  
  /// second layer ///
  mu_alpha ~ normal(0, sigma_a);       // prior for population player effect
  mu_beta ~ normal(0, sigma_b);        // prior for population winner effect
  
  tau ~ inv_gamma(1, 1);               // prior for sd of both random effects - I guess this means both tau_1 and tau_2 have this prior
  L_Omega ~ lkj_corr_cholesky(2);      // prior for correlation matrix
  sigma_g1 ~ normal(0, 1);             // prior for sd of gamma1
  sigma_g2 ~ normal(0, 1);             // prior for sd of gamma2
  
  /// third layer ///
  gamma1 ~ normal(0, sigma_g1);        // prior for gamma1
  gamma2 ~ normal(0, sigma_g2);        // prior for gamma2
  to_vector(beta_std) ~ normal(0, 1);  // std normal prior for all x_ij in beta_std
  
  vector[N] pred;
  for(i in 1:N){
    pred[i] = beta[1, id[i]] + beta[2, id[i]] * win_prop[i] +
    gamma1 * colour[i] + gamma2 * elo[i];
  }
  y ~ bernoulli_logit(pred);
}

