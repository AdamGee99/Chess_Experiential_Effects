// March, 26 2025 -- FIT 2
// identical to model fit in paper, just renaming for completness
// update diag_pre_multilpy() to quad_form_diag() 

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
  real mu_beta;                        // mean of nu_2
  
  /// third layer ///
  vector[2] nu;                        // location of beta[ , j] - the mu vector in paper
  vector<lower=0>[2] tau;              // scale of beta[ , j], sd of effects - tau vector in paper
  cholesky_factor_corr[2] L_Omega;     // Cholesky of correlation of beta[ , j]
  real<lower=0> sigma_g1;              // sd of gamma1
  real<lower=0> sigma_g2;              // sd of gamma2
  
  /// fourth layer ///
  matrix[2, J] beta_std;               // standard beta (beta - nu) / Sigma - why are we standardizing here...
  real gamma1;                         // effect of colour
  real gamma2;                         // effect of elo difference
}

transformed parameters {
  matrix[2, J] beta = rep_matrix(nu, J) + quad_form_diag(L_Omega, tau) * beta_std;
}

model {
  /// first layer ///
  sigma_1 ~ normal(0, 1);              // prior for sd of mu_beta
  
  /// second layer ///
  mu_beta ~ normal(0, sigma_1);        // prior for population winner effect
  
  /// third layer ///
  nu[1] ~ normal(0, 1);
  nu[2] ~ normal(mu_beta, 1);          // standardized so sds here fixed
  tau ~ inv_gamma(1, 1);               // prior for sd of both random effects - I guess this means both tau_1 and tau_2 have this prior
  L_Omega ~ lkj_corr_cholesky(2);      // prior for correlation matrix
  sigma_g1 ~ normal(0, 1);             // prior for sd of gamma1
  sigma_g2 ~ normal(0, 1);             // prior for sd of gamma2
  
  /// fourth layer ///
  gamma1 ~ normal(0, sigma_g1);        // prior for gamma1
  gamma2 ~ normal(0, sigma_g2);        // prior for gamma2
  to_vector(beta_std) ~ normal(0, 1);  // beta[ , j] ~ multi_normal(nu, Sigma) - guess this also means that all x_ij in beta_std are N(0,1)
  
  vector[N] pred;
  for(i in 1:N){
    pred[i] = beta[1, id[i]] + beta[2, id[i]] * win_prop[i] +
    gamma1 * colour[i] + gamma2 * elo[i];
  }
  y ~ bernoulli_logit(pred);
}

