// Model March 6, 2025
// for the bots
// removing rating effect and including an opponent effect
// for player i vs player j, prob = logistic(alpha_i - alpha_j + ...)
// ignoring correlation for now 


data {
  int<lower=0> N;                            // the number of games
  int<lower=0> J;                            // the number of focal players
  array[N] int<lower=0, upper=1> y;          // the outcome of each game
  array[N] int<lower=1, upper = J> id_focal; // indicating focal player involved
  array[N] int<lower=1, upper = J> id_opp;   // indicating the opponent of focal player
  vector[N] colour;                          // the colour of the focal player
  vector[N] win_prop;                        // current win ratio for focal
}


parameters {
  real<lower=0> sigma_b;               // sd of mu_beta
  real<lower=0> sigma_g;               // sd of gamma
  real<lower=0> sigma_a;               // sd of mu_beta
  
  real mu_beta;                        // population average winner effect
  
  array[J] real alpha;                 // player effect
  array[J] real beta;                  // winner effect
  real gamma;                          // colour effect
}


model {
  sigma_b ~ normal(0, 1);              // prior for sd of mu_beta
  sigma_g ~ normal(0, 1);              // prior for sd of gamma
  sigma_a ~ normal(0, 1);              // prior for sd of alpha
  
  mu_beta ~ normal(0, sigma_b);        // prior for population winner effect
  
  beta ~ normal(mu_beta, sigma_b);     // prior for beta
  alpha ~ normal(0, sigma_a);          // prior for alpha
  gamma ~ normal(0, sigma_g);          // prior for gamma
  
  
  vector[N] pred;
  for(i in 1:N){
    pred[i] = alpha[id_focal[i]] - alpha[id_opp[i]] + beta[id_focal[i]] * win_prop[i] +
    gamma * colour[i];
  }
  y ~ bernoulli_logit(pred);
}

