// March 26, 2025 -- FIT 1
// for the bots
// same as paper fit, but no winner effect

data {
  int<lower=0> N;                       // the number of games
  int<lower=0> J;                       // the number of focal players
  array[N] int<lower=0, upper=1> y;     // the outcome of each game
  array[N] int<lower=1, upper = J> id;  // indicating focal player involved
  vector[N] colour;                     // the colour of the focal player
  vector[N] elo;                        // diff in elo scores between players
}

parameters {
  real<lower=0> sigma_g1;              // sd of gamma1
  real<lower=0> sigma_g2;              // sd of gamma2
  real<lower=0> sigma_a;               // sd of alphas
  
  array[J] real alpha;                 // player effects
  real gamma1;                         // colour effect
  real gamma2;                         // rating effect
}

model {
  sigma_g1 ~ normal(0, 1);             // prior for sd of gamma1
  sigma_g2 ~ normal(0, 1);             // prior for sd of gamma2
  sigma_a ~ normal(0, 1);              // prior for sd of alpha
  
  alpha ~ normal(0, sigma_a);          // prior for alpha
  gamma1 ~ normal(0, sigma_g1);        // prior for gamma1
  gamma2 ~ normal(0, sigma_g2);        // prior for gamma2
  
  
  vector[N] pred;
  for(i in 1:N){
    pred[i] = alpha[id[i]] +
    gamma1 * colour[i] + 
    gamma2 * elo[i];
  }
  y ~ bernoulli_logit(pred);
}

