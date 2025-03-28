// March 28, 2025
// player and opponent effects with paired interaction
// identical to fit 5 but no player/opponent effect

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
  /// first layer ///
  real<lower=0> sigma_1;               // sd of mu_beta
  
  /// second layer /// 
  real mu_beta;                        // mean of nu_2
  real<lower=0> sigma_g1;              // sd of gamma1
  
  /// third layer ///
  array[J] real beta;                  // winner effects
  real gamma1;                         // effect of colour
  vector[J*(J-1)] delta;               // pairing parameters - just initialize the parameters you need - only Jx(J-1)
}

transformed parameters{
  matrix[J, J] delta_trans; {          // transformed matrix
  int count = 1;
  for (i in 1:J) { //rows
  for (j in 1:J) { //cols
    if (i == j){ 
      delta_trans[i,j] = 0; // make diagonal 0
    }
    if (j > i) {
      delta_trans[i,j] = delta[count]; // effect of player i vs player j
    }
    else {
      delta_trans[i,j] = -delta_trans[j,i]; // the negative of the effect of reverse pairing
    }
    count += 1;
  }
  }
  }
}

model {
  /// first layer ///
  sigma_1 ~ normal(0, 1);              // prior for sd of mu_beta
  
  /// second layer ///
  mu_beta ~ normal(0, sigma_1);        // prior for population winner effect
  sigma_g1 ~ normal(0, 1);             // prior for sd of gamma1
  
  /// third layer ///
  beta ~ normal(mu_beta, 1);           // prior for winner effect
  gamma1 ~ normal(0, sigma_g1);        // prior for gamma1
  to_vector(delta) ~ normal(0, 2.5);   // prior for paired effects - can adjust the variance later
  
  /// make diagonal all 0s
  /// make symmetric but negative on one side, positive on other...
  
  vector[N] pred;
  for(i in 1:N){
    pred[i] = delta_trans[id_focal[i], id_opp[i]] + beta[id_focal[i]] * win_prop[i] +
    gamma1 * colour[i];
  }
  y ~ bernoulli_logit(pred);
}

