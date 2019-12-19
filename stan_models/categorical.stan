/* From the Stan User's Manual v2.21

 */

data {
  int<lower = 2> K; // n outcomes
  int N; // n obs
  int<lower = 1> D; // n predictors
  int<lower = 1, upper = K> y[N]; // categorical outcome
  matrix[N, D] x;
  int M; // n years
  matrix[N, M] detYear;
}

parameters {
  matrix[D, K] beta;
  matrix[M-1, K] beta_year_raw;
}
transformed parameters{
  matrix[M, K] beta_year;

  for(k in 1:K){
	beta_year[,k] = append_row(beta_year_raw[,k], -sum(beta_year_raw[,k]));
  }

  
}

model {
  matrix[N, K] x_beta = x * beta + detYear * beta_year;

  to_vector(beta) ~ normal(0, 2);
  to_vector(beta_year_raw) ~ normal(0, 2);
  
  for (n in 1:N) {
	y[n] ~ categorical_logit(x_beta[n]');
  }
}

generated quantities{
  vector[K] base_probs = softmax(to_vector(beta[1,]));
  vector[K] Bchn_probs = softmax(to_vector(beta[1,] + beta[2,]));
  
  
}
