data {
  int<lower=1> T;                // pre-treatment time points
  int<lower=0> T_new;            // post-treatment time points
  int<lower=1> K;                // number of donors
  vector[T] y;                   // centered outcome (pre)
  matrix[T, K] X;                // donor matrix (pre)
  matrix[T_new, K] X_new;        // donor matrix (post)
}

parameters {
  vector[K] beta;
  real<lower=0> sigma_y;
  real<lower=0> sigma_mu;

  real mu0;              // initial level
  vector[T] mu_raw;      // standard normal innovations
}

transformed parameters {
  vector[T] mu;

  mu[1] = mu0 + sigma_mu * mu_raw[1];

  for (t in 2:T)
    mu[t] = mu[t-1] + sigma_mu * mu_raw[t];
}

model {

  // Priors
  beta ~ normal(0, 0.5);

  sigma_y ~ normal(0, 0.1);
  sigma_mu ~ normal(0, 0.05);   // slightly tighter helps

  mu0 ~ normal(0, 0.1);         // independent prior on initial level
  mu_raw ~ normal(0, 1);

  // Likelihood
  y ~ normal(mu + X * beta, sigma_y);
}


generated quantities {

  vector[T] y_rep;
  vector[T] log_lik;
  vector[T_new] y_forecast;
  vector[T_new] mu_forecast;

  // Posterior predictive (pre)
  for (t in 1:T) {
    y_rep[t] = normal_rng(mu[t] + X[t] * beta, sigma_y);
    log_lik[t] = normal_lpdf(y[t] | mu[t] + X[t] * beta, sigma_y);
  }

  // Forecast latent state forward
  mu_forecast[1] = mu[T] + normal_rng(0, sigma_mu);

  for (t in 2:T_new)
    mu_forecast[t] = mu_forecast[t-1] + normal_rng(0, sigma_mu);

  // Forecast outcomes
  for (t in 1:T_new)
    y_forecast[t] =
      normal_rng(mu_forecast[t] + X_new[t] * beta, sigma_y);
}
