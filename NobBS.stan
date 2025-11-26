data {
  int Today;
  int D;
  int n_subpopulation;
  array[Today, D + 1, n_subpopulation] int n;
  real alpha1_mean_prior;
  real alpha1_prec_prior;
  real alphat_rate_prior;
  real alphat_shape_prior;
  vector[Today] beta_priors;
  real dispersion_prior_shape;
  real dispersion_prior_rate;
}

parameters {
  simplex[D + 1] beta[n_subpopulation];
  matrix[Today, n_subpopulation] z_alpha;
  real<lower = 0, upper = 1000> r;
  real<lower = 0> tau2_alpha;
}

transformed parameters {
  array[n_subpopulation] matrix[Today, D + 1] logged_lambda;
  matrix[Today, n_subpopulation] alpha;

  alpha[1,] = alpha1_mean_prior + z_alpha[1,]./sqrt(alpha1_prec_prior);
  for( t in 2:Today ){
     alpha[t,] = alpha[t-1,] + z_alpha[t,]./sqrt(tau2_alpha);
  }
  
  for (h in 1:n_subpopulation){
    logged_lambda[h] = rep_matrix(alpha[1:Today,h], D + 1) + log(rep_matrix(beta[h], Today))';
  }
}

model {
  //Prior for variance
  tau2_alpha ~ gamma(alphat_shape_prior, alphat_rate_prior);
  
  //Prior for NB rate:
  r ~ gamma(dispersion_prior_shape, dispersion_prior_rate);
  
  // Prior for alpha
  for( t in 1:Today ){
    	z_alpha[t,] ~ normal(0, 1);
  }
     
  // Prior for beta
  for (h in 1:n_subpopulation){
     beta[h] ~ dirichlet(beta_priors);
  }
  
  //likelihood
  for (h in 1:n_subpopulation) {
    for (t in 1:Today) {
        n[t, 1:(D - t + 2), h] ~ neg_binomial_2_log(logged_lambda[h][t, 1:(D - t + 2)], r);
    }
  }
}


