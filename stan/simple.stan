data{
   int<lower=0> N;             // Number of observations
   vector<lower=0, upper=1>[N] Y;  // Outcome variable (binary)
   vector[N] X;                // Predictor variable (value past)
   int<lower=1> NC;            // Number of categories
   int<lower=1, upper=NC> C[N];
}
parameters{
     vector[NC] alpha;
     vector[NC] beta;   
     real<lower=0> phi;
}

model{
    vector[N] mu;
    alpha[C] ~ student_t(3, 0, 2.5);
    beta[C] ~ student_t(3, 0, 2.5);
    phi ~ gamma(0.01, 0.01);
    
    //Compute mu
    for ( i in 1:N ) {
        mu[i] = inv_logit(alpha[C[i]] + beta[C[i]] * X[i]);
    }
    // Likelihood
   for (n in 1:N) {
    Y[n] ~ beta( mu[n]*phi,(1-mu[n])*phi);
  }
}
generated quantities{
vector[N] mu;

//Compute mu
    for ( i in 1:N ) {
        mu[i] = inv_logit(alpha[C[i]] + beta[C[i]] * X[i]);
    }
// Log-likelihood for WAIC
vector[N] log_lik;
for ( i in 1:N ) log_lik[i] = beta_lpdf( Y[i] | mu[i]*phi ,(1-mu[i])*phi );

// Posterior predictions for the check
array[N] real y_rep = beta_rng(mu*phi ,(1-mu)*phi);

}