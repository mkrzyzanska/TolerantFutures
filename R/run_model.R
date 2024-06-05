# load libraries
library(here)

# Load data and clean
d2<-read.csv(here("data","Ch2final2.csv"),row.names=1) # read data from csv
d2[d2$conceptPresent=="the Suffragettes",]$conceptPresent<-'The suffragettes' # Fix the capitalisation on suffragettes
d2[d2$valuePresent>=1,]$valuePresent <- 0.99999999 # Change value 1 and higher to be withn the bounds for beta distribution
d2[d2$valuePresent<=0,]$valuePresent <- 0.00000001 # Change value 0 and lower to be withn the bounds for beta distribution

# Put data in the right format for stan:
    d<-d2
    d<-list(
    Y=d$valuePresent,
    X=standardize(d$valuePast),
    C=as.integer(as.factor(d$conceptPresent)),
    N=nrow(d),
    NC=length(unique(d$conceptPresent)))

# Run simple model:
    
    stan_model<- here("stan","simple.stan")
    m <- stan(file=here(stan_model , data=d,iter=5000, chains=4,cores=4)
    saveRDS(m, here("models","simple.rds"))