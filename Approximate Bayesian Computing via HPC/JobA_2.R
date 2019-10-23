# prior simulation function
simulate_prior <- function(n){
  data.frame(beta0 = rnorm(n, sd = 5),
             beta1 = rnorm(n, sd = 5),
             rho = rbeta(n, 1, 5),
             pi_EI = rbeta(n, 115,885),
             pi_IR = rbeta(n, 200,800),
             S01 = rpois(n,50000),
             S02 = rpois(n,50000),
             S03 = rpois(n,10000),
             E01 = rpois(n,1),
             E02 = rpois(n,1),
             E03 = rpois(n,1),
             I01 = rpois(n,1),
             I02 = rpois(n,1),
             I03 = rpois(n,1))
}

getX <- function(ntpts, nloc){
  # Intercept and intervention term
  X0 <- cbind(1, cumsum(1:ntpts > round(ntpts/2)))
  X0[,2] <- X0[,2]/max(X0[,2])
  X0[rep(1:ntpts, nloc),]
}

getD <- function(nloc){
  # constant overall contact:
  D <- 1-diag(nloc)
  D/(nloc-1)
}

# Num time points = 150
simulate_data <- function(single_par, 
                          Nt = 150, 
                          N = c(51000, 51000, 11000)){
  # Set up
  
  X <- getX(Nt, length(N))
  D <- getD(length(N))
  
  S = E = I = R = Estar = Istar = Rstar = matrix(NA, nrow = 150 + 1, ncol = 3)
  S[1,] = with(single_par, c(S01, S02, S03))
  E[1,] = with(single_par, c(E01, E02, E03))
  I[1,] = with(single_par, c(I01, I02, I03))
  R[1,] = N - S[1,] - E[1,] - I[1,]
  if (any(R[1,]  < 0)){
    # Bad starting value, return bad result
    S[] <- Inf
    E[] <- Inf
    I[] <- Inf
    R[] <- Inf
    Estar[] <- Inf
    Istar[] <- Inf
    Rstar[] <- Inf
    return(list(S=S[1:Nt,],
                E=E[1:Nt,],
                I=I[1:Nt,],
                R=R[1:Nt,],
                Estar=Estar[1:Nt,],
                Istar=Istar[1:Nt,],
                Rstar=Rstar[1:Nt,],
                param = single_par))
  }
  
  beta = with(single_par, c(beta0, beta1))
  rho = single_par$rho
  theta_components = exp(matrix(X %*% beta, ncol = length(N)))
  for (i in 1:Nt){
    # Draw transition counts
    theta <- I[i,]*theta_components[i,]/N
    pi_SE = 1-exp(-theta - t(rho*(D %*% theta)))
    Estar[i,] = rbinom(n = rep(1, ncol(S)), size = S[i,], prob = pi_SE)
    Istar[i,] = rbinom(n = rep(1, ncol(S)), size = E[i,], prob = single_par$pi_EI)
    Rstar[i,] = rbinom(n = rep(1, ncol(S)), size = I[i,], prob = single_par$pi_IR)
    # Update compartments
    S[i+1,] = S[i,] - Estar[i,]
    E[i+1,] = E[i,] - Istar[i,] + Estar[i,]
    I[i+1,] = I[i,] - Rstar[i,] + Istar[i,]
    R[i+1,] = R[i,] + Rstar[i,]
  }
  list(S=S[1:Nt,],
       E=E[1:Nt,],
       I=I[1:Nt,],
       R=R[1:Nt,],
       Estar=Estar[1:Nt,],
       Istar=Istar[1:Nt,],
       Rstar=Rstar[1:Nt,],
       param = single_par)
  
}


# True data
set.seed(123123)
true_data <- simulate_data(single_par = data.frame(beta0=-1, beta1=-2,
                                                   rho=0.1,
                                                   pi_EI=0.14,pi_IR=0.2,
                                                   S01=50000,S02=50000,S03=10000,
                                                   E01=0,E02=0,E03=5,
                                                   I01=0,I02=0,I03=0))
true_Istar <- true_data$Istar


#col1 <- rgb(0.4980392, 0.7882353, 0.4980392,1)
#col2 <- rgb(0.7450980, 0.6823529, 0.8313725,1)
#col3 <- rgb(0.9921569, 0.7529412, 0.5254902,1)
#plot(true_Istar[,1], type = "l", ylim = c(0,max(true_Istar)*1.1), 
#     col = col1, lwd = 2, 
#     main = "New Infection Counts", xlab = "Time", ylab = "Count")
#lines(true_Istar[,2], type = "l", col = col2, lwd = 2)
#lines(true_Istar[,3], type = "l", col =col3, lwd = 2)
#legend(x=10,y = 25, legend = paste0("Location ", 1:3), 
#       lwd=rep(3,3), col = c(col1,col2,col3))

# simulate data
#system.time({
set.seed(12323)  
pars <- simulate_prior(2000000)
j <-0
  for (i in c(1:nrow(pars))) {
    if (i %% 100 ==0){print(i)}
    # set.seed(100+i)
    sim.i <- simulate_data(pars[i,])$Istar
    e <- sqrt(sum((sim.i-true_Istar)^2))
    if (e<100) {
      j <- j+1
      write.csv(sim.i, paste0("JobA.A_sample",j,".csv"))
    }
  }
  
 

#plot(density(keepers$beta0, bw = 1), main = "Density Estimate for Beta-0")
#abline(v = true_data$param$beta0, col = "red")

#plot(density(keepers$beta1, bw = 1), main = "Density Estimate for Beta-1")
#abline(v = true_data$param$beta1, col = "red")
