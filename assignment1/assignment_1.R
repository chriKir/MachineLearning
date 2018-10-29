len = 15;
lambda =7;
order = 9;
x <- vector("numeric", len);
y <- vector("numeric", len);

#fun <- function(x){3*x^5 + 2*x^4 - 2*x^3 + x^2 + 2*x};
fun <- function(x){ 0.5*x^4 + 4*x^3 - 5*x^2 + 10};

for(i in 1:len){
  ran <- dnorm(i);
  ranNr <- runif(1, -5.0, 5.0);
  x[i] <- ranNr;
  y[i] <- fun(ranNr) + rnorm(1)*20;
}

plot(x,y);
z <- seq(-5,5,0.1)
lines(z, fun(z), col="green", type="l")

basisFunction <- function(x, i){x^i}

Phi <- matrix(0, len, order);

for(i in 1:len){
  for(j in 0:(order-1)){
    Phi[i,j+1] <- basisFunction(x[i], j);
  }
}

Phi_T <- t(Phi)
Phi_reg <- Phi_T%*%Phi + lambda*diag(order)
Phi_t <- solve(Phi_reg)%*%Phi_T
w <- Phi_t%*%y

reg <- function(x){
  y = 0;
  for(i in 0:(order-1)){
    y = y + w[i+1]*x^i;
  }
  return(y)
}
lines(z, reg(z), col="red", type="l")

errorFunction <- function(w){
  result = 0;
  for(n in 1:len){
    result = result + (reg(x[n]) - fun(x[n]))^2 + lambda/2 * (t(w)*w)
  }
  return(result)
}

for(i in 1:(len/3)){
  trainingValues = vector("numeric", len-3);
  
}
