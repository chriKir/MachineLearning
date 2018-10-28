len = 30;
lambda = 1;
order = 5;
x <- vector("numeric", len);
y <- vector("numeric", len);

fun <- function(x){2.5*x^4 - 2*x^3 + x^2 - 2*x};

for(i in 1:len){
  ran <- dnorm(i);
  ranNr <- sample(0:10, 1)/10;
  x[i] <- ranNr;
  y[i] <- fun(ranNr) + rnorm(1)/20;
}

plot(x,y);
z <- seq(0,1,0.01)
lines(z, fun(z), col="green", type="l")

basisFunction <- function(x, i){x^i}

Phi <- matrix(0, len, order);

for(i in 1:len){
  for(j in 0:(order-1)){
    Phi[i,j+1] <- basisFunction(x[i], j);
  }
}

Phi_T <- t(Phi)
Phi_reg <- Phi_T%*%Phi #+ lambda*diag(order)
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
