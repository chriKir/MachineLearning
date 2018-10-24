len = 30;
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

phi_0 <- function(x){1}
phi_1 <- function(x){x}
phi_2 <- function(x){x^2}
phi_3 <- function(x){x^3}
phi_4 <- function(x){x^4}

vector_phi_0 <- vector("numeric", len)
vector_phi_1 <- vector("numeric", len)
vector_phi_2 <- vector("numeric", len)
vector_phi_3 <- vector("numeric", len)
vector_phi_4 <- vector("numeric", len)

for(i in 1:len){
  vector_phi_0[i] <- phi_0(x[i]);
  vector_phi_1[i] <- phi_1(x[i]);
  vector_phi_2[i] <- phi_2(x[i]);
  vector_phi_3[i] <- phi_3(x[i]);
  vector_phi_4[i] <- phi_4(x[i]);
}

Phi <- matrix(c(vector_phi_0, vector_phi_1, vector_phi_2, vector_phi_3, vector_phi_4), nrow=30, ncol=5)
Phi_T <- t(Phi)
Phi_t <- solve((Phi_T%*%Phi))%*%Phi_T
w <- Phi_t%*%y

reg <- function(x){w[5]*x^4 + w[4]*x^3 + w[3]*x^2 + w[2]*x + w[1]}
lines(z, reg(z), col="red", type="l")
