len = 30;
x <- vector("numeric", len);
y <- vector("numeric", len);

fun = function(x){2.5*x^4 - 2*x^3 + x^2 - 2*x};

for(i in 1:len){
  ran = dnorm(i);
  ranNr = sample(0:10, 1)/10;
  x[i] = ranNr;
  y[i] = fun(ranNr) + rnorm(1)/20;
}

plot(x, y)
x <- seq(0,1,0.01)
lines(x, fun(x), col="green", type="l")

