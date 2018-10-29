len = 15;
order = 5;
x <- vector("numeric", len);
y <- vector("numeric", len);

fun <- function(x){ x^3 - 9*x + 15};

for(i in 1:len){
  ranNr <- runif(1, -2.0, 2.0);
  x[i] <- ranNr;
  y[i] <- fun(ranNr) + rnorm(1)/10;
}

plot(x,y)

basisFunction <- function(x, i){x^i}

linReg <- function(len, order, x, y, lambda){
  Phi <- matrix(0, len, order);
  
  for(i in 1:len){
    for(j in 0:(order-1)){
      Phi[i,j+1] <- basisFunction(x[i], j);
    }
  }
  
  Phi_T <- t(Phi);
  Phi_reg <- Phi_T%*%Phi + lambda*diag(order);
  Phi_t <- solve(Phi_reg)%*%Phi_T;
  w <- Phi_t%*%y;
  
  return(w);
}

reg <- function(x, w){
  y = 0;
  for(i in 0:(order-1)){
    y = y + w[i+1]*x^i;
  }
  return(y)
}

errorFunction <- function(x, w, lambda, len){
  result = 0;
  for(n in 1:len){
    result = result + (reg(x[n], w) - y[n])^2 #+ lambda/2 * (t(w)%*%w);
  }
  result = result + lambda/2 * (t(w)%*%w);
  return(abs(0.5*result));
}

lambdaAxis <- vector("numeric", 30);
errorTrainingAxis <- vector("numeric", 30);
errorTestAxis <- vector("numeric", 30);
index = 1;
maxY = 0;
minY = 100;

for(l in -31:-1){
  lambda = exp(l);
  #lambda = l;
  print(lambda)
  errorTraining = 0;
  errorTest = 0;
  
  for(i in 1:(len)){
    k = 1;
    trainingValues = vector("numeric", len-1);
    trainingTargetValues = vector("numeric", len-1);
    testValues = vector("numeric", 1);
    
    for(j in 1:(len-1)){
      if(j == ((i-1)*1)+1){
        testValues[1] = x[k];
        k = k+1;
      }
      trainingValues[j] = x[k];
      trainingTargetValues[j] = y[k];
      k = k+1;
    }
    if(testValues[1] == 0){
      testValues[1] = x[len];
    }
    
    vectorW = linReg(len-1, order, trainingValues, trainingTargetValues, lambda);

    z = seq(-5,50,0.1);
    lines(z, reg(z, vectorW) ,col="green", type="l");
    
    errorTrainingTmp = errorFunction(trainingValues, vectorW, lambda, len-1);
    errorTraining = errorTraining + sqrt(2*errorTrainingTmp/(len-1));

    errorTestTmp = errorFunction(testValues, vectorW, lambda, 1);

    errorTest = errorTest + sqrt(2*errorTestTmp);
  }
  
  errorTraining = errorTraining/len;
  errorTest = errorTest/len;
  
  lambdaAxis[index] = lambda;
  errorTrainingAxis[index] = errorTraining;
  errorTestAxis[index] = errorTest;
  
  index = index+1;
  
  if(maxY < errorTraining)
    maxY = errorTraining;
  if(maxY < errorTest)
    maxY = errorTest;
  
  if(minY > errorTraining)
    minY = errorTraining;
  if(minY > errorTest)
    minY = errorTest;
}

plot(lambdaAxis, errorTrainingAxis ,col="red", type="l", ylim=c(minY,maxY));
lines(lambdaAxis, errorTestAxis ,col="green", type="l");
