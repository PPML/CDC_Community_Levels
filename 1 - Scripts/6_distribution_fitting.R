# fit gamma function to RIDOH quantiles
fit_gamma = function(par = c(shape = 1, rate = 1), delta = 3){
  
  # quantiles
  quants = c(6, 11, 19, 27, 32, 46)
  # fudge factor for repeat tests

  if(par[1]>0 & par[2]>0){
    q25 = qgamma(.25, shape = par[1], rate = par[2]) - (quants[1] + delta)
    q50 = qgamma(.5, shape = par[1], rate = par[2]) - (quants[2] + delta)
    q75 = qgamma(.75, shape = par[1], rate = par[2]) - (quants[3] + delta)
    q90 = qgamma(.90, shape = par[1], rate = par[2]) - (quants[4] + delta)
    q95 = qgamma(.95, shape = par[1], rate = par[2]) - (quants[4] + delta)
    q99 = qgamma(.99, shape = par[1], rate = par[2]) - (quants[4] + delta)
    
    return(q25^2 + q50^2 + q75^2 + q90^2 + q95^2 + q99^2)
  }
  else return(Inf)
  
}

# fits with different lags
fit1 = optim(par = c(5, 10), fn = fit_gamma)$par
fit2 = optim(par = c(1, 1), fn = fit_gamma, delta = 0)$par

# pull values for different lags
days = seq(0:50)
weights1 = sapply(days, function(a){
  pgamma(a+1, shape = fit1[1], rate = fit1[2])-
    pgamma(a, shape = fit1[1], rate = fit1[2])})
weights2 = sapply(days, function(a){
  pgamma(a+1, shape = fit2[1], rate = fit2[2])-
    pgamma(a, shape = fit2[1], rate = fit2[2])})


# exploratory plots
qgamma(.5, shape = fit1[1], rate = fit1[2])
qgamma(.75, shape = fit1[1], rate = fit1[2])
qgamma(.9, shape = fit1[1], rate = fit1[2])
qgamma(.95, shape = fit1[1], rate = fit1[2])
qgamma(.99, shape = fit1[1], rate = fit1[2])

x = seq(1:50)
y = dgamma(x, shape = fit1[1], rate = fit1[2])
df = data.frame(x,y)
ggplot(df, aes(x = x, y = y)) + geom_line()
