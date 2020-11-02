standardize = function(X) {
  ones = rep(1, nrow(X))
  Xoff = X - ones %*% t(colMeans(X))
  scale = ones %*% t(colSums(Xoff*Xoff))
  Xstd = Xoff / scale
  return (Xstd)
}

ridge_data = function(n, p, sd){
  X = matrix(rnorm(n*p), nrow=n, ncol = p)
  btrue = rnorm(p)
  sd = 10
  X = standardize(X)
  y = drop(X %*% btrue + sd*rnorm(n))
  y = y - mean(y)
  y = y / sd(y)
  return (list(X = X, y = y))
}

ridge_map = function(y, X, lmbda) {
  XtX = crossprod(X)
  Xty = drop(crossprod(X,y))
  yty = drop(crossprod(y))
  p = ncol(X)
  V = XtX + diag(lmbda, p)
  Vinv = chol2inv(chol(V))
  bhat = drop(Vinv %*% Xty)
  return (list(coef = bhat, ypred = drop(X %*% bhat)))
}

ridge_glmnet = function(y, X, glm_lmbda) {
  fit = glmnet(X, y, alpha = 0, lambda = glm_lmbda, intercept = FALSE, standardize = FALSE)
  b = coef(fit, s = glm_lmbda)  # extract coefficients at a single value of lambda
  ypred = c(predict(fit, newx = X, s = c(glm_lmbda)))  # make predictions
  return (list(b = b, ypred = ypred))
}

ridge_glmnet_cv = function(y, X) {
  lambdas = 10^seq(3, -2, by = -.1)
  cv_fit = cv.glmnet(X, y, alpha = 0, lambda = lambdas)
  opt_lambda = cv_fit$lambda.min
  fit = cv_fit$glmnet.fit
  b = coef(fit, s = opt_lambda)
  ypred = c(predict(fit, newx = X, s = c(opt_lambda)))
  return (list(fit=fit, ypred = ypred, opt_lambda=opt_lambda))
}

rsquare = function(ytrue, ypred) {
  sse = sum((ypred - ytrue)^2)
  sst = sum((ytrue - mean(ytrue))^2)
  rsq = 1 - sse / sst
  return (rsq)
}

n = 200
p = 4
sd = 2
data = ridge_data(n, p, sd)
lmbda = sd * sd
glm_lmbda = lmbda / n
r_map = ridge_map(data$y, data$X, lmbda)
r_glmnet = ridge_glmnet(data$y, data$X, glm_lmbda)
r_glmnet_cv = ridge_glmnet_cv(data$y, data$X)

rsquare(data$y, r_map$ypred)
rsquare(data$y, r_glmnet$ypred)
rsquare(data$y, r_glmnet_cv$ypred)

r_map$coef
r_glmnet$b