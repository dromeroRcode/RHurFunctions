kdci2<-function (x = NULL, y = NULL, alpha = 0.05, type = "t", bootstrap = F, 
          B = 1000) 
{
  Q <- function(i, j) {
    Q.ij <- 0
    ij <- (j[2] - i[2]) * (j[1] - i[1])
    if (ij > 0) 
      Q.ij <- 1
    if (ij < 0) 
      Q.ij <- -1
    Q.ij
  }
  C.i <- function(x, y, i) {
    C.i <- 0
    for (k in 1:length(x)) if (k != i) 
      C.i <- C.i + Q(c(x[i], y[i]), c(x[k], y[k]))
    C.i
  }
  if (!bootstrap) {
    c.i <- numeric(0)
    n <- length(x)
    for (i in 1:n) c.i <- c(c.i, C.i(x, y, i))
    options(warn = -1)
    tau.hat <- cor.test(x, y, method = "k")$estimate
    options(warn = 0)
    sigma.hat.2 <- 2 * (n - 2) * var(c.i)/n/(n - 1)
    sigma.hat.2 <- sigma.hat.2 + 1 - (tau.hat)^2
    sigma.hat.2 <- sigma.hat.2 * 2/n/(n - 1)
    if (type == "t") 
      z <- qnorm(alpha/2, lower.tail = F)
    if (type != "t") 
      z <- qnorm(alpha, lower.tail = F)
    tau.L <- tau.hat - z * sqrt(sigma.hat.2)
    tau.U <- tau.hat + z * sqrt(sigma.hat.2)
    if (type == "l") 
      tau.U <- 1
    if (type == "u") 
      tau.L <- -1
  }
  if (bootstrap) {
    tau <- numeric(0)
    for (b in 1:B) {
      b.sample <- sample(1:length(x), length(x), replace = T)
      options(warn = -1)
      tau.sample <- cor.test(x[b.sample], y[b.sample], 
                             method = "k")
      options(warn = 0)
      tau.sample <- tau.sample$estimate
      tau <- c(tau, tau.sample)
    }
    tau.hat <- sort(tau)
    hist(tau.hat)
    if (type == "t") 
      k <- floor((B + 1) * alpha/2)
    if (type != "t") 
      k <- floor((B + 1) * alpha)
    tau.L <- tau.hat[k]
    tau.U <- tau.hat[(B + 1 - k)]
    if (type == "l") 
      tau.U <- 1
    if (type == "u") 
      tau.L <- -1
  }
  tau.L <- round(tau.L, 3)
  tau.U <- round(tau.U, 3)
  results<-c(tau.L,tau.U)
  return(results)
}
