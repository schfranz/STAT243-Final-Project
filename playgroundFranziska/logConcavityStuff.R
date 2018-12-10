#playing with log-concavity

#log-concavity determined by if F(alpha*x + (1-alpha*y)) >= F(x)^alpha * F(y)^(1-alpha) (Davis paper)
#example with dnorm, should return all TRUE
alpha <- 0.5
x <- seq(100)
y <- x
hm <- dnorm(alpha*x + (1 - alpha*y))
hm2 <- dnorm(x)^alpha * dnorm(y)^(1-alpha)
hm >= hm2