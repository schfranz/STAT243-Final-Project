#playing with log-concavity

#log-concavity determined by if F(alpha*x + (1-alpha)*y) >= F(x)^alpha * F(y)^(1-alpha) (wrong in Davis paper)
#example with dnorm, should return all TRUE
alpha <- 0.5
x <- seq(100)
y <- x
hm <- dnorm(alpha*x + (1 - alpha*y))
hm2 <- dnorm(x)^alpha * dnorm(y)^(1-alpha)
hm >= hm2

#demo for log-concavity with normal distribution
hm <- dnorm(seq(100),1)
plot(x, hm)
h <- log(hm)
plot(x, h)
plot(x[1:(length(x)-1)]+0.5, diff(h)/diff(x))


#functions that are non-log-concave according to Wikipedia:
	#Student's t
	#Cauchy
	#Pareto
	#log-normal
	#F-distribution
#some non-log-concave functions may have log-concave CDFs, so this is not a good test
#test with Student's t
(dt(alpha*x + (1 - alpha)*y,1)) >= (dt(x,1)^alpha * dt(y,1)^(1-alpha))


#demonstration
#according to paper, h(x) = ln(g(x)) and h'(x) = dh(x)/dx decreases monotonically with increasing x in D (domain)
hm <- dt(seq(100),1)
plot(x, hm)
h <- log(hm)
plot(x, h)
plot(x[1:(length(x)-1)]+0.5, diff(h)/diff(x))