set.seed(600); anpha <- 0.05
n <- 10; sample1 <- c(rep(1, 8), rep(0, 2))
theta_hat <- sum(sample1); theta_hat

boot_dist <- replicate(10000, sum(sample(sample1, n, replace=TRUE)))
hist(boot_dist, breaks = 40); sd(boot_dist)

(confint <- quantile(boot_dist, c(anpha/2, 1-anpha/2), names=FALSE))

sample0 <- c(rep(1, 5), rep(0, 5)); (theta_0 <- sum(sample0))

rand_dist <- replicate(10000, sum(sample(sample0, n, replace = TRUE)))
hist(rand_dist, breaks = 40); sd(rand_dist)

(pvalue <- sum(abs(rand_dist-theta_0) >= abs(theta_hat - theta_0))/length(rand_dist))

pvalue < anpha; !(confint[1] <= theta_0 && theta_0 <= confint[2])