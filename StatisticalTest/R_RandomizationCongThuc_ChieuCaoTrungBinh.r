set.seed(400);
anpha <- 0.05; 
n <- 100;
sample1 <- rnorm(n, mean = 1.6, sd = 0.08)
x_bar <- mean(sample1); x_bar; x0 <- 1.62; d_mean <- x_bar - x0

rand_dist <- replicate(10000, mean(sample(sample1 - d_mean, n, replace = TRUE)))
pvalue <- sum(abs(rand_dist - x0) > abs(x_bar - x0)) / length(rand_dist);
pvalue

pvalue < anpha; p_hat < quantile(rand_dist, anpha/2) || p_hat > quantile(rand_dist, 1-anpha/2)
se <- sd(sample1)/sqrt(n);
t <- d_mean/se;
pvalue <- 2*(1-pt(abs(t), df = n-1));
pvalue

pvalue < anpha; t < qt(anpha/2, df=n-1) || t > qt(1-anpha/2, df=n-1)