require(mosaicData); set.seed(500); anpha <- 0.05
boxplot(wage ~ sex, data=CPS85)
sample1 <- subset(CPS85, sex=='M', select=c(wage))[[1]]
sample2 <- subset(CPS85, sex=='F', select=c(wage))[[1]]
n1 <- length(sample1); n2 <- length(sample2); n1; n2

x_1 <- mean(sample1); x_2 <- mean(sample2); x_1 - x_2

diffmean <- function() {
  index <- 1:(n1+n2) %in% sample(1:(n1+n2), n1)
  rand_sample1 <- c(sample1, sample2)[index]
  rand_sample2 <- c(sample1, sample2)[!index]
  return(mean(rand_sample1)-mean(rand_sample2))
}

rand_dist <- replicate(10000, diffmean()); hist(rand_dist)
pvalue <- sum(rand_dist > x_1 - x_2)/length(rand_dist); 
pvalue

pvalue < anpha; x_1 - x_2 > quantile(rand_dist, 1-anpha)

se <- sqrt(var(sample1)/ n1 + var(sample2)/n2); t <- (x_1 - x_2)/se
pvalue <- 1 - pt(t, df = min(n1, n2) - 1); pvalue

pvalue < anpha; t > qt(1 - anpha/2, df = min(n1, n2) - 1)