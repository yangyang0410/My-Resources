library(tidyverse)
library(quadprog)

df <- read_csv("portfolio.csv") %>% 
  mutate(across(everything(), ~ as.numeric(gsub("%", "", .)) / 100))

# Calculate mean and covariance of monthly returns
mean_returns <- colMeans(df, na.rm = TRUE)
cov_matrix <- cov(df, use = "complete.obs")

# Number of assets
n <- ncol(df)

# Constraints for quadprog
Amat <- cbind(1, diag(n), t(rep(1, n)))
bvec <- c(1, rep(0, n))
dvec <- rep(0, n)

# Quadratic programming solution
sol <- solve.QP(Dmat = cov_matrix, dvec = rep(0, n),
                Amat = t(Amat), bvec = bvec, meq = meq)
