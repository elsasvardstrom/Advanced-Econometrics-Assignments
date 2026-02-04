# Data Definition: # These are fixed observed values.
x_T <- c(4.06, 3.63, 0.41, 2.45, 0.22, 3.39, 0.27, 0.69, 0.32,
         0.09, 4.00, 3.63, 2.65, 3.06, 3.77, 2.29, 2.31, 1.51)

y_T <- c(168.44, 178.66, 58.81, 189.29, 14.79, 178.70, 103.69, 103.30, 57.30,
         15.10, 172.28, 172.30, 194.32, 193.20, 167.50, 191.50, 173.98, 174.60)
# Length of x and y = 18:
length(x_T)
length(y_T)

# Log-Transform the dependent variable:
lny <- log(y_T)

# Build the Design Matrix Manually:
X <- cbind(1, x_T, log(x_T))

# Manual OLS Estimation:
beta_hat <- solve(t(X)%*% X) %*% (t(X)%*% lny)

# Orthogonality check:
e_hat <- lny - X %*% beta_hat
t(X) %*% e_hat

# Parameter Extraction:
b0 <- beta_hat[1]
b1 <- beta_hat[2]
b2 <- beta_hat[3]

# Fitted values:
y_hat_log <- X %*% beta_hat
y_hat <- exp(y_hat_log)

# Plotting:
plot(x_T, y_T, pch = 19)
ord <- order(x_T)
lines(x_T[ord], y_T[ord], lwd = 2)

# Analytical Maximization:
x_star <- -b2/b1
y_hat_star <- exp(b0) * exp(b1*x_star) * x_star^b2