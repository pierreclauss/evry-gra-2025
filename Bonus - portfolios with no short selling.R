# Empirical constrained GMV with no short selling
# min(-d^T b + 1/2 b^T D b) with the constraints A^T b >= b_0
dvec <-  numeric(n)
# Constraints of sum of the weights equal to 1 and positive weights
Amat <-  cbind(e, diag(1, n, n))
bvec <-  cbind(1, t(numeric(n)))
omega_constraint <-
  solve.QP(Sigma, dvec, Amat, bvec, meq = 1)$solution
sigmag_constraint <-
  sqrt(t(omega_constraint) %*% Sigma %*% omega_constraint)
barplot(as.numeric(omega_constraint), col = 'black')
(sigmag_constraint)
(sd(as.matrix(table_returns_backtest) %*% omega_constraint) * sqrt(perio))