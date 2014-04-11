### Simulate from Weibull

rwei <- function(n, c, d)
{
    u <- runif(n)
    (-log(u)/d)^(1/c)
}

score_c <- function(x, c, d)
{
    1/c + log(x) - d*log(x)*x^c
}

score_d <- function(x, c, d)
{
    1/d - x^c
}

info_cd <- function(n, c, d)
{
    dat <- rwei(n, c, d)
    s_c <- score_c(dat, c, d)
    s_d <- score_d(dat, c, d)
    all_s <- cbind(s_c, s_d)
    all_info <- apply(all_s, 1, tcrossprod)
    mean_info <- apply(all_info, 1, mean)
    matrix(mean_info, nrow=2)
}

info_c <- function(n, c, d)
{
    dat <- rwei(n, c, d)
    s_c <- score_c(dat, c, d)
    all_info <- s_c*s_c
    mean(all_info)
}

n <- 100000
c <- 3
d <- 0.002

crb_inv1 <- info_c(n, c, d)
crb1 <- crb_inv1^(-1)

crb_inv2 <- info_cd(n, c, d)
crb2 <- solve(crb_inv2)[1,1]

dtau_dtheta <- function(c, d)
{
    dtheta1 <- -log(log(2)/d)*(log(2)/d)^(1/c)/c^2
    dtheta2 <- -1/c*log(2)^(1/c)*(1/d)^(1/c+1)
    c(dtheta1, dtheta2)
}

dtdt <- dtau_dtheta(3, 0.002)

crb3 <- drop(dtdt %*% solve(crb_inv2) %*% dtdt)
sqrt(crb3)/0.1
