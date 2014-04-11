# HW 3 - 2014
setwd("~/Google Drive/Documents/programming/repos/statistical-theory/hw3")

library(xtable)

dat_file <- "data9.txt"
dat_str <- paste(readLines(dat_file), collapse="")
dat <- eval(parse(text=dat_str))

mean_dat <- mean(dat)
var_dat <- mean((dat-mean_dat)^2)

theta <- c(mean_dat, var_dat)

get_log_lik <- function(theta, dat=dat)
{
    alpha <- theta[1]^2 / theta[2]
    beta <- theta[1] / theta[2]
    
    log_pr <- dgamma(x=dat, shape=alpha, rate=beta, log=TRUE)
    log_Y <- log(dat)
    log_EY <- log(theta[1])
    
    log_lik_each <- log_pr + log_Y - log_EY
    log_lik <- sum(log_lik_each)
    return(log_lik)
}

latex_matrix <- function(mat, format="%f")
{
    str <- "\\begin{bmatrix}\n"
    for (i in 1:nrow(mat))
    {
        line_str <- paste0(sprintf(format,mat[i,]), collapse=" & ")
        str <- paste0(str, line_str)
        if (i != nrow(mat))
        {
            str <- paste0(str, " \\\\")
        }
        str <- paste0(str, "\n")
    }
    str <- paste0(str, "\\end{bmatrix}")
    return(str)
}

optim_out <- suppressWarnings(optim(par=theta, 
                                    fn=get_log_lik, 
                                    dat=dat, 
                                    control=list(fnscale=-1),
                                    hessian=TRUE))

hess <- optim_out$hessian
vec <- eigen(optim_out$hessian)$vectors
val <- eigen(optim_out$hessian)$values

dat2 <- c(3, 4, 4, 3, 6, 0, 2, 4, 4, 3, 3, 5, 2 ,2, 4, 3, 5, 4, 1, 2, 3, 3, 6, 0, 5, 6, 5, 2, 4, 4, 0, 4, 4, 1, 2, 8, 4, 7, 3, 5, 3, 3, 2, 4, 5, 7, 3, 2, 4, 5)
est <- mean(dat2 == 0)
est

mle_df <- data.frame(
theta1 = c(
80.36865,
109.7853,
90.99945,
81.44357,
101.9241,
92.92737,
96.05893,
70.32257,
85.84821,
73.89659
),
theta2 = c(
10675.78343,
13356.19,
12486.82,
12821.99356,
13084.32,
12286.38729,
12657.37306,
12421.78847,
11932.33,
11354.32
))