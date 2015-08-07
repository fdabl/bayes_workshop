# example I will use; written for clarity, not efficiency ;)
dat <- c(0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1)

binom.boot <- function(dat, samples = 10000) {
    n <- length(dat) # is fixed!
    result <- numeric(samples)

    for (i in 1:samples) {
        draw <- sample(dat, n, replace = TRUE)
        result[i] <- sum(draw)
    }

    result
}

nbinom.boot <- function(dat, samples = 10000) {
    k <- sum(dat) # is fixed!
    result <- numeric(samples)

    for (i in 1:samples) {
        ss <- sample(c(0, 1), 1, prob = c(0.5, 0.5))

        while (sum(ss) != k) {
            ss <- c(ss, sample(c(0, 1), 1, prob = c(0.5, 0.5)))
        }

        result[i] <- length(ss)
    }
    result
}


b1 <- binom.boot(dat)
b2 <- nbinom.boot(dat)
par(mfrow = c(1,2))
hist(b1, main = "binomial sampling", xlab = "k", freq = FALSE, ylab = "p(z | theta, N)")
hist(b2, main = "negative binomial sampling", xlab = "N", freq = FALSE, ylab = "p(N | theta, z)")

# testing
n <- length(b1)
compute.p <- function(boot) {
    b <- sort(boot)
    n <- length(boot)

    crit.low <- b[0.025 * n]
    crit.high <- b[0.975 * n]

    # two tailed
    p <- sum(boot >= crit.high) / n + sum(boot <= crit.low) / n
    p
}

binom.p <- compute.p(b1)
nbinom.p <- compute.p(b2)

# why do the p-values not differ? where is the error?

# without bootstrap
binom.p2 <- pbinom(8, 26, 0.5) * 2
nbinom.p2 <- pnbinom(8, 26, 0.5) * 2

all.ps <- rbind(c(binom.p, binom.p2), c(nbinom.p, nbinom.p2))
colnames(all.ps) <- c('bootstrap', 'not bootstrap')
rownames(all.ps) <- c('binomial', 'neg binomial')


## miscellaneous
x1 <- rnorm(30, mean = 55, sd = 5)
x2 <- rnorm(30, mean = 50, sd = 5)

bootstrap <- function(diff, samples = 100) {
    sapply(1:samples, function(i) 
          (mean(sample(diff, length(diff), replace = TRUE))))
}

#boot <- bootstrap(x1 - x2)
#term <- qt(0.975, 29) * sd(boot)
#cis <- c(mean(boot) - term, mean(boot) + term)
