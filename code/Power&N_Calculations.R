setwd("C:/Users/user/OneDrive/바탕 화면/leaRning")

#Four inputs for the sample size calculation
#1. Significance level alpha
#2. Power, beta
#3. The mean effect size we wish to detect, delta
#4. The standard deviation of output(come from a pilot study or previous research)

power.t.test(delta = 2, sd = 3.5, power = 0.8, sig.level = 0.01)

powers <- seq(0.5, 0.95, 0.01)
sample_size <- numeric(length(powers))
for (i in 1:length(powers)){
  sample_size[i] <- power.t.test(delta = 2, sd = 3.5, power = powers[i],
                                 sig.level = 0.01)$n
}
plot(powers, sample_size, type = "l", 
     xlab = "power: beta", ylab = "sample size: N", 
     col = "red")
text(x = 0.6, y = 100, adj = c(0, 0),
     labels = substitute(paste(delta, "= 2")))
text(x = 0.6, y = 95, adj = c(0, 0),
     labels = substitute(paste(sigma ,"= 3.5")))
text(x = 0.6, y = 90, adj = c(0, 0),
     labels = substitute(paste(alpha, "= 0.01")))
