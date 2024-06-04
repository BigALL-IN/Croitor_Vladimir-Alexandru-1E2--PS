#D1
data = read.table('probabilitati.csv', skip=1, header=FALSE)
probs = data$V1
sample_mean = mean(probs)
n = length(probs)

sigma = sqrt(92.16)


alfa = 0.05
critical_z = qnorm(1 - alfa / 2, 0, 1)
a = sample_mean - critical_z * sigma / sqrt(n)
b = sample_mean + critical_z * sigma / sqrt(n)
interval_95 = c(a, b)


alfa2 = 0.01
critical_z = qnorm(1 - alfa2 / 2, 0, 1)
a = sample_mean - critical_z * sigma / sqrt(n)
b = sample_mean + critical_z * sigma / sqrt(n)
interval_99 = c(a, b)

interval_95
interval_99

#D2

data = read.table('statistica.csv', skip=1, header=FALSE)
stat = data$V1  
sample_mean = mean(stat)
std_dev = sd(stat)
n_stat = length(stat)

alfa = 0.05
critical_t_95 = qt(1 - alfa / 2, df=n_stat - 1)
error_95 = critical_t_95 * std_dev / sqrt(n_stat)
ci_95_stat = c(sample_mean - error_95, sample_mean + error_95)

alfa2 = 0.01
critical_t_99 = qt(1 - alfa2 / 2, df=n_stat - 1)
error_99 = critical_t_99 * std_dev / sqrt(n_stat)
ci_99_stat = c(sample_mean - error_99, sample_mean + error_99)


ci_95_stat
ci_99_stat

#D3
n = 100   
X = 14     
p0 = 0.15
phat = X / n

z = (phat - p0) / sqrt(p0 * (1 - p0) / n)

z_critical_95 = qnorm(0.975)  
z_critical_99 = qnorm(0.995)  


reject_95 = abs(z) > z_critical_95
reject_99 = abs(z) > z_critical_99

if (!reject_99) {
  cat("La un nivel de semnificație de 1%, putem respinge ipoteza nulă.\n")
} else {
  cat("La un nivel de semnificație de 1%, nu putem respinge ipoteza nulă.\n")
}

if (!reject_95) {
  cat("La un nivel de semnificație de 5%, putem respinge ipoteza nulă.\n")
} else {
  cat("La un nivel de semnificație de 5%, nu putem respinge ipoteza nulă.\n")
}