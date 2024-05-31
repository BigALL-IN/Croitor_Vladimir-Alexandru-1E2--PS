#A1_a
poisson_prob = function(k, m, lambda) {
  probabilities = dpois(k:m, lambda)
  return(probabilities)
}

geometric_prob = function(k, m, p) {
  probabilities = dgeom(k:m, p)
  return(probabilities)
}

binomial_prob = function(k, m, n, p) {
  probabilities = dbinom(k:m, n, p)
  return(probabilities)
}

#A1_b
plot_probabilities = function(probabilities, distribution) {
  plot(probabilities, type = "h", main = paste("Probability Mass Function -", distribution),
       xlab = "Value", ylab = "Probability")
}

#A1_c
find_k0 = function(lambda) {
  k0 = 0
  cumulative_prob = 0
  while (cumulative_prob <= 1 - 1e-6) {
    cumulative_prob = cumulative_prob + dpois(k0, lambda)
    k0 = k0 + 1
  }
  return(k0)
}






#A2_a
calculate_frequencies_and_means = function(file_name) {
  data = read.table(file_name, header = TRUE)
  P = data$P
  S = data$S
  P_table = table(P)
  S_table = table(S)
  P_relative = as.vector(P_table) / length(P)
  S_relative = as.vector(S_table) / length(S)
  P_mean = mean(P)
  S_mean = mean(S)
  return(list(P_table = P_table, S_table = S_table,
              P_relative = P_relative, S_relative = S_relative,
              P_mean = P_mean, S_mean = S_mean))
}


#A2_b
remove_outliers_and_plot = function(file_name, sample_name) {
  data = read.table(file_name, header = TRUE)
    sample = data[[sample_name]]
    remove_rule = function(sample) {
    mean_sample = mean(sample)
    sd_sample = sd(sample)
    lower_limit = mean_sample - 2 * sd_sample
    upper_limit = mean_sample + 2 * sd_sample
    cleaned_sample = sample[sample >= lower_limit & sample <= upper_limit]
    return(cleaned_sample)
  }
  cleaned_sample = remove_rule(sample)
  intervals = cut(cleaned_sample, breaks = seq(1, 10, by = 1))
  freq_table = table(intervals)
  barplot(freq_table, main = paste("Histogram of", sample_name, "without outliers"),
          xlab = "Value", ylab = "Frequency", col = "skyblue", border = "black")
  return(cleaned_sample)
}


plot_histogram = function(sample, sample_name) {
  hist(sample, breaks = seq(1, 10, by = 1), main = paste("Histogram of", sample_name),
       xlab = "Value", ylab = "Frequency", col = "skyblue", border = "black")
}


lambda = 2
k = 0
m = 5
p = 0.3
n = 10

# A1_a
poisson_probabilities = poisson_prob(k, m, lambda)
geometric_probabilities = geometric_prob(k, m, p)
binomial_probabilities = binomial_prob(k, m, n, p)

# A1_b
plot_probabilities(poisson_probabilities, "Poisson(lambda)")
plot_probabilities(geometric_probabilities, "Geometric(p)")
plot_probabilities(binomial_probabilities, "Binomial(n, p)")

# A1(c)
k0 = find_k0(lambda)
print(paste("The smallest value of k0 for which P(Y <= k0) > 1 - 10^(-6) is:", k0))

# A2_a
means = calculate_means("note PS.txt")
print(paste("Mean of P:", means$P_mean))
print(paste("Mean of S:", means$S_mean))

# A2_b
cleaned_P = remove_outliers("note PS.txt", "P")
cleaned_S = remove_outliers("note PS.txt", "S")

plot_histogram(cleaned_P, "P")
plot_histogram(cleaned_S, "S")