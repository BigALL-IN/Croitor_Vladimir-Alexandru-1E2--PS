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
  while (cumulative_prob <= 1 - 0.000001) {
    cumulative_prob = cumulative_prob + dpois(k0, lambda)
    k0 = k0 + 1
  }
  return(k0)
}






#A2_a
calculate_means = function(file_name) {
  data = read.table(file_name, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  print(head(data))
  print(summary(data))
  data = na.omit(data)
  
  data$P = as.numeric(data$P)
  data$S = as.numeric(data$S)
  
  if (any(is.na(data$P)) | any(is.na(data$S))) {
    stop("Există valori non-numerice sau lipsă în datele tale!")
  }
  
  P_table = table(data$P)
  S_table = table(data$S)
  
  P_relative = as.vector(P_table) / length(data$P)
  S_relative = as.vector(S_table) / length(data$S)
  
  P_mean = mean(data$P)
  S_mean = mean(data$S)
  
  return(list(
    frecvente_P = P_table,
    frecvente_relative_P = P_relative,
    medie_P = P_mean,
    frecvente_S = S_table,
    frecvente_relative_S = S_relative,
    medie_S = S_mean
  ))
}



#A2_b
detect_and_remove_outliers = function(file_name, sample_name) {
  data = read.table(file_name, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  data[[sample_name]] = as.numeric(data[[sample_name]])
  data = na.omit(data)
  sample_data = data[[sample_name]]
  Q1 = quantile(sample_data, 0.25)
  Q3 = quantile(sample_data, 0.75)
  IQR_value = Q3 - Q1
  lower_bound = Q1 - 1.5 * IQR_value
  upper_bound = Q3 + 1.5 * IQR_value
  cleaned_sample = sample_data[sample_data >= lower_bound & sample_data <= upper_bound]
  
  hist(cleaned_sample, breaks = seq(1, 10, by = 1), right = TRUE, 
       main = paste("Distribuția frecvențelor pentru eșantionul curat", sample_name),
       xlab = "Intervale", ylab = "Frecvențe", col = "blue")
  
  return(cleaned_sample)
}




lambda = 3
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
means = calculate_means("note_PS.csv")
cat("Mean of P:", means$medie_P, "\n")
cat("Mean of S:", means$medie_S, "\n")

# A2_b
cleaned_sample_P = detect_and_remove_outliers("note_PS.csv", "P")
cleaned_sample_S = detect_and_remove_outliers("note_PS.csv", "S")