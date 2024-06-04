#C1_a
gen_permutari = function(n) {
  U = runif(n)  
  permutare = order(U)
  return(permutare)
}


sir_biti = function(n, k) {
  siruri = matrix(rbinom(n * k, 1, 0.5), nrow = n, ncol = k)
}


n = 10
permutare = gen_permutari(n)
cat("Permutari aleatoare:", permutare, "\n")



#C1_b
C1_b = function(Wi, Wj) {
  lenWi = length(Wi)
  lenWj = length(Wj)
  Lij = min(lenWi, lenWj)
  
  for (l in 1:Lij) {
    if (Wi[l] < Wj[l]) {
      return(TRUE)
    } else if (Wi[l] > Wj[l]) {
      return(FALSE)
    }
  }
  
  if (lenWi < lenWj) {
    while (lenWi < lenWj) {
      Wi = c(Wi, rbinom(1, 1, 0.5))  
      lenWi = length(Wi)
      
      if (Wi[lenWi] < Wj[lenWi]) return(TRUE)
      if (Wi[lenWi] > Wj[lenWi]) return(FALSE)
    }
  } else if (lenWi > lenWj) {
    while (lenWj < lenWi) {
      Wj = c(Wj, rbinom(1, 1, 0.5))  
      lenWj = length(Wj)
      
      if (Wi[lenWj] < Wj[lenWj]) return(TRUE)
      if (Wi[lenWj] > Wj[lenWj]) return(FALSE)
    }
  }
  
  while (TRUE) {
    Wi = c(Wi, rbinom(1, 1, 0.5))
    Wj = c(Wj, rbinom(1, 1, 0.5))
    
    if (Wi[length(Wi)] < Wj[length(Wj)]) return(TRUE)
    if (Wi[length(Wi)] > Wj[length(Wj)]) return(FALSE)
  }
}


#C1_c

pivot = function(siruri, low, high) {
  pivot_index = sample(low:high, 1)
  pivot_row = siruri[pivot_index, ]
  
  siruri[pivot_index, ] = siruri[high, ]
  siruri[high, ] = pivot_row
  
  pivot_index = high
  pivot_row = siruri[pivot_index, ]
  
  i = low - 1
  for (j in low:(high - 1)) {
    if (C1_b(siruri[j, ], pivot_row)) {
      i = i + 1
      temp = siruri[i, ]
      siruri[i, ] = siruri[j, ]
      siruri[j, ] = temp
    }
  }
  
  siruri[high, ] = siruri[i + 1, ]
  siruri[i + 1, ] = pivot_row
  
  return(list(siruri = siruri, pivot_index = i + 1))
}

random_quicksort = function(siruri, low, high) {
  if (low < high) {
    result = pivot(siruri, low, high)
    pivot_index = result$pivot_index
    siruri = result$siruri  
    
    siruri = random_quicksort(siruri, low, pivot_index - 1)
    siruri = random_quicksort(siruri, pivot_index + 1, high)
  }
  
  return(siruri)  
}


siruri = sir_biti(n = 10, k = 5)  # 
print("Lista initiala:")
print(siruri)

random_quicksort(siruri, 1, nrow(siruri))


#C1_d
generate_permutation = function(n, k) {
  siruri = matrix(rbinom(n * k, 1, 0.5), nrow = n, ncol = k)
  
  sorted_siruri = random_quicksort(siruri, 1, n)
  
  indices = apply(sorted_siruri, 1, function(row) paste(row, collapse = ""))
  original_indices = apply(siruri, 1, function(row) paste(row, collapse = ""))
  permutation_indices = order(match(original_indices, indices))
  
  return(permutation_indices)
}

n = 10
k = 5
permutation = generate_permutation(n, k)
cat("Permutarea indicilor ordonați:", permutation, "\n")
#C2_a

max_cut = function(V, E, n, iterations) {
  max_cut_size = 0
  
  for (i in 1:iterations) {
    
    A = sample(V, n)
    B = setdiff(V, A)
    
    cut_size = 0
    for (e in E) {
      u = e[1]
      v = e[2]
      if ((u %in% A && v %in% B) || (u %in% B && v %in% A)) {
        cut_size = cut_size + 1
      }
    }
    
  
    if (cut_size > max_cut_size) {
      max_cut_size = cut_size
      best_cut = list(A = A, B = B, cut_size = cut_size)
    }
  }
  
  return(best_cut)
}


V = c(1, 2, 3, 4, 5, 6)
E = list(c(1, 2), c(1, 3), c(2, 4), c(3, 5), c(4, 6), c(5, 6))
n = 3  
iterations = 1000
result = max_cut(V, E, n, iterations)

cat("Subsetul A:", result$A, "\n")
cat("Subsetul B:", result$B, "\n")
cat("Cardinalul taieturii E(A, B):", result$cut_size, "\n")

#C2_b
#Sansele de a gasi o taietura cat mai mare cresc daca crestem numarul de iteratii.
  
  
  