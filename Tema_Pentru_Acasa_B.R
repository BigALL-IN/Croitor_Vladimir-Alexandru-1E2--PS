B1 = function(sample_size, R, r) {
  count = 0
  for (i in 1:sample_size){
    x = runif(1, min = -13, max = 13)
    y = runif(1, min = -13, max = 13)
    z = runif(1, min = -3, max = 3)
    
    if ((z^2+(sqrt(x^2+y^2)-R)^2)<r^2){
      count = count + 1
    }
  }
  
  bounding_volume = (2 * (R + r))^2 * (2 * r)
  estimation = bounding_volume * count / sample_size  
  volume = 2*pi^2*R*r^2
  er_abs = abs(estimation - volume)
  er_rel = er_abs / abs(volume)
  
  cat("esantion de dimensiune ", sample_size, ":\n")
  cat("estimare: ", estimation, "\n")
  cat("eroarea relativa: ", er_rel, "\n")
}

B1(20000,10,3)
B1(50000,10,3)
B1(100000,10,3)



#B2
B2=function(N){
  c=0
  for(i in 1:N){
    x=runif(1,0,2)
    y=runif(1,0,2.4 )
    if((2*x>=y)&(y<=6-3*x)){
      c=c+1
    }
  }
  return(4.8*c/N)
}
B2(20000)


#B3
B3_a=function(N,a,b){
  sum=0
  for(i in 1:N){
    x=runif(1,a,b)
    sum=sum+((2*x-1)/(x^2-x-6))
  }
  return((b-a)*sum/N)
}
estimare = B3_a(10000,-1,1)
valoare_exacta = log(3)-log(2)
eroare = abs(estimare-valoare_exacta)
cat("Estimare B3_a: ", estimare, "\n")
cat("Eroare absolută: ", eroare, "\n")


B3_b = function(N, a, b) {
  total = 0
  for (i in 1:N) {
    x = runif(1, a, b) 
    if (3*x - 3 > 0){
    total = total + ((x + 4) / sqrt(3*x - 3))
    }
  }
  estimate = total / N
  return(estimate)
}
estimare = B3_b(20000, 3, 11)
valoare_exacta = 61.2
eroare = abs(estimare - valoare_exacta)

cat("Estimare B3_b: ", estimare, "\n")
cat("Eroare B3_b: ", eroare, "\n")


B3_c=function(N, a, b){
  sum=0
  for(i in 1:N){
    x=rgamma(1, 1)
    sum=sum+x*exp(-x^2)
  }
  return(sum/N)
}
cat("estimare B3/c: ", B3_c(20000,0))
cat("eroare absoluta: ", abs(B3_c(10000,0)-1/2))


#B4_a
n = 1000 
p = 0.25   
q = 0.01   


B4_a = function(num_utilizatori_initial, num_utilizatori_doriti, n, p, q) {
  ani = 0
  num_utilizatori_actual = num_utilizatori_initial
  while (num_utilizatori_actual < num_utilizatori_doriti) {
    ani = ani + 1
    noi_utilizatori = rbinom(1, n, p)
    utilizatori_retragere = rbinom(1, num_utilizatori_actual, q)
    num_utilizatori_actual = num_utilizatori_actual + noi_utilizatori - utilizatori_retragere
  }
  return(ani)
}

rezultat = B4_a(10000, 15000, n, p, q)
cat("Numărul mediu de ani: ", rezultat, "\n")


B4_b = function(n,p,q)
{
  utilizatori_actuali = 10000
  luni = 40*12 + 10
  
  for(i in 1:luni)
  {
 
    noi_utilizatori = rbinom(1, floor(n / 12), p)
    utilizatori_pierduti = rbinom(1, utilizatori_actuali, q / 12)
    utilizatori_actuali = utilizatori_actuali + noi_utilizatori - utilizatori_pierduti
    
    if (utilizatori_actuali >= 15000) {
      return(1)
    }
  }
  return(0)
}

mean(replicate(1000, B4_b(1000,0.25,0.01)))


#B4_c
num_utilizatori_initial = 10000
num_utilizatori_doriti = 15000
n = 1000
p = 0.25
q = 0.01
eroare = 0.01
confidenta = 0.99
B4_c = function(num_utilizatori_initial, num_utilizatori_doriti, n, p, q, eroare, confidenta) {
  z = qnorm((1 + confidenta) / 2)
  
  num_simulari = 0
  prob_actuala = 0
  var_estimata = 1
  
  while (TRUE) {
    num_simulari = num_simulari + 1000
    prob_actuala = B4_b(num_simulari, num_utilizatori_initial, num_utilizatori_doriti, n, p, q)
    var_estimata = prob_actuala * (1 - prob_actuala) / num_simulari
    
    eroare_actuala = z * sqrt(var_estimata)
    
    if (eroare_actuala <= eroare) {
      break
    }
  }
  
  return(list(probabilitate = prob_actuala, num_simulari = num_simulari))
}

rezultat = B4_c(num_utilizatori_initial, num_utilizatori_doriti, n, p, q, eroare, confidenta)

cat("Numărul de simulări necesare pentru o eroare de maxim ±0.01 cu o probabilitate de 0.99:", rezultat$num_simulari, "\n")
cat("Probabilitatea estimată cu eroare maximă de ±0.01:", rezultat$probabilitate, "\n")
