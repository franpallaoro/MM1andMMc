#função para gerar uma trajetória do processo M/M/1
genMM1 <- function(mu, lambda, n, y0){
  rate <- lambda + mu
  S <- rep(0, n) #tempo de permanencia
  yn <- rep(0, n) #cadeia imersa
  yn[1] <- y0 #estado inicial
  S[1] <- rexp(1, rate)
  
  for(i in 2:n){
    S[i] <- rexp(1, rate)
    prob <- sample(c(-1,1), size = 1, prob = c(lambda/rate, mu/rate)) #prob de transição a um passo da cadeia imersa
    yn[i] <- yn[i-1] + prob
  }
  
  #cumsum(Sn)  = tempos dos saltos Jn e yn = traj CM imersa e CTMC entao yn = X(Jn) 
  return(cbind(yn, cumsum(S))) 
}

lambda <- c(3, 6, 10, 12) #taxa de nascimento M/M/1
mu <- c(1, 5, 2, 10) #taxa de morte M/M/1
n <- 50 #tamanho da trajetória
y0 <- 50 #estado inicial da cadeia

par(mfrow=c(2,2))
for ( i in 1:length(mu)) {
  traj <- genMM1(mu[i], lambda[i], n, y0)
  sfun <- stepfun(traj[-n,2], traj[,1], right = TRUE)
  plot(sfun,
       main = paste('Trajetória M/M/1', 'mu =', mu[i], 'lambda =', lambda[i]), 
       xlab ='t', ylab = 'estado', do.points = TRUE, verticals = FALSE,
       xlim = c(0, traj[n,2]))
  abline(v = 0, ncol = "red", lty = 2) #t = 0
  abline(h = y0, col = "blue", lty = 2) #estado inicial 
}


