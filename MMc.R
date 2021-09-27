#função para gerar uma trajetória do processo M/M/c
genMMc <- function(mu, lambda, c, n, y0){
  mux <- mui(mu, c, 1) #mu_1 = mu
  rate <- lambda + mux
  S <- rep(0, n) #tempo de permanencia
  yn <- rep(0, n) #cadeia imersa
  yn[1] <- y0 #estado inicial
  S[1] <- rexp(1, rate)
  
  for(i in 2:n){
    muxl <- mui(mu, c, i) #mu_i = imu i<c e cmu i >=c
    rate <- lambda + muxl
    S[i] <- rexp(1, rate)
    prob <- sample(c(-1,1), size = 1, prob = c(lambda/rate, muxl/rate)) #prob de transição a um passo da cadeia imersa
    yn[i] <- yn[i-1] + prob
  }
  
  #cumsum(Sn)  = tempos dos saltos Jn e yn = traj CM imersa e CTMC entao yn = X(Jn) 
  return(cbind(yn, cumsum(S))) 
}

#encontrando mu_i se i < c imu else cmu
mui <- function(mu, c, state){
  if(state < c) return(state*mu)
  else return(c*mu)
}

lambda <- 30 #taxa de nascimento M/M/c
mu <- 1 # mu necessário para calcular taxa de morte M/M/c
n <- 30 #tamanho da trajetória
y0 <- 50 #estado inicial da cadeia
c <- c(2, 4, 10, 20)  #número de servidores

par(mfrow=c(2,2))
for ( i in 1:length(c)) {
  traj <- genMMc(mu, lambda, c[i], n, y0)
  sfun <- stepfun(traj[-n,2], traj[,1], right = TRUE)
  plot(sfun,
       main = paste('Trajetória M/M/c', 'mu =', mu, 'lambda =', lambda, 'c =', c[i]), 
       xlab ='t', ylab = 'estado', do.points = TRUE, verticals = FALSE,
       xlim = c(0, traj[n,2]))
  abline(v = 0, ncol = "red", lty = 2) #t = 0
  abline(h = y0, col = "blue", lty = 2) #estado inicial 
}

