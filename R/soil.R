
# TODO: verificar unidades e deixar hoomogenio. Parece que as funcoes 
# mais abaixo utiliam outro tipo de unidade. Checar pq om não precisa
# dividir por 100

#-----------------------------------------------
## Soil hydraulic propoerties Saxton e Tawls 2006
export("theta_1500")
theta_1500 <- function(s, c, om) {
  x <- (-0.024*s + 0.487*c + 0.006*om
         + 0.005*s*om - 0.013*c*om
         + 0.068*s*c + 0.031)
  return(x + (0.14*x - 0.02))
}

export("theta_33")
theta_33 <- function(s, c, om) {
  x <- (-0.251*s + 0.195*c + 0.011*om
        + 0.006*s*om - 0.027*c*om
        + 0.452*s*c + 0.299)
  return(x + (1.283*x*x - 0.374*x - 0.015))
}

export("theta_s33")
theta_s33 <- function(s, c, om) {
  x <- (0.278*s + 0.034*c + 0.022*om
        - 0.018*s*om - 0.027*c*om
        -0.584*s*c + 0.078)
  return(x + (0.636*x - 0.107))
}

export("theta_s")
theta_s <- function(theta_33, theta_s33, s) {
  return(theta_33 + theta_s33 - 0.097*s + 0.043)
}

export("b")
b <- function(theta_1500, theta_33) {
  return( (log(1500) - log(33)) / (log(theta_33) - log(theta_1500)) )
}

export("lambda")
lambda <- function(theta_1500, theta_33) {
  return(1/b(theta_1500, theta_33))
}

export("k_s")
k_s <- function(theta_1500, theta_33, theta_s) {
  return(1930*(theta_s - theta_33)^(3-lambda(theta_1500, theta_33)))
}

export("rho_n")
rho_n <- function(theta_s) {
  return((1 - theta_s)*2.65)
}

# TODO: Checar se isso esta correto
export("alpha")
alpha <- function(rho_n) {
  return( rho_n / 2.65)
}

# TODO: Checar se isso esta correto
export("r_w")
r_w <- function(rho_n) {
  return( rho_n / 100)
}

# TODO: r_v não esta batendo com planilha. Planilha esta estranho
# parace ter trocado um sinal de / por *
export("r_v")
r_v <- function(alpha, r_w) {
  return( alpha*r_w / (1 - (r_w*(1 - alpha))) )
}


export("rho_b")
rho_b <- function(rho_n, r_v) {
  return(rho_n*(1 - r_v) + r_v*2.65)
}

# TODO: copiei da planilha. Não tem isso no artigo. Verificar
export("paw")
paw <- function(theta_1500, theta_33, r_v) {
  return( (theta_33 - theta_1500) * (1 - r_v))
}

#-----------------------------------------------
## Soil Albedo: Equation proposed by Baumer (1990) as used in WEPP

export("sol_alb")
sol_alb <- function(sol_cbn) {
  return( 0.6 / exp(0.4* sol_cbn * 1.72))
}

#-----------------------------------------------
## USLE_K Equation proposed by Williams (1995) as cited in SWAT theoritical documentation

export("usle_k")
usle_k <- function(sol_cbn, clay, silt, sand) {
  return( ( 0.2 + 0.3*exp(-0.256*sand*(1-silt/100)))*
            ((silt/(clay+silt))^0.3)*
            (1-(0.25*sol_cbn/(sol_cbn+exp(3.72-2.95*sol_cbn))))*
            (1-(0.7*(1-sand/100))/((1-sand/100)+exp(-5.51+22.9*(1-sand/100)))) )
}

#------------------------------------------------
## Converte camada para parametros do SWAT
export("convert_layer_to_swat")
convert_layer_to_swat <- function(om, clay, silt, sand, rock) {
  theta_1500 <- theta_1500(sand/100,clay/100,om)
  theta_33 <- theta_33(sand/100,clay/100,om)
  theta_s33 <- theta_s33(sand/100,clay/100,om)
  theta_s <- theta_s(theta_33, theta_s33, sand/100)
  k_s <- k_s(theta_1500, theta_33, theta_s)
  rho_n <- rho_n(theta_s)
  r_v <- r_v(rock/2.65,rock/100)
  rho_b <- rho_b(rho_n, r_v)
  paw <- paw(theta_1500, theta_33, r_v)
  usle_k <- usle_k(om, clay, silt, sand)
  sol_alb <- sol_alb(om)
  
  swat = data.frame(SOL_BD = rho_b)
  swat$SOL_AWC <- paw
  swat$SOL_K <- k_s
  swat$SOL_CBN <- om
  swat$CLAY <- clay
  swat$SILT <- silt
  swat$SAND <- sand
  swat$ROCK <- rock
  swat$SOL_ALB <- sol_alb
  swat$USLE_K <- usle_k
  swat$SOL_EC <- 1.0
  
  return(swat)
  
}



