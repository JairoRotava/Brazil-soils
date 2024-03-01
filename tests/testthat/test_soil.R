library(testthat)

# TODO: mehh...deve ter um jeito melhor de fazer isso...
lib <- modules::use("../../R/soil.R")

test_that("Soil hydraulic equations", {
  # Argumentos de teste
  hydgrp <- "c"
  z1 <- 300
  oc1 <- 3.8
  clay1 <- 23
  silt1 <- 33
  sand1 <- 44
  rock1 <- 0
  
  # Resultados esperados equacoes
  theta_1500 <- 0.161407744
  theta_33 <- 0.298649936
  theta_s33 <- 0.185754347
  theta_s <- 0.484724283
  k_s <- 16.30645214
  rho_n <- 1.365480649
  rho_b <- 1.365480649
  r_v <- 0
  paw <- 0.137242192
  
  # Resultados esperados para SWAT SWAT
  sol_bd <- rho_b
  sol_awc <- paw
  sol_k <- k_s
  sol_cbn <- oc1
  clay <- clay1
  silt <- silt1
  sand <- sand1
  rock <- rock1
  sol_alb <- 0.0439
  usle_k <- 0.1281
  sol_ec <- 1.0
  
  # sand %w
  s <- sand1/100
  # clay %w
  c <- clay1/100
  # organic matter %w
  om <- oc1
  # Gravel - cascalho. Fracao por volume (g cm-3) e fracao por peso (g g-1)
  r = rock1
  







  expect_equal(lib$theta_1500(s,c,om), theta_1500)
  expect_equal(lib$theta_33(s,c,om), theta_33)
  expect_equal(lib$theta_s33(s,c,om), theta_s33)
  expect_equal(lib$theta_s(theta_33, theta_s33, s), theta_s)
  expect_equal(lib$k_s(theta_1500, theta_33, theta_s ), k_s)
  expect_equal(lib$rho_n(theta_s), rho_n)
  # TODO: testar isso direito. Na planilha tem valor zero para rock,
  # por isso não é possivel saber se esta funcionando.
  # Também precisa checar se a formula esta correta. No artigo
  # esta diferente da planilha.
  expect_equal(lib$r_v(r/2.65,r/100), r_v)
  expect_equal(lib$rho_b(rho_n, r_v), rho_b)
  expect_equal(lib$paw(theta_1500, theta_33, r_v), paw)
  expect_equal(lib$usle_k(sol_cbn, clay, silt, sand), usle_k, tolerance=1e-4)
  expect_equal(lib$sol_alb(sol_cbn), sol_alb, tolerance=1e-4)
  
  # TODO: Checar se dataframe esta gerando resultados esperados
  testthat::testthat_print(lib$convert_layer_to_swat(oc1, clay1, silt1, sand1, rock1))


})
