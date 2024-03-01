soil <- modules::use("R/soil.R")

# sand %w
s <- 44/100
# clay %w
c <- 23/100
# organic matter %w
om <- 3.8
soil$theta_1500(s, c, om)
soil$theta_33(s, c, om)

