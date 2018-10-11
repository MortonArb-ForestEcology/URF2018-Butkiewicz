# Need to calculate the hite threshold that corresponds to a DBH of 20 cm

# Allometric function from inside of ED
dbh2h <- function(dbh, hite.ref=1.3, b1ht, b2ht){
  b1ht*(1-exp(dbh*b2ht))+hite.ref
}

# Default params for PFT 10 (mid-successional hardwood) 
b1ht = 25.18 # DBH-height allometry intercept
b2ht = -0.04964 # DBH-height allometry slope
dbh2h(dbh=20, b1ht=b1ht, b2ht=b2ht)

dbh.range <- 5:50
hite.range <- dbh2h(dbh=dbh.range, b1ht=b1ht, b2ht=b2ht)

plot(hite.range ~ dbh.range, type="l", col="green3")
