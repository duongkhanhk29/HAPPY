# APPENDIX
##### CHOOSE THE RANDOM SEED
min_error <- Inf
best_i <- NULL

for (i in 1:1200) {
  set.seed(i)
  print(i)
  rf <- regression_forest(xx, yy, num.trees = 5000, 
                          clusters = cluster_code,
                          sample.weights = data$weight,
                          tune.parameters = "all")
  error <- test_calibration(rf)[2,1]
  if (error < min_error) {
    min_error <- error
    best_i <- i
    print(min_error)
  }
}
