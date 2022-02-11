# ==============================================================================
# Benchmarking: 4 options
# ==============================================================================


# Option 1----------------------------------------------------------------------

start_time <- Sys.time()
Sys.sleep(3)
end_time <- Sys.time()

end_time - start_time



# Option 2----------------------------------------------------------------------

system.time({ Sys.sleep(3) })



# Option 3----------------------------------------------------------------------

somedata <- data.frame(name = c("wale", "shark", "lobster", "shrimp"),
                       size = c(10000, 2000, 50, 8))

rbenchmark::benchmark("dplyr" = {
  somedata %>% dplyr::filter(size < 100)
},
"baseR" = {
  somedata$size[somedata$size < 100]
},
replications = 1000,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))



# Option 4----------------------------------------------------------------------

somedata <- data.frame(name = c("wale", "shark", "lobster", "shrimp"),
                       size = c(10000, 2000, 50, 8))

mbm <- microbenchmark::microbenchmark("dplyr" = {
  somedata %>% dplyr::filter(size < 100)
},
"baseR" = {
  somedata$size[somedata$size < 100]
},
times = 1000L)

ggplot2::autoplot(mbm)


