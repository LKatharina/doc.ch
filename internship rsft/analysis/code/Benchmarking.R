# ==============================================================================
# Benchmarking 
# ==============================================================================


# Option 1----------------------------------------------------------------------

start_time <- Sys.time()
Sys.sleep(3)
end_time <- Sys.time()

end_time - start_time



# Option 2----------------------------------------------------------------------

system.time({ Sys.sleep(3) })



# Option 3----------------------------------------------------------------------

library(rbenchmark)
library(dplyr)

somedata <- data.frame(name = c("wale", "shark", "lobster", "shrimp"),
                       size = c(10000, 2000, 50, 8))

benchmark("dplyr" = {
  somedata %>% filter(size < 100)
},
"baseR" = {
  somedata$size[somedata$size < 100]
},
replications = 1000,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))



# Option 4----------------------------------------------------------------------

library(microbenchmark)
library(dplyr)

somedata <- data.frame(name = c("wale", "shark", "lobster", "shrimp"),
                       size = c(10000, 2000, 50, 8))

check_for_seconds <- function(values) {
}

mbm <- microbenchmark("dplyr" = {
  somedata %>% filter(size < 100)
},
"baseR" = {
  somedata$size[somedata$size < 100]
},
times = 1000L)

library(ggplot2)
autoplot(mbm)


