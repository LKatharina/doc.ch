
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#         Internship: Decisions from experience - Bayesian models           ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

library(ggplot2)
library(stringr)
library(data.table)
library(cognitivemodels)

rm(list = ls(all = TRUE))
gc()

# Random sampling
samplesize <- 7
probabilities <- c(0.2, 0.8) # First number is prob for Heads

tosses <- sample(x = 0:1, 
                 size = samplesize, 
                 prob = probabilities, 
                 replace = TRUE)

 

# Simple likelihood model without prior believe

# Heads and tails count
heads <- sum(tosses == 0)
tails <- sum(tosses == 1)

# Number of tosses
total <- heads + tails

# Range of possible probabilities
rangeP <- seq(0, 
              1, 
              length.out = 100)

# Likelihood of data given 0
lik <- dbinom(x = heads, 
              prob = rangeP, 
              size = total)

# Plotting
plot(rangeP, lik,
     type = "l", 
     xlab = "Range", 
     ylab = "Density",
     main = "P(data|0)")



# Simple bayesian model with prior believe

# Head and tail count
heads <- sum(tosses == 0)
tails <- sum(tosses == 1)

# Mean and SD of prior probability distribution
priorM <- 0.5
priorSD <- 0.1

# Number of trials
total <- Heads + Tails
  
# Range of possible values
rangeP <- seq(0, 
              1, 
              length.out = 100)


# Likelihood of data given 0
lik <- dbinom(x = heads, 
              prob = rangeP, 
              size = total)

# Prior believe of probability distribution
prior <- dnorm(x = rangeP, 
               mean = priorM, 
               sd = priorSD)

# Unstandardized posterior probability distribution
unstdPost <- lik * prior
# Standardized posterior probability distribution
stdPost <- unstdPost / sum(unstdPost)

# Plotting
plot(rangeP, lik,
     type = "l", 
     xlab = "Range", 
     ylab = "Density",
     main = "P(data|0)")
lines(rangeP, 
      prior / (total * 2), 
      col = "red")
lines(rangeP, 
      lik * prior, 
      col = "green")
lines(rangeP, 
      stdPost, 
      col = "blue")
legend("topleft", 
       legend = c("Lik", "Prior", "Unstd Post", "Post"),
       text.col = 1:4, 
       bty = "n")


beliefes <- rbeta(n = 1000, shape1 = heads, shape2 = tails)

hist(beliefes, xlim = c(0:1))

ggplot2::ggplot(P, ggplot2::aes(x = r)) +
  ggplot2::geom_histogram() +
  ggplot2::facet_wrap(~trial)

D <- data.frame(s1 = 0, s2 = 3,
                r1 = 1, r2 = 2,
                ps1 = c(0.49, 0.51), ps2 = c(0.51, 0.49),
                pr1 = 0.5, pr2 = 0.5,
                s = 0,
                init = 0, 
                t = 1,
                goal = 9)

M <- cognitivemodels::hm1988(~ s1+ps1+s2+ps2 | r1+pr1+r2+pr2, 
                             trials = ".ALL", states = ~s, budget = ~goal, ntrials = 3, data = D)
predict(M, type = "value")

P <- data.frame(trial = 4-M$get_timehorizons(),
                 state = M$get_states(),
                 predict = predict(M),
                 predict(M, type = "value"),
                D[rep(1:2, each = 12),])



#---------------------------------#
#        Version 1.0           ----
#---------------------------------#

start_time <- Sys.time()

sampling <- function(dfe_n, subject_n = 5, s1, s2, r1, r2, ps1, pr1, beta_n = 100, prior_s1 = 1, prior_s2 = 1, prior_r1 = 1, prior_r2 = 1, seed = 123) {
  set.seed(seed)
  outcomes <- NULL
  for (i in 1:subject_n) {
    outcomes <- rbind(outcomes, data.table(id = i,
                                           out_s = rbinom(n = dfe_n, 
                                                          size = 1, 
                                                          prob = 1-ps1),
                                           out_r = rbinom(n = dfe_n, 
                                                          size = 1, 
                                                          prob = 1-pr1)))
  }
  beta_sampling <- data.table(ps1 = rbeta(n = beta_n, 
                                          shape1 = prior_s1+sum(outcomes$out_s == 0), 
                                          shape2 = prior_s2+sum(outcomes$out_s == 1)),
                              pr1 = rbeta(n = beta_n, 
                                          shape1 = prior_r1+sum(outcomes$out_r == 0), 
                                          shape2 = prior_r2+sum(outcomes$out_r == 1)))
  sampling <- data.table(id = rep(unique(outcomes$id), each = nrow(beta_sampling)/subject_n),
                         dfe_n = rep(dfe_n, times = beta_n),
                         s1 = rep(s1, times = beta_n), s2 = rep(s2, times = beta_n), 
                         r1 = rep(r1, times = beta_n), r2 = rep(r2, times = beta_n), 
                         beta_sampling, 
                         ps2 = 1-beta_sampling$ps1,
                         pr2 = 1-beta_sampling$pr1)
  sampling
}

vect_sampling <- Vectorize(sampling, SIMPLIFY = FALSE)
D <- vect_sampling(dfe_n = c(3, 10), 
                   subject_n = 5,
                   s1 = 0, 
                   s2 = 1, 
                   r1 = 0, 
                   r2 = 4, 
                   ps1 = 0.5, 
                   pr1 = 0.8,
                   beta_n = 100)
D_as_one <- do.call(rbind.data.frame, D)

modeling <- function(goal, ntrials, df) {
  M <- cognitivemodels::hm1988(~ s1+ps1+s2+ps2 | r1+pr1+r2+pr2, 
                               trials = ".ALL", states = ".ALL", budget = ~goal, ntrials = ~ntrials, data = df)
  P <- data.table(trial = ntrials+1-M$get_timehorizons(),
                  state = M$get_states(),
                  predict = predict(M),
                  predict(M, type = "value"))
  P <- cbind(P,
             df[rep(1:nrow(df), each = nrow(P)/nrow(df)),])
  P
}

myP <- modeling(goal = 5, ntrials = 3, df = D_as_one)

end_time <- Sys.time()

end_time - start_time
# Time difference of 24.23936 secs



#---------------------------------#
##        Improving V1.0       ----
#---------------------------------#

### Multiple subjects ----
dfe_n = c(3, 10)
subject_n = 5
s1 = 0 
s2 = 1
r1 = 0 
r2 = 4
ps1 = 0.5
pr1 = 0.2
beta_n = 100

outcomes <- data.table(dfe_n = c(rep(dfe_n[1], subject_n), 
                                 rep(dfe_n[2], subject_n)),
                       out_s1 = c(rbinom(n = subject_n, 
                                         size = dfe_n[1], 
                                         prob = 1-ps1),
                                  rbinom(n = subject_n, 
                                         size = dfe_n[2], 
                                         prob = 1-ps1)),
                       out_r1 = c(rbinom(n = subject_n, 
                                         size = dfe_n[1], 
                                         prob = 1-pr1),
                                  rbinom(n = subject_n, 
                                         size = dfe_n[2], 
                                         prob = 1-pr1)))

outcomes_r <- list(out_s1 = rbinom(n = subject_n, 
                                   size = dfe_n, 
                                   prob = 1-ps1))

outcomes_s <- data.table(out_r1 = rbinom(n = subject_n, 
                                         size = dfe_n, 
                                         prob = 1-pr1))
# YES but slow
outcomes_total <- NULL
for (i in 1:nrow(outcomes)) {
  outcomes_total <- rbind(outcomes_total, 
                          data.table(id = ifelse(outcomes$dfe_n[i] == outcomes$dfe_n[1],
                                                 yes = i,
                                                 no = i - subject_n),
                                     dfe_n = outcomes$dfe_n[i],
                                     out_s = c(rep(s1, outcomes$out_s1[i]),
                                               rep(s2, outcomes$dfe_n[i] - outcomes$out_s1[i])),
                                     out_r = c(rep(r1, outcomes$out_r1[i]),
                                               rep(r2, outcomes$dfe_n[i] - outcomes$out_r1[i]))),
                          fill = TRUE)
}

# NO
outcomes_total2 <- melt(outcomes, 
                        id.vars = c("id"), 
                        measure.vars = c("out_s1", "out_r1"), 
                        variable.name = "x", 
                        value.name = c("y", "z"))

# NO
library(tidyr)
outcomes_total3 <- pivot_longer(outcomes, cols = 2:3, names_to = "x", values_to = "y")

# NO
df.expanded <- outcomes[rep("0", outcomes$out_s1), -c(2:3)]

# NO
subjectify <- function(x, y) {
  D <- data.table(r = rep(c(s1,s2), times = c(x, dfe_n-x)),
                  s = rep(c(r1,r2), times = c(y, dfe_n-y)))
  D
  
  
}
x <- 4
subjectify(outcomes_r[[1]][1])
rep(s1:s2, times = c(3, 10-3))

sapply(outcomes, FUN = subjectify, simplify = "array")


# serious try
# looping WORKS!!!!!!!!
outcomes <- data.table(dfe_n = c(rep(dfe_n[1], subject_n), 
                                 rep(dfe_n[2], subject_n)),
                       out_s1 = c(rbinom(n = subject_n, 
                                         size = dfe_n[1], 
                                         prob = 1-ps1),
                                  rbinom(n = subject_n, 
                                         size = dfe_n[2], 
                                         prob = 1-ps1)),
                       out_r1 = c(rbinom(n = subject_n, 
                                         size = dfe_n[1], 
                                         prob = 1-pr1),
                                  rbinom(n = subject_n, 
                                         size = dfe_n[2], 
                                         prob = 1-pr1)))

# simple dataset with id and dfe_n
D <- data.table(id = c(rep(1:subject_n, each = dfe_n[1]),
                       rep(1:subject_n, each = dfe_n[2])),
                dfe_n = c(rep(c(dfe_n[1], dfe_n[2]), times = c(dfe_n[1]*subject_n, dfe_n[2]*subject_n))))

# with for loop
outcomes_total <- NULL
for (i in 1:nrow(outcomes)) {
  outcomes_total <- rbind(outcomes_total, 
                          data.table(out_s = c(rep(s1, outcomes$out_s1[i]),
                                               rep(s2, outcomes$dfe_n[i] - outcomes$out_s1[i])),
                                     out_r = c(rep(r1, outcomes$out_r1[i]),
                                               rep(r2, outcomes$dfe_n[i] - outcomes$out_r1[i]))),
                          fill = TRUE)
  if (i == nrow(outcomes)) {
    D <- cbind(D, outcomes_total)
    D
  }
}


# functioning DOESN'T WORK!!!!!!!!!!!
outcomes_dfe1 <- data.table(dfe_n = rep(dfe_n[1], subject_n),
                            out_s1 = rbinom(n = subject_n, 
                                            size = dfe_n[1], 
                                            prob = 1-ps1),
                            out_r1 = rbinom(n = subject_n, 
                                            size = dfe_n[1], 
                                            prob = 1-pr1))
outcomer2_dfe2 <- data.table(dfe_n = rep(dfe_n[2], subject_n),
                             out_s1 = rbinom(n = subject_n, 
                                             size = dfe_n[2], 
                                             prob = 1-ps1),
                             out_r1 = rbinom(n = subject_n, 
                                             size = dfe_n[2], 
                                             prob = 1-pr1))

# simple datasets with id and dfe_n
D <- data.table(id = c(rep(1:subject_n, each = dfe_n[1]),
                        rep(1:subject_n, each = dfe_n[2])),
                 dfe_n = c(rep(c(dfe_n[1], dfe_n[2]), times = c(dfe_n[1]*subject_n, dfe_n[2]*subject_n))))

# with function
outcomes_total <- NULL
subjectify <- function(x, dfe_n) {
  outcomes_total <- rbind(outcomes_total, 
                          data.table(out_s = c(rep(s1, x),
                                               rep(s2, dfe_n - x)),
                                     out_r = c(rep(r1, out_r1),
                                               rep(r2, dfe_n - x))),
                          fill = TRUE)
  outcomes_total
}
apply(outcomes_dfe1[,c(2,3)], 1, subjectify, dfe_n = dfe_n[1])
apply(outcomes_dfe2[,c(2,3)], 1, subjectify, dfe_n = dfe_n[2])
D <- cbind(D, outcomes_total)


# make it faster
s1 <- 0
s2 <- 1
r1 <- 0
r2 <- 4
ps1 <- 0.5
ps2 <- 1-ps1
pr1 <- 0.8
pr2 <- 1-pr1

dfe_n <- c(3, 10)
dfe_l <- length(dfe_n)
subject_n <- 5

trials <- 3
budget <- c(15, 17)
budget_l <- length(budget)

beta_n <- 100

count_s1 <- c(sapply(dfe_n, rbinom, n = subject_n, prob = ps1))
count_r1 <- c(sapply(dfe_n, rbinom, n = subject_n, prob = pr1))

freq_rown <- subject_n * dfe_l * budget_l

data_freq <- data.table(count_s1 = rep(count_s1, times = budget_l),
                        count_r1 = rep(count_r1, times = budget_l),
                        dfe = rep(dfe_n, each = subject_n, times = budget_l),
                        budget = rep(budget, each = subject_n * dfe_l),
                        subject_nr = rep(1:subject_n, times = dfe_l * budget_l),
                        s1 = rep(s1, times = freq_rown),
                        s2 = rep(s2, times = freq_rown),
                        r1 = rep(r1, times = freq_rown),
                        r2 = rep(r2, times = freq_rown),
                        ps1 = rep(ps1, times = freq_rown),
                        ps2 = rep(ps2, times = freq_rown),
                        pr1 = rep(pr1, times = freq_rown),
                        pr2 = rep(pr2, times = freq_rown))

data_freq[, `:=` (count_s2 = dfe - count_s1, 
                  count_r2 = dfe - count_r1,
                  nr = 1:freq_rown,
                  id = paste(budget, dfe, subject_nr, sep="_"))]

data_beta <- data_freq[rep(1:freq_rown, each = beta_n)]
data_beta[, `:=` (beta_s1 = rbeta(.N, count_s1 + prior[1], count_s2 + prior[2]),
                  beta_r1 = rbeta(.N, count_r1 + prior[1], count_r2 + prior[2])),
          by = .(dfe, subject_nr, budget)]

data_beta[, `:=` (beta_s2 = 1 - beta_s1,
                  beta_r2 = 1 - beta_r1,
                  start = 0,
                  beta_draw = 1:nrow(data_beta))]

rsft_model <- hm1988(~ s1 + beta_s1 + s2 + beta_s2 | r1 + beta_r1  + r2 + beta_r2,
  trials = ".ALL",
  data = data_beta,
  budget = ~budget,
  initstate = ~start,
  nt = trials,
  states = ".ALL",
  choicerule = choicerule,
  fix = list(tau = tau))

predict(rsft_model)



#---------------------------------#
## Benchmark Improvements V1.0 ----
#---------------------------------#

library(microbenchmark)

mbm <- microbenchmark("old" = {
  outcomes_total <- NULL
  for (i in 1:nrow(outcomes)) {
    outcomes_total <- rbind(outcomes_total, 
                            data.table(id = ifelse(outcomes$dfe_n[i] == outcomes$dfe_n[1],
                                                   yes = i,
                                                   no = i - subject_n),
                                       dfe_n = outcomes$dfe_n[i],
                                       out_s = c(rep(s1, outcomes$out_s1[i]),
                                                 rep(s2, outcomes$dfe_n[i] - outcomes$out_s1[i])),
                                       out_r = c(rep(r1, outcomes$out_r1[i]),
                                                 rep(r2, outcomes$dfe_n[i] - outcomes$out_r1[i]))),
                            fill = TRUE)
  }
},
"new" = {
  D <- data.table(id = c(rep(1:subject_n, each = dfe_n[1]),
                         rep(1:subject_n, each = dfe_n[2])),
                  dfe_n = c(rep(c(dfe_n[1], dfe_n[2]), times = c(dfe_n[1]*subject_n, dfe_n[2]*subject_n))))
  
  outcomes_total <- NULL
  for (i in 1:nrow(outcomes)) {
    outcomes_total <- rbind(outcomes_total, 
                            data.table(out_s = c(rep(s1, outcomes$out_s1[i]),
                                                 rep(s2, outcomes$dfe_n[i] - outcomes$out_s1[i])),
                                       out_r = c(rep(r1, outcomes$out_r1[i]),
                                                 rep(r2, outcomes$dfe_n[i] - outcomes$out_r1[i]))),
                            fill = TRUE)
    if (i == nrow(outcomes)) {
      D <- cbind(D, outcomes_total)
      D
    }
  }
},
times = 100L)

library(ggplot2)
autoplot(mbm)




mbm <- microbenchmark("old" = {
  outcomes <- NULL
  for (i in 1:subject_n) {
    outcomes <- rbind(outcomes, data.table(id = i,
                                           out_s = rbinom(n = dfe_n, 
                                                          size = 1, 
                                                          prob = 1-ps1),
                                           out_r = rbinom(n = dfe_n, 
                                                          size = 1, 
                                                          prob = 1-pr1)))
  }
},
"new" = {
  outcomes <- data.table(dfe_n = c(rep(dfe_n[1], subject_n), 
                                   rep(dfe_n[2], subject_n)),
                         out_s1 = c(rbinom(n = subject_n, 
                                           size = dfe_n[1], 
                                           prob = 1-ps1),
                                    rbinom(n = subject_n, 
                                           size = dfe_n[2], 
                                           prob = 1-ps1)),
                         out_r1 = c(rbinom(n = subject_n, 
                                           size = dfe_n[1], 
                                           prob = 1-pr1),
                                    rbinom(n = subject_n, 
                                           size = dfe_n[2], 
                                           prob = 1-pr1)))
  
  outcomes_total <- NULL
  for (i in 1:nrow(outcomes)) {
    outcomes_total <- rbind(outcomes_total, 
                            data.table(id = ifelse(outcomes$dfe_n[i] == outcomes$dfe_n[1],
                                                   yes = i,
                                                   no = i - subject_n),
                                       dfe_n = outcomes$dfe_n[i],
                                       out_s = c(rep(s1, outcomes$out_s1[i]),
                                                 rep(s2, outcomes$dfe_n[i] - outcomes$out_s1[i])),
                                       out_r = c(rep(r1, outcomes$out_r1[i]),
                                                 rep(r2, outcomes$dfe_n[i] - outcomes$out_r1[i]))),
                            fill = TRUE)
  }
},
times = 100L)

ps1 <- 0.5
pr1 <- 0.2
subject_n <- 5
dfe_n <- c(3, 10)

mbm <- microbenchmark("old" = {
  outcomes <- data.table(dfe_n = c(rep(dfe_n[1], subject_n), 
                                   rep(dfe_n[2], subject_n)),
                         out_s1 = c(rbinom(n = subject_n, 
                                           size = dfe_n[1], 
                                           prob = ps1),
                                    rbinom(n = subject_n, 
                                           size = dfe_n[2], 
                                           prob = ps1)),
                         out_r1 = c(rbinom(n = subject_n, 
                                           size = dfe_n[1], 
                                           prob = pr1),
                                    rbinom(n = subject_n, 
                                           size = dfe_n[2], 
                                           prob = pr1)))
  D <- data.table(id = c(rep(1:subject_n, each = dfe_n[1]),
                         rep(1:subject_n, each = dfe_n[2])),
                  dfe_n = c(rep(c(dfe_n[1], dfe_n[2]), times = c(dfe_n[1]*subject_n, dfe_n[2]*subject_n))))
  }, "new" = {
  count_xh <- c(sapply(dfe_n, rbinom, n = subject_n, prob = ps1))
  count_xl <- c(sapply(dfe_n, rbinom, n = subject_n, prob = pr1))
  },
times = 100L)


library(ggplot2)
autoplot(mbm)



#---------------------------------#
#        Version 2.0           ----
#---------------------------------#

sampling <- function(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n = 100, prior = c(1,1), seed = 42) {
  set.seed(seed)
  
  ps2 <- 1-ps1
  pr2 <- 1-pr1
  dfe_l <- length(dfe_n)
  budget_l <- length(budget)
  
  count_s1 <- c(sapply(dfe_n, rbinom, n = subject_n, prob = ps1))
  count_r1 <- c(sapply(dfe_n, rbinom, n = subject_n, prob = pr1))
  
  freq_rown <- subject_n * dfe_l * budget_l
  
  data_freq <- data.table(count_s1 = rep(count_s1, times = budget_l),
                          count_r1 = rep(count_r1, times = budget_l),
                          dfe_id = rep(dfe_n, each = subject_n, times = budget_l),
                          budget = rep(budget, each = subject_n * dfe_l),
                          # subject_nr = rep(1:subject_n, times = dfe_l * budget_l),
                          subject_id = 1:freq_rown,
                          s1 = rep(s1, times = freq_rown),
                          s2 = rep(s2, times = freq_rown),
                          r1 = rep(r1, times = freq_rown),
                          r2 = rep(r2, times = freq_rown),
                          ps1 = rep(ps1, times = freq_rown),
                          ps2 = rep(ps2, times = freq_rown),
                          pr1 = rep(pr1, times = freq_rown),
                          pr2 = rep(pr2, times = freq_rown))
  
  data_freq[, `:=` (count_s2 = dfe_id - count_s1, 
                    count_r2 = dfe_id - count_r1,
                    env_id = paste(ps1, s1, s2, pr1, r1, r2, budget, sep="_"))]
  
  data_beta <- data_freq[rep(1:freq_rown, each = beta_n)]
  
  data_beta[, `:=` (b_ps1 = rbeta(.N, count_s1 + prior[1], count_s2 + prior[2]),
                    b_pr1 = rbeta(.N, count_r1 + prior[1], count_r2 + prior[2])),
            by = .(dfe_id, subject_id, budget)]
  
  data_beta[, `:=` (b_ps2 = 1 - b_ps1,
                    b_pr2 = 1 - b_pr1,
                    start = 0,
                    beta_id = 1:nrow(data_beta))]
  
  return(data_beta)
}


modeling <- function(df, ntrials, choicerule = "softmax", tau = 0.2) {
  rsft_model <- hm1988(~ s1 + b_ps1 + s2 + b_ps2 | r1 + b_pr1  + r2 + b_pr2,
                       trials = ".ALL",
                       data = df,
                       budget = ~budget,
                       initstate = ~start,
                       nt = ntrials,
                       states = ".ALL",
                       choicerule = choicerule,
                       fix = list(tau = tau))
  
  predict(rsft_model)
  rsft_model$envid
  rsft_sim <- data.table(trial = (ntrials + 1) - rsft_model$get_timehorizons(),
                         state = rsft_model$get_states(),
                         prhv_rsft = rsft_model$predict("response"),
                         prstate = rsft_model$predict("pstate"),
                         rvalue = predict(rsft_model, type="values")[,1],
                         svalue = predict(rsft_model, type="values")[,2])
  
  rsft_sim[, beta_id := cumsum(trial == 1)]
  db <- merge(rsft_sim, df)
  
  setcolorder(db, neworder = c("env_id", "dfe_id", "subject_id", "beta_id"))
  
  return(db)
}


data <- sampling(s1 = 0, s2 = 1,
                 r1 = 0, r2 = 4,
                 ps1 = 0.5, pr1 = 0.8,
                 dfe_n = c(3, 10),
                 subject_n = 5,
                 budget = c(10, 12),
                 beta_n = 100)

model <- modeling(df = data, ntrials = 3) 


sum()


