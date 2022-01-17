library(cognitivemodels)
library(cognitiveutils)
library(ggplot2)

# Load data ----------------------------------------------------------------


# Variables ----------------------------------------------------------------
# outcomes and probabilities
ps1 = 0.5
pr1 = 0.8
s1 = 0
s2 = 1
r1 = 0
r2 = 4

#RSFT task
nt = 3 #trials
b = c(10,12) # budget

# Sampling
small = 3 # samplesize: large vs. small
large = 10 # samplesize: large vs. small
subject_n = 5 # number of different samples (binominal distribution) / number of participants per sample size

# Beta distribution
beta_n = 100 # draws from beta distribution


# Function for simulation of learning and choices
createData <- function(ps1,pr1,s1,s2,r2,b,small,large, subject_n, beta_n, seed = 42, choicerule = "softmax", tau = 0.2, prior = c(1,1)){
  set.seed(seed)
  # count_s1 = NULL
  # count_r1 = NULL
  dfe_n = c(small,large)
  
  # Sampling from binominal distribution
  # for (player in dfe_n) {
  #   for(sample in 1:subject_n){
  #     outh = rbinom(n=player, size=1, prob=ps1)
  #     outl = rbinom(n=player, size=1, prob=pr1)
  #     count_s1 = c(count_s1,sum(outh))
  #     count_r1 = c(count_r1,sum(outl))
  #   }
  # }
  
  # -------------------------------------------------------------------
  # ALTERNATIVE
  # --> Please double check that it does what it should
  # n: Number of random draws = number of persons = length of output vector
  # size: Number of trials = numnber of samples by person = max count
  count_s1 <- c(sapply(dfe_n, rbinom, n = subject_n, prob = ps1))
  count_r1 <- c(sapply(dfe_n, rbinom, n = subject_n, prob = pr1))
  
  nb <- length(b)
  nrowsdata <- subject_n * length(dfe_n) * nb
  data = data.table(
    numberOfdraws = rep(dfe_n, each = subject_n, times = nb),
    subject_n = rep(1:subject_n, times = length(dfe_n) * nb),
    count_s1 = rep(count_s1, nb),
    count_r1 = rep(count_r1, nb),
    b = rep(b, each = subject_n * length(dfe_n)),
    s1 = rep(s1, nrowsdata),
    s2 = rep(s2, nrowsdata),
    r1 = rep(r1, nrowsdata),
    r2 = rep(r2, nrowsdata),
    ps1 = rep(ps1, nrowsdata),
    ps2 = rep((1-ps1), nrowsdata),
    pr1 = rep(pr1, nrowsdata),
    pr2 = rep((1-pr1), nrowsdata)
  )
  
  # x = 1
  # d <- data
  # while(x < length(b)){
  #   data = rbind(data,d)
  #   x = x + 1
  # }
  
  # vb = rep(b,each = subject_n * length(dfe_n))
  # data[,b := vb]
  data[,count_s2 := numberOfdraws - count_s1]
  data[,count_r2 := numberOfdraws - count_r1]
  
  # M = bayes_beta_c( ~ count_s1 + count_r1,
  #                    data = data,
  #                    format = "count",
  #                    fix = list(delta = 1,
  #                               priorpar = c(1,1),
  #                               sigma = 0.01))
  # data[, pmax := predict(M)] 
  data[, nr := 1:nrow(data)]
  data[, id := paste(b,numberOfdraws,subject_n, sep="_")]
  
  # Sampling from beta distribution
  # i <- 1
  # data_bayes <- lapply(1:nrow(data), function(i) {
  #   j = 1
  #   bs1 <- rbeta(n = beta_n,data$count_s1[i], data$count_s2[i])
  #   br1 <- rbeta(n = beta_n,data$count_r1[i], data$count_r2[i])
  #   d <- data[j,]
  #   while(j < beta_n){
  #      d <- rbind(d,data[i,])
  #      j = j + 1
  #   }
  #   dat <- cbind(d,bs1,br1)
  #   return(dat)
  # }
  # )
  # make a longer data frame (expand data)
  data_bayes <- data[rep(1:nrowsdata, each = beta_n)]
  data_bayes[, c("bs1", "br1") := .(
    rbeta(.N, count_s1 + prior[1], count_s2 + prior[2]),
    rbeta(.N, count_r1 + prior[1], count_r2 + prior[2])
  ),
  by = .(numberOfdraws, subject_n, b)
  ]
  

  # Make the list into one data table (data frame)
  # data_bayes <- rbindlist(data_bayes)
  data_bayes[, bs2 := 1 - bs1]
  data_bayes[, br2 := 1 - br1]
  data_bayes[, start := 0]
  data_bayes[, betadraw := 1:nrow(data_bayes)]
  
  
  #  RSFT model
  debug_data <<- data_bayes
  rsft_model <- hm1988(
    ~ s1 + bs1 + s2 + bs2 | r1 + br1  + r2 + br2,  # our formula (as before)
    trials = ".ALL",        # NEW: ".ALL" will predict for *all possible* trials
    data = data_bayes,         # our data (as before)
    budget = ~b,      # name of our budget column in our data
    initstate = ~start,    # name of our starting-state column in our data
    nt = nt,            # we always 5 trials therefore I hard-code this
    states = ".ALL",        # NEW: ".ALL" will predict for *all possible* states
    choicerule = choicerule,
    fix = list(tau = tau))
  
  predict(rsft_model)
  
  
  rsft_sim <- data.table(
    trial = (nt + 1) - rsft_model$get_timehorizons(), # get trials that are *remaining*
    state =   rsft_model$get_states(),         # get possible states
    prhv_rsft =    rsft_model$predict("response"),  # get pr(hv) prediction
    prstate = rsft_model$predict("pstate"), # get pr(state)
    rvalue = predict(rsft_model, type="values")[,1],
    svalue = predict(rsft_model, type="values")[,2]
  )
  
  
  # Stimuli nr
  rsft_sim[, betadraw := cumsum(trial == 1)]
  db <- merge(rsft_sim, data_bayes)
  
  return(db)
}

db <- createData(ps1 = ps1, pr1 = pr1, s1 = s1, s2 = s2, r2 = r2, b = b, small = small, large = large
                 ,subject_n = subject_n, beta_n = beta_n)



library(tidybayes) # for fancy density plots, google it, it is great
# define one plotting function
make_plot <- function(i) {
  the_name <- "Sample Size"
  the_labels <- c("Large (10 per option)", "Small (3 per option)")
  the_colors <- c("red", "grey25")
  ggplot(
    data = db[id %in% i],
    mapping = aes(x = trial, y = bs1, color = numberOfdraws, fill = numberOfdraws)) +
    stat_halfeye( # uses median and QI = quantile interval (also known as the percentile interval or equi-tailed interval)
      .width = c(.66, 0.95), #use .66, .95 to show 66 and 96% HDI
      slab_alpha = 0.15,
      position = position_dodge(width = .09),
      aes(shape = numberOfdraws), point_fill = "white") +
    theme_classic() +
    scale_fill_manual(
      values=the_colors,
      name = the_name, labels = the_labels) +
    scale_colour_manual(
      values=the_colors,
      name = the_name, labels = the_labels) +
    scale_shape_manual(
      values = c(16, 21),
      name = the_name, labels = the_labels) +
    facet_wrap(~trial+state, labeller = label_both, scale = "free_x") +
    ylab("Predicted Proportion of Risky Choices") +
    labs(title = "Environment",
         subtitle = paste("Reach", db[id %in% i]$b[1], "in 3 trials")) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

 
# plot and combine
p1 <- make_plot(c("10_3_1", "10_3_2", "10_3_3", "10_10_1", "10_10_2","10_10_3"))
p2 <- make_plot(c("12_3_1", "12_3_2", "12_3_3", "12_10_1", "12_10_2","12_10_3"))

p1 + p2 +
  plot_layout(guides = "collect", widths = c(.4,.05,.4)) +
  plot_annotation(caption = "Note: Points = Median, thick line = 66% quantile intervall, thin line = 95% quantile intervall")

ggsave("../figures/temp_dfe_easy_hard_n50.png",width = 14, height = 4)

