
library(ggplot2)
library(data.table)
library(tidybayes)
library(patchwork)
library(psych)
library(future)

rm(list = ls())
gc()

source("get_sample.R")

subjects <- 5
betas <- c(seq(1, 9, 1), seq(10, 100, 10), seq(250, 1000, 250), 2000, 3000)
ntrials <- 3
seeds <- seq(1, 20, 1)
budget <- 18
dfes <- c(3, 10)
prior <- c(1, 1)
ps1 <- 0.5
s1 <- 6
s2 <- 8
pr1 <- 0.25
r1 <- 1
r2 <- 9

paras <- data.table::as.data.table(expand.grid(beta_n = betas, budget = budget, subject_n = subjects, dfe_n = dfes, seed = seeds))
paras[, `:=` (ps1 = ps1, s1 = s1, s2 = s2, pr1 = pr1, r1 = r1, r2 = r2, para_id = .I)]

rown <- nrow(paras)
get_betas <- function(data) {
  l1 <- c(1:(rown/4))
  l2 <- c((rown/4+1):(rown/2))
  l3 <- c((rown/2+1):(rown*.75))
  l4 <- c((rown*.75+1):rown)
  
  future::plan(multisession)
  
  b1 %<-% {
    data[l1, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
  }
  b2 %<-% {
    data[l2, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
  }
  b3 %<-% {
    data[l3, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
  } 
  b4 %<-% {
    data[l4, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
  } 
  rbind(b1, b2, b3, b4)
}
beta_sample_f <- get_betas(paras)


# for (i in seq(1, nrow(paras), 2)) {
#   a %<-% {
#     beta_sample <- paras[i, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed)]
#   } 
# }
# 
# for (i in seq(2, nrow(paras), 2)) {
#   b %<-% {
#     beta_sample <- paras[i, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed)]
#   } 
# }
# 
# 
# X <- matrix(c(1:4, 1, 6:8), nrow = 2L)

library(microbenchmark)
library(ggplot2)

mbm <- microbenchmark::microbenchmark("y0" = {
  beta_sample <- paras[, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
},
"y1" = {
  beta_sample_f <- get_betas(paras)
},
times = 10L)

autoplot(mbm)


library(rbenchmark)

benchmark("y0" = {
  beta_sample <- paras[, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
},
"y1" = {
  plan(list(multisession, multisession))
  
  beta_sample_f %<-% {
    b1 %<-% {
      paras[1:250, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
    }
    b2 %<-% {
      paras[251:500, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
    }
    rbind(b1, b2)
  }
  beta_sample_f
},
replications = 10,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))


beta_sample <- paras[, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]

overview <- beta_sample[, .(mean_diff_dfe = abs(mean(b_ps1[dfe_id == dfes[1]]) - mean(b_ps1[dfe_id == dfes[2]])), 
                            sd_diff_dfe = abs(sd(b_ps1[dfe_id == dfes[1]]) - sd(b_ps1[dfe_id == dfes[2]])),
                            median_diff_dfe = abs(median(b_ps1[dfe_id == dfes[1]]) - median(b_ps1[dfe_id == dfes[2]])),
                            fstq_diff_dfe = abs(quantile(b_ps1[dfe_id == dfes[1]], prob = .25) - quantile(b_ps1[dfe_id == dfes[2]], prob = .25)),
                            thrq_diff_dfe = abs(quantile(b_ps1[dfe_id == dfes[1]], prob = .75) - quantile(b_ps1[dfe_id == dfes[2]], prob = .75)),
                            var_dfe1 = abs(var(b_ps1[dfe_id == dfes[1]])),
                            var_dfe2 = abs(var(b_ps1[dfe_id == dfes[2]]))),
                        keyby = .(beta_n, seed)]
overview <- overview[, .(mean_diff_dfe = mean(mean_diff_dfe),
                         sd_diff_dfe = mean(sd_diff_dfe),
                         median_diff_dfe = mean(median_diff_dfe),
                         fstq_diff_dfe = mean(fstq_diff_dfe),
                         thrq_diff_dfe = mean(thrq_diff_dfe),
                         var_dfe1 = var(var_dfe1),
                         var_dfe2 = var(var_dfe2)), 
                     by = .(beta_n)]
overview[, (2:6) := round(.SD, 3), .SDcols = c(2:6)]

# beta_sample <- NULL
# for (i in 1:length(seed)) {
#   if (i == 1) {
#     beta_sample <- paras[, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed == seed[i])]
#   } else {
#     beta_sample <- rbind(beta_sample, paras[, get_sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed == seed[i])])
#   }
# }

# beta_sample <- c(sapply(X = paras, FUN = get_sample, s1 = s1, s2 = s2, r1 = r1, r2 = r2, ps1 = ps1, pr1 = pr2, dfe_n = dfe_n, subject_n = subject_n, budget = budget, beta_n = beta_n, prior = prior, seed = seed))

if (!exists("data_summary")) {
  data_summary <- overview[, id := paste(budget, ps1, s1, s2, pr1, r1, r2, sep = "_")]
} else {
  data_summary <- rbind(data_summary, overview[, id := paste(budget, ps1, s1, s2, pr1, r1, r2, sep = "_")])
}




data_plot <- beta_sample[beta_n %in% list(10, 50, 250, 1000, 3000)]
the_name <- "Sample Size"
the_labels <- c("Large (10 per option)", "Small (3 per option)")
the_colors <- c("red", "gray25")
ggplot(
  data = data_plot,
  mapping = aes(x = seed, y = b_ps1, color = dfe_id, fill = dfe_id)) +
  stat_halfeye( # uses median and QI = quantile interval (also known as the percentile interval or equi-tailed interval)
    .width = c(.66, 0.95), #use .66, .95 to show 66 and 96% HDI
    slab_alpha = 0.15,
    position = position_dodge(width = .09),
    aes(shape = dfe_id), point_fill = "white") +
  theme_classic() +
  scale_fill_manual(
    values = the_colors,
    name = the_name, labels = the_labels) +
  scale_colour_manual(
    values = the_colors,
    name = the_name, labels = the_labels) +
  scale_shape_manual(
    values = c(16, 21),
    name = the_name, labels = the_labels) +
  facet_wrap(~ beta_n + seed, labeller = label_both, scale = "free_x") +
  ylab("Predicted Proportion of Risky Choices") +
  ylim(c(0,1)) +
  labs(title = "Environment",
       subtitle = paste("Reach", data_plot$budget[1], "in 3 trials"),
       caption = paste0("Beta_n = ", max(data_plot$beta_id)/max(data_plot$subject_id), 
                        ", Subject_n = ", max(data_plot$subject_id)/(length(unique(data_plot$dfe_id))*length(unique(data_plot$dfe_id))))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.key.size = unit(.2, 'cm'),
        legend.key.height= unit(.5, 'cm'),
        legend.key.width= unit(.5, 'cm'),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5))






library(microbenchmark)


# mbm <- microbenchmark("new" = {
#   cores <- 4
#   paras[, core := c(rep(1, times = nrow(paras)-floor(nrow(paras)/cores)*cores), 
#                    rep(c(1:cores), each = floor(nrow(paras)/cores)))]
#   # paras <- as.data.frame(paras)
#   cl <- parallel::makeCluster(2)
#   doParallel::registerDoParallel(cl)
#   parallel::clusterExport(cl, c("Get_Sample", "paras", "cores"))
#   beta_sample_f <- foreach(x = 1:cores, .combine = "rbind") %dopar% {
#     Get_Sample(s1 = paras$s1[core == x], s2 = paras$s2[core == x], r1 = paras$r1[core == x], r2 = paras$r2[core == x], ps1 = paras$ps1[core == x], pr1 = paras$pr1[core == x], dfe_n = paras$dfe_n[core == x], subject_n = paras$subject_n[core == x], budget = paras$budget[core == x], beta_n = paras$beta_n[core == x], prior = paras$prior[core == x], seed = paras$seed[core == x])
#     # paras[core == x, Get_Sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed)]
#   # beta_sample_f <- paras[, Get_Betas_2(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed, core), by = para_id]
#   # beta_sample_f <- bind_rows(beta_sample_f, .id = "para_id")
#   }
# },
# "old" = {
#   beta_sample <- paras[, Get_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed), by = para_id]
# },
# times = 10L)
# 
# ggplot2::autoplot(mbm)





# cores <- min(availableCores(), nrow(paras))
# paras[, core := rep(1:cores, length.out = nrow(paras))]
# cl <- parallel::makeCluster(2)
# doParallel::registerDoParallel(cl)
# parallel::clusterExport(cl, c("Apply_Sample", "Get_Sample", "data.table", "paras", "cores"))
# beta_sample_f <- foreach(x = 1:cores, .combine = "rbind") %dopar% {
#   # paras[core == x, Apply_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed), by = para_id]
#   # Get_Sample(s1 = paras$s1[core == x], s2 = paras$s2[core == x], r1 = paras$r1[core == x], r2 = paras$r2[core == x], ps1 = paras$ps1[core == x], pr1 = paras$pr1[core == x], dfe_n = paras$dfe_n[core == x], subject_n = paras$subject_n[core == x], budget = paras$budget[core == x], beta_n = paras$beta_n[core == x], prior = paras$prior[core == x], seed = paras$seed[core == x])
#   # paras[core == x, Get_Sample(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed)]
#   # beta_sample_f <- paras[, Get_Betas_2(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, prior, seed, core), by = para_id]
#   # beta_sample_f <- bind_rows(beta_sample_f, .id = "para_id")
#   do.call(Map, c(f = Get_Sample, paras[core == x, .(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed)]))
# }
# parallel::stopCluster(cl)
# beta_sample_f <- rbindlist(beta_sample_f)


# cores <- 4
# paras[, core := c(rep(1, times = nrow(paras)-floor(nrow(paras)/cores)*cores), 
#                   rep(c(1:cores), each = floor(nrow(paras)/cores)))]
# # paras <- as.data.frame(paras)
# cl <- parallel::makeCluster(2)
# doParallel::registerDoParallel(cl)
# parallel::clusterExport(cl, c("Get_Sample", "paras", "cores"))
# # Packages <- c("data.table", "tidybayes", "patchwork", "magrittr")
# parallel::clusterCall(cl, function() library(data.table))
# beta_sample_f <- paras[, Get_Betas_2(s1, s2 , r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed), by = para_id]


cores <- min(availableCores(), nrow(paras))
paras[, core := rep(1:cores, length.out = nrow(paras))]
registerDoFuture()
plan(multisession)
beta_sample_f <- foreach(x = 1:cores, .export = c("Apply_Sample", "Get_Sample", "data.table", "paras", "cores"), .combine = "rbind") %dopar% {
  # Get_Sample(s1 = paras[core == x]$s1, s2 = paras[core == x]$s2, r1 = paras[core == x]$r1, r2 = paras[core == x]$r2,
  #              ps1 = paras[core == x]$ps1, pr1 = paras[core == x]$pr1, dfe_n = paras[core == x]$dfe_n, subject_n = paras[core == x]$subject_n, 
  #              budget = paras[core == x]$budget, beta_n = paras[core == x]$beta_n, seed = paras[core == x]$seed)
  # paras[core == x, Apply_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed), by = para_id]
  do.call(Map, c(f = Get_Sample, paras[core == x, .(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed)]))
}
b <- rbindlist(beta_sample_f)



# paras[, Get_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed), by = para_id]
b <- paras[, Get_Sample(s1 = s1, s2 = s2, r1 = r1, r2 = r2, ps1 = ps1, pr1 = pr1, dfe_n = dfe_n, subject_n = subject_n, budget = budget, beta_n = beta_n, seed = seed), by = para_id]
rbindlist(do.call(Map, c(f = Get_Sample, paras[, .(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed)])))
rbindlist(apply(paras, 1, FUN = function(x) Apply_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed)))




mbm <- microbenchmark::microbenchmark("normal" = {
  sim <- paras[, Apply_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed), by = para_id]
},
# "parallel" = {
#   cores <- min(availableCores(), nrow(paras))
#   paras[, core := rep(1:cores, length.out = nrow(paras))]
#   cl <- parallel::makeCluster(cores)
#   doParallel::registerDoParallel(cl)
#   parallel::clusterExport(cl, c("Apply_Sample", "Get_Sample", "data.table", "paras", "cores"))
#   beta_sample_f <- foreach(x = 1:cores, .combine = "rbind") %dopar% {
#     do.call(Map, c(f = Get_Sample, paras[core == x, .(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed)]))
#   }
#   parallel::stopCluster(cl)
#   rbindlist(beta_sample_f)
# },
"future" = {
  registerDoFuture()
  plan(multisession)
  beta_sample_f <- foreach(x = 1:cores, .export = c("Apply_Sample", "Get_Sample", "data.table", "paras", "cores"), .combine = "rbind") %dopar% {
    do.call(Map, c(f = Get_Sample, paras[core == x, .(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed)]))
  }
  beta_sample_f <- rbindlist(beta_sample_f)
},
times = 10L)

ggplot2::autoplot(mbm)




mbm <- microbenchmark::microbenchmark("data.table" = {
  paras[, Get_Sample(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed), by = para_id]
},
"do.call" = {
  rbindlist(do.call(Map, c(f = Get_Sample, paras[, .(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed)])))
},
times = 10L)

ggplot2::autoplot(mbm)



# Clear environment and set working directory-----------------------------------
rm(list = ls())
gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Load Package------------------------------------------------------------------
library(ggplot2)
library(data.table)
library(cognitivemodels)
library(tidybayes)
library(patchwork)
library(psych)
library(future)
library(doFuture)


# Source Scripts----------------------------------------------------------------
source("Get_Sample.R")
source("Get_Model.R")
source("Get_Comparison.R")
source("Get_Plot.R")
source("Get_Predictions.R")
source("Apply_Sample.R")


# Set Parameters----------------------------------------------------------------
ps1 <- 0.5
s1 <- 6
s2 <- 8
pr1 <- 0.25
r1 <- 1
r2 <- 9
prior <- c(1, 1)

subject_n <- 50
beta_n <- seq(10, 500, 10)
budget <- 18
dfe_n <- c(3, 10)

seed <- 42

ntrials <- 3


# Execution code----------------------------------------------------------------

# Create dataset with all possible combinations of parameters
paras <- as.data.table(expand.grid(beta_n = beta_n, budget = budget, subject_n = subject_n, ntrials = ntrials, seed = seed, dfe_n = dfe_n))
paras[, `:=` (ps1 = ps1, s1 = s1, s2 = s2, pr1 = pr1, r1 = r1, r2 = r2, para_id = .I)]
cores <- min(availableCores(), nrow(paras))
paras[, core := rep(1:cores, length.out = nrow(paras))]

parallel <- "y"

if (parallel == "y") {
  registerDoFuture()
  plan(multisession)
  system.time({
    beta_sample_f <- foreach(x = 1:cores, .export = c("Apply_Sample", "Get_Sample", "data.table", "paras", "cores"), .combine = "rbind") %dorng% {
      paras[core == x, Get_Sample(s1 = s1, s2 = s2, r1 = r1, r2 = r2, ps1 = ps1, pr1 = pr1, dfe_n = dfe_n, subject_n = subject_n, budget = budget, beta_n = beta_n, seed = seed), by = para_id]
    }
  })
} else {
  system.time({
    beta_sample_n <- do.call(Map, c(f = Get_Sample, paras[, .(s1, s2, r1, r2, ps1, pr1, dfe_n, subject_n, budget, beta_n, seed)]))
  })
}



beta_sample_f <- rbindlist(beta_sample_f)
beta_sample_n <- rbindlist(beta_sample_n)

dim(beta_sample_f)
dim(beta_sample_n)




