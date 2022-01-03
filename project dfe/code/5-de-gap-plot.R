library(ggplot2)
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- data.frame( x = factor(1:4, labels = c("Hertwig (2004) \n No Goal", "High Goal \n 30", "Low Goal \n 8", "Insignificant Goal \n 1")),
                    desc = c(.64,.80,.12,.64), exp = c(.12,.05,.12,.12))

data <- melt(data,
     id = 1)                    

ggplot(data, aes(x=x, y=value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5)+
  scale_fill_manual(values = c("blue","black"), labels = c("Description", "Experience")) +
  theme_classic(base_size = 18, base_family = "Arial") +
  labs(x = "Condition", y = "Predicted Proportion of Risky Choices", fill = "Format",
       title = "Reach goal in 3 trials", subtitle = "Risky: 32, 2.5% or 0      Safe: 3, 25% or 0")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))

ggsave("../figures/degap.png",width = 12, height = 6)
