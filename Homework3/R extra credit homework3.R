library(dplyr)
library(tidyr)


data <- read.csv("~/Desktop/Collaboration_MGB293/Homework3/demographics.csv")

data_dummy <- data %>%
  mutate(X_LA = ifelse(Region == "Los Angeles", 1,0),
         X_SB = ifelse(Region == "Santa Barbara", 1,0),
         X_SD = ifelse(Region == "San Diego", 1,0),
         X_FRE = ifelse(Region == "Fresno", 1,0),
         X_INC2 = ifelse(Income == "High", 1,0)
         )



data_disney1 <- data_dummy %>%
  mutate(alpha = (X_LA*-0.7 + X_SB*-0.2 + X_SD*-0.5 + X_FRE*-0.4 + X_INC2*0.25),
         beta1 = (X_LA*-0.06 + X_SB*-0.06 + X_SD*-0.06 + X_FRE*-0.07 + X_INC2*0.02),
         beta2 = (X_LA*-0.2 + X_SB*-0.2 + X_SD*-0.2 + X_FRE*-0.25 + X_INC2*0.1),
         discount_rate = c(0.19,0.15,0.17,0.11, 0.14, 0.12, 0.13,0.11),
         price = 260*(1-discount_rate),
         discount = -206*discount_rate,
         sqrt_discount = -sqrt(-discount),
         utility = alpha + beta1*discount + beta2*sqrt_discount,
         probablity = (exp(utility)/(1+exp(utility))),
         ev = probablity*price
        )
        
revenue_disney1 <- sum(data_disney1$ev)    





data_disney2 <- data_dummy%>%
  mutate(alpha_gc = (X_LA*-4 + X_SB*-3 + X_SD*-3 + X_FRE*-4 + X_INC2*4.5),
         alpha_dr = (X_LA*-3 + X_SB*-2 + X_SD*-1 + X_FRE*-1 + X_INC2*0),
         beta_gc = (X_LA*-0.09 + X_SB*-0.1 + X_SD*-0.1 + X_FRE*-0.11 + X_INC2*0.07),
         beta_dr = (X_LA*-0.07 + X_SB*-0.09 + X_SD*-0.09 + X_FRE*-0.09 + X_INC2*0.07),
         discount_rate = c(0.15, 0.07, 0.07, 0.14, 0.19, 0.15, 0.13, 0.14),
         base_gc = c (420, 420, 420, 420, 420, 420, 420, 420),
         price_gc = base_gc*(1-discount_rate),
         dis_gc = price_gc - base_gc,
         utility_gc = alpha_gc + beta_gc*dis_gc,
         base_dr = c (360, 360, 360, 360, 360, 360, 360, 360),
         price_dr = base_dr*(1-discount_rate),
         dis_dr = price_dr - base_dr,
         utility_dr = alpha_dr + beta_dr*dis_dr,
         pr_gc = exp(utility_gc)/(exp(utility_dr)+exp(utility_gc)+exp(0)),
         pr_dr = exp(utility_dr)/(exp(utility_dr)+exp(utility_gc)+exp(0)),
         pr_none = 1- pr_gc - pr_dr,
         exp_profit = pr_gc*price_gc + pr_dr*price_dr
         
              )

                           
  revenue_disney2 = sum(data_disney2$exp_profit)                        
                          
                
        
