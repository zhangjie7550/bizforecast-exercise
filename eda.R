library(tidyverse)
library(ggthemes)
# set random seed
set.seed(1212)


# set theme
old <- theme_get()
new <- theme_set(theme_tufte() + theme(text = element_text(family = "Noto Sans CJK SC"),
                                       legend.position = "bottom"))

# read data
dat <- read.csv("Data/cpl_and_conversion.csv") %>% as_tibble()


# Q1月利润分布 -------------------------

# 单日利润
Profit_per_day <- function(){
    
    # 毛利M
    M <- runif(1, min = 350, max = 400)
    # 栗子数量
    L <- runif(1, min = 3000, max = 4000)
    # 栗子转化率 conversion rate
    R <- rnorm(1, mean = .04, sd = .005)
    # 收入I = 毛利M * 栗子数量L * 栗子转化率R
    I <- M * L * R
    
    # 固定成本
    H <- 20000
    # 单个栗子成本
    Cpl <- runif(1, min = 8, max = 10)
    # 开销E = 固定成本H + 栗子数量L * 单个栗子成本Cpl
    E <- H + L * Cpl
    
    # 利润P = 收入I- 开销E
    P <- I - E
}




# 月利润
Profit_per_month <- function(){
    #令一个月30天
    profit <- replicate(30, Profit_per_day()) %>% sum()
}

# 月利润的分布
profit_distribution <- replicate(3000, Profit_per_month())

# 利润分布图
pd <- profit_distribution %>% as_tibble()
m <- mean(profit_distribution)

pd %>% 
    ggplot(aes(x = value)) +
    geom_density() +
    geom_vline(xintercept = quantile(profit_distribution, probs = c(.025, .5,.975)), lty = 3) +
    labs(x = "月利润")

# 95%置信区间
quantile(profit_distribution, probs = c(.025, .975))

# Q2 利润达成概率、亏损概率------------
pd %>% 
    mutate(goal = ifelse(value >= 100000, 1, 0),
           loss = ifelse(value < 0, 1, 0)) %>% 
    summarise(goal_prob = mean(goal),
              loss_prob = mean(loss))

# Q3 利润的累积函数---------------------
pd %>% 
    ggplot(aes(value)) + 
    stat_ecdf(geom = "step") +
    labs(x = "月利润")


# Q4 修正栗子转化率 ------------------

# 建立Cpl 与 R(conversion rate)的关联
p <- lm(dat$conversion_rate ~ dat$cost_per_lead)


Profit_per_day_new <- function(){
    
    
    # 固定成本
    H <- 20000
    # 栗子数量
    L <- runif(1, min = 3000, max = 4000)
    # 单个栗子成本
    Cpl <- runif(1, min = 8, max = 10)
    # 开销E = 固定成本H + 栗子数量L * 单个栗子成本Cpl
    E <- H + L * Cpl
    
    
    # 毛利M
    M <- runif(1, min = 350, max = 400)
    # 栗子转化率 conversion rate, 使用栗子成本计算
    R <- Cpl * 0.0246 - 0.1810
    
    # 收入I = 毛利M * 栗子数量L * 栗子转化率R
    I <- M * L * R
    
    # 利润P = 收入I- 开销E
    P <- I - E
}

# 月利润
Profit_per_month_new <- function(){
    #令一个月30天
    profit <- replicate(30, Profit_per_day_new()) %>% sum()
}

# 月利润的分布
profit_distribution_new <- replicate(3000, Profit_per_month_new())

# 利润分布图
pd_new <- profit_distribution_new %>% as_tibble()
m <- mean(profit_distribution_new)

pd_new %>% 
    ggplot(aes(x = value)) +
    geom_density() +
    geom_vline(xintercept = quantile(profit_distribution_new, probs = c(.025, .5,.975)), lty = 3) +
    labs(x = "月利润")

# 95%置信区间
quantile(profit_distribution_new, probs = c(.025, .975))

# 目标达成与亏损概率
pd_new %>% 
    mutate(goal = ifelse(value >= 100000, 1, 0),
           loss = ifelse(value < 0, 1, 0)) %>% 
    summarise(goal_prob = mean(goal),
              loss_prob = mean(loss))

# 累积函数

pd_new %>% 
    ggplot(aes(value)) + 
    stat_ecdf(geom = "step") +
    labs(x = "月利润")

# Q5 增加成本与栗子数量的方案对比--------------------


Profit_per_day_increase_cost <- function(){
    
    
    # 固定成本, 增加25%-30%
    H <- 20000 * runif(1, 1.25, 1.3)
    # 栗子数量, 增加15%-25%
    L <- runif(1, min = 3000, max = 4000) * runif(1, 1.15, 1.25)
    # 单个栗子成本
    Cpl <- runif(1, min = 8, max = 10)
    # 开销E = 固定成本H + 栗子数量L * 单个栗子成本Cpl
    E <- H + L * Cpl
    
    
    # 毛利M
    M <- runif(1, min = 350, max = 400)
    # 栗子转化率 conversion rate, 使用栗子成本计算
    R <- Cpl * 0.0246 - 0.1810
    
    # 收入I = 毛利M * 栗子数量L * 栗子转化率R
    I <- M * L * R
    
    # 利润P = 收入I- 开销E
    P <- I - E
}

# 比较两种方案的月收入,返回原方案-新方案的差
Profit_compare <- function(){
    #令一个月30天
    profit_original <- replicate(30, Profit_per_day_new()) %>% sum()
    profit_increase_cost <- replicate(30, Profit_per_day_increase_cost()) %>% sum()
    # return difference
    profit_dif <- profit_original-profit_increase_cost
}

# 抽样比较
profit_dif <- replicate(3000, Profit_compare())

# 收入差异比较
profit_dif %>% as_tibble() %>% 
    ggplot(aes(value)) +
    geom_density() +
    geom_vline(xintercept = c(quantile(profit_dif, probs = c(.025, .5, .975))), lty = 3) +
    labs(x = "收入差分布")

# 计算增加成本方案的结果
# 月利润
Profit_per_month_increase_cost <- function(){
    #令一个月30天
    profit <- replicate(30, Profit_per_day_increase_cost()) %>% sum()
}

# 月利润的分布
profit_distribution_increase_cost <- replicate(3000, Profit_per_month_increase_cost())

quantile(profit_distribution_increase_cost, probs = c(.025, .975))

# 利用crossbar 展示
df <- data.frame(
    cat = factor(c("original", "increase_cost")),
    m = c(quantile(profit_distribution_new,probs=.5), quantile(profit_distribution_increase_cost,probs=.5)),
    l = c(quantile(profit_distribution_new,probs=.025), quantile(profit_distribution_increase_cost,probs=.025)),
    u = c(quantile(profit_distribution_new,probs=.975), quantile(profit_distribution_increase_cost,probs=.975))
)

df %>% 
    ggplot(aes(x=cat, y=m, col=cat)) +
    geom_crossbar(aes(ymin = l, ymax = u),width=.05)
