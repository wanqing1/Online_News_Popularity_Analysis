library(tidyverse)
# Parameter: change in avg number of shares from 2013 to 2014
# SRS
# Note: data from 2013 and 2014 are independent
news <- read.csv("OnlineNewsPopularity.csv", header = T) %>%
  mutate(year_of_publish = as.integer(str_extract(url, "\\d{4}")),
         Positive_article = 
           ifelse(rate_positive_words -  rate_negative_words > 0, 1, 0),
         channel = case_when(
           data_channel_is_lifestyle == 1 ~ "lifestyle",
           data_channel_is_entertainment == 1 ~ "entertainment",
           data_channel_is_bus == 1 ~ "business",
           data_channel_is_socmed == 1 ~ "socmed",
           data_channel_is_tech == 1 ~ "tech",
           data_channel_is_world == 1 ~ "world",
           TRUE ~ "other"
         )) %>%
  select(year_of_publish, channel, Positive_article, shares)

# set seed, sample from 2013 and 2014
set.seed(10)
n <- 1000
sample_2013 <- news |>
  filter(year_of_publish == 2013) |>
  sample_n(size = n, replace = FALSE)
sample_2014 <- news |>
  filter(year_of_publish == 2014) |>
  sample_n(size = n, replace = FALSE)

# calculate average of shares
mean_2013 <- mean(sample_2013$shares)
mean_2014 <- mean(sample_2014$shares)

# vanilla estimator
mean_diff <- mean_2014 - mean_2013
mean_diff

# calculate se of shares
var_2013 <- var(sample_2013$shares)
var_2014 <- var(sample_2014$shares)
N_2013 <- news |>
  filter(year_of_publish == 2013) |>
  nrow()
N_2014 <- news |>
  filter(year_of_publish == 2014) |>
  nrow()
se_mean_diff <- sqrt((1 - n / N_2013) * var_2013 / n + 
                       (1 - n / N_2014) * var_2014 / n)
se_mean_diff

# construct 95% confidence interval
CI <- data.frame(
  lower_ci = mean_diff - 1.96 * se_mean_diff,
  upper_ci = mean_diff + 1.96 * se_mean_diff
)
CI

# Since the confidence interval contains 0, we cannot conclude if 
# there is a change of average shares between 2013 and 2014. 



# Parameter: change in population proportion of positive articles from 2013 to
# 2014
# SRS

#2013 subset
news_2013 <- news %>%
  filter(year_of_publish == 2013)

#2014 subset
news_2014 <- news %>%
  filter(year_of_publish == 2014)

#SRS: set sample size = 1000 for each year
set.seed(10)
combined_srs_prop <- as.data.frame(
  cbind(sample_2013$Positive_article, sample_2014$Positive_article))
colnames(combined_srs_prop) <- c("positive_2013", "positive_2014")

srs_2013_prop <- combined_srs_prop %>%
  summarize(positive_prop_2013 = mean(positive_2013 == 1)) %>%
  pull()

srs_2014_prop <- combined_srs_prop %>%
  summarize(positive_prop_2014 = mean(positive_2014 == 1)) %>%
  pull()

# Estimate of change in population proportion (vanilla)
srs_prop_estimate <- srs_2014_prop - srs_2013_prop
srs_prop_estimate

# Standard error of our estimate
srs_prop_estimate_se <- sqrt(
  srs_2013_prop * (1 - srs_2013_prop) / sample_size_each_yr * 
    (1 - sample_size_each_yr  / nrow(news_2013)) + # FPC
    srs_2014_prop * (1 - srs_2014_prop) / sample_size_each_yr *
    (1 - sample_size_each_yr  / nrow(news_2014))) # FPC
srs_prop_estimate_se
# 95% confidence interval for change of population proportion
srs_prop_ci <- data.frame(
  lower_ci = srs_prop_estimate - 1.96 * srs_prop_estimate_se,
  upper_ci = srs_prop_estimate + 1.96 * srs_prop_estimate_se
)
srs_prop_ci
# Since 95% confidence interval excludes 0, we conclude that there may be a
# change, specifically a decrease, of the proportion of positive articles from
# 2013 to 2014.


# Parameter: change in avg number of shares from 2013 to 2014
# Stratified sampling

news_2013$channel <- as.factor(news_2013$channel)
news_2014$channel <- as.factor(news_2014$channel)


#between stratum variance 2013
attach(news_2013)
N_2013 <- length(shares)
N_h2013 <- tapply(shares, channel, length)
N_h2013

avg_all <- mean(news_2013$shares)
avg_b <- mean(news_2013$shares[news_2013$channel == "business"])
avg_e <- mean(news_2013$shares[news_2013$channel == "entertainment"])
avg_l <- mean(news_2013$shares[news_2013$channel == "lifestyle"])
avg_s <- mean(news_2013$shares[news_2013$channel == "socmed"])
avg_t <- mean(news_2013$shares[news_2013$channel == "tech"])
avg_w <- mean(news_2013$shares[news_2013$channel == "world"])
avg_o <- mean(news_2013$shares[news_2013$channel == "other"])

avg_stratum <- c(avg_b, avg_e, avg_l, avg_o, avg_s, avg_t, avg_w)
var_between <- sum((avg_stratum-avg_all)^2*(N_h2013/N_2013))
var_between

#stratum size
s_b <- sd(news_2013$shares[news_2013$channel == "business"])
s_e <- sd(news_2013$shares[news_2013$channel == "entertainment"])
s_l <- sd(news_2013$shares[news_2013$channel == "lifestyle"])
s_s <- sd(news_2013$shares[news_2013$channel == "socmed"])
s_t <- sd(news_2013$shares[news_2013$channel == "tech"])
s_w <- sd(news_2013$shares[news_2013$channel == "world"])
s_o <- sd(news_2013$shares[news_2013$channel == "other"])

s_all <- c(s_b, s_e, s_l,  s_o, s_s, s_t, s_w)
var_within <- sum((s_all^2)*(N_h2013/N_2013))
var_within

n <- 1000
n_h2013 <- (N_h2013*s_all/sum(N_h2013*s_all))*n
n_h2013
n_h2013 <- round(n_h2013)
n_h2013
detach(news_2013)

#stratum mean
attach(news_2013)
channels <- unique(channel)

STR.sample.prop2013 <- NULL
set.seed(10)
for (i in 1:length(channels)){
  row.indices <- which(channel == channels[i])
  sample.indices <- sample(row.indices, n_h2013[i], replace=FALSE)
  STR.sample.prop2013 <- rbind(STR.sample.prop2013, news_2013[sample.indices,])
}
ybar_h2013 <- tapply(STR.sample.prop2013$shares, STR.sample.prop2013$channel, mean)
var_h2013 <- tapply(STR.sample.prop2013$shares, STR.sample.prop2013$channel, var)
se_ybar_h2013 <- sqrt((1-n_h2013/N_h2013)*var_h2013/n_h2013)
rbind(ybar_h2013, se_ybar_h2013)

detach(news_2013)

#stratum mean
ybar_str2013 <- sum(ybar_h2013*(N_h2013/N_2013))
SE_ybar_str2013 <- sqrt(sum(se_ybar_h2013^2*(N_h2013/N_2013)^2))
str.pop2013 <- c(ybar_str2013, SE_ybar_str2013)
cat("stratified mean",":", ybar_str2013,"\n")
cat("stratified SE",":", SE_ybar_str2013,"\n")


#For 2014
attach(news_2014)

N_2014 <- length(shares)
N_2014

N_h2014 <- tapply(shares, channel, length)
N_h2014

avg_all2014 <- mean(news_2014$shares)
avg_b2014 <- mean(news_2014$shares[news_2014$channel == "business"])
avg_e2014 <- mean(news_2014$shares[news_2014$channel == "entertainment"])
avg_l2014 <- mean(news_2014$shares[news_2014$channel == "lifestyle"])
avg_s2014 <- mean(news_2014$shares[news_2014$channel == "socmed"])
avg_t2014 <- mean(news_2014$shares[news_2014$channel == "tech"])
avg_w2014 <- mean(news_2014$shares[news_2014$channel == "world"])
avg_o2014 <- mean(news_2014$shares[news_2014$channel == "other"])

avg_stratum2014 <- c(avg_b2014, avg_e2014, avg_l2014, avg_o2014, avg_s2014, avg_t2014, avg_w2014)

var_between2014 <- sum((avg_stratum2014-avg_all2014)^2*(N_h2014/N_2014))
var_between2014


s_b2014 <- sd(news_2014$shares[news_2014$channel == "business"])
s_e2014 <- sd(news_2014$shares[news_2014$channel == "entertainment"])
s_l2014 <- sd(news_2014$shares[news_2014$channel == "lifestyle"])
s_s2014 <- sd(news_2014$shares[news_2014$channel == "socmed"])
s_t2014 <- sd(news_2014$shares[news_2014$channel == "tech"])
s_w2014 <- sd(news_2014$shares[news_2014$channel == "world"])
s_o2014 <- sd(news_2014$shares[news_2014$channel == "other"])

s_all2014 <- c(s_b2014, s_e2014, s_l2014,  s_o2014, s_s2014, s_t2014, s_w2014)
var_within2014 <- sum((s_all2014^2)*(N_h2014/N_2014))


n <- 1000
n_h2014 <- (N_h2014*s_all2014/sum(N_h2014*s_all2014))*n
n_h2014
sum(n_h2014)
n_h2014 <- round(n_h2014)
n_h2014

detach(news_2014)

#stratum mean
attach(news_2014)
channels <- unique(channel)

STR.sample.prop2014 <- NULL
set.seed(10)
for (i in 1:length(channels)){
  row.indices <- which(channel == channels[i])
  sample.indices <- sample(row.indices, n_h2014[i], replace=FALSE)
  STR.sample.prop2014 <- rbind(STR.sample.prop2014, news_2014[sample.indices,])
}
ybar_h2014 <- tapply(STR.sample.prop2014$shares, STR.sample.prop2014$channel, mean)
var_h2014 <- tapply(STR.sample.prop2014$shares, STR.sample.prop2014$channel, var)
se_ybar_h2014 <- sqrt((1-n_h2014/N_h2014)*var_h2014/n_h2014)
rbind(ybar_h2014, se_ybar_h2014)

detach(news_2014)

#stratum mean
ybar_str2014 <- sum(ybar_h2014*(N_h2014/N_2014))
SE_ybar_str2014 <- sqrt(sum(se_ybar_h2014^2*(N_h2014/N_2014)^2))
str.pop2014 <- c(ybar_str2014, SE_ybar_str2014)

rbind(c("stratified mean","stratified SE"), round(str.pop2013,digits = 2)
      , round(str.pop2014, digits = 2))


#CI
MeanDiff <- ybar_str2014 - ybar_str2013
SE_MeanDiff <- sqrt(SE_ybar_str2013^2 + SE_ybar_str2014^2)
CI <- c(MeanDiff - 1.96*SE_MeanDiff, MeanDiff + 1.96*SE_MeanDiff)
CI
#The CI contains 0, and the difference is very small as we can tell.

# Parameter:  change in population proportion of positive articles from 2013 to
# 2014
# Stratified sampling
news_2013$Positive_article <- as.factor(news_2013$Positive_article)
news_2014$Positive_article <- as.factor(news_2014$Positive_article)

#stratum size 2013 optimal allocation
attach(news_2013)

positive_2013 <- news_2013[news_2013$Positive_article == "1",]
negative_2013 <- news_2013[news_2013$Positive_article == "0",]
N_Pos_2013<- tapply(positive_2013$Positive_article, positive_2013$channel, length)
N_Neg_2013<- N_h2013 - N_Pos_2013
prop_2013 <- N_Pos_2013/N_h2013
avg_prop_2013 <- sum(N_Pos_2013)/sum(N_h2013)

prop_sd_within <- sqrt(prop_2013 * (1-prop_2013) / N_h2013)

var_h2013_between <- (prop_2013 - avg_prop_2013)^2
sum(var_h2013_between)
sum(prop_sd_within^2)

n <- 1000
n_h2013_prop <- (N_h2013*prop_sd_within/sum(N_h2013*prop_sd_within))*n
n_h2013_prop
n_h2013_prop <- round(n_h2013_prop)
n_h2013_prop
n <- sum(n_h2013_prop)  #adjust sample size due to rounding

detach(news_2013)

#strata size 2014 optimal allocation
attach(news_2014)

positive_2014 <- news_2014[news_2014$Positive_article == "1",]
negative_2014 <- news_2014[news_2014$Positive_article == "0",]
N_Pos_2014<-tapply(positive_2014$Positive_article, positive_2014$channel, length)
N_Neg_2014<-N_h2014 - N_Pos_2014
prop_2014 <- N_Pos_2014/N_h2014
avg_prop_2014 <- sum(N_Pos_2014)/sum(N_h2014)

prop_sd_within2014 <- sqrt(prop_2014 * (1-prop_2014) / N_h2014)

var_h2014_between <- (prop_2014 - avg_prop_2014)^2
sum(var_h2014_between)
sum(prop_sd_within2014^2)

n <- 1000
n_h2014_prop <- (N_h2014*prop_sd_within/sum(N_h2014*prop_sd_within))*n
n_h2014_prop
n_h2014_prop <- round(n_h2014_prop)
n_h2014_prop
n <- sum(n_h2014_prop)  #adjust sample size due to rounding

detach(news_2014)

#positive article proportion in 2013 using stratified sampling
attach(news_2013)
set.seed(10)
##partitioning data by chanel
channels <- unique(channel)
STR.sample.prop2013 <- NULL
for (i in 1:length(channels)){
  row.indices <- which(channel == channels[i])
  sample.indices <- sample(row.indices, n_h2013_prop[i], replace=FALSE)
  STR.sample.prop2013 <- rbind(STR.sample.prop2013, news_2013[sample.indices,])
}
##get proportion of positive article inside each chanel
###get size for positive and negative artical under each chanel
STR_2013_positive <- STR.sample.prop2013[STR.sample.prop2013$Positive_article == "1",]
STR_2013_negative <- STR.sample.prop2013[STR.sample.prop2013$Positive_article == "0",]
STR_n_Pos_2013<-tapply(STR_2013_positive$shares, STR_2013_positive$channel, length)
STR_n_Neg_2013<-tapply(STR_2013_negative$shares, STR_2013_negative$channel, length)
t_STR_n_2013<-STR_n_Pos_2013+STR_n_Neg_2013

###estimated proportion of positive article in 2013
str_2013_prop <- STR_n_Pos_2013/(t_STR_n_2013)
prop_bar_2013 <- sum(str_2013_prop*(N_h2013/N_2013))
prop_bar_2013
### calculate SE for the estimate
str_2013_prop_bar_var <- (1-n_h2013_prop/N_h2013)*
  str_2013_prop*(1-str_2013_prop)/(t_STR_n_2013)
prop_bar_2013_se <- sqrt(sum(str_2013_prop_bar_var*
                               (N_h2013/N_2013)^2))
prop_bar_2013_se
detach(news_2013)


#positive article proportion in 2014 using stratified sampling
attach(news_2014)
set.seed(10)
##partitioning data by chanel
channels <- unique(channel)
STR.sample.prop2014 <- NULL
for (i in 1:length(channels)){
  row.indices <- which(channel == channels[i])
  sample.indices <- sample(row.indices, n_h2014_prop[i], replace=FALSE)
  STR.sample.prop2014 <- rbind(STR.sample.prop2014, news_2014[sample.indices,])
}
##get proportion of positive article inside each channel
###get size for positive and negative article under each channel
STR_2014_positive <- STR.sample.prop2014[STR.sample.prop2014$Positive_article == "1",]
STR_2014_negative <- STR.sample.prop2014[STR.sample.prop2014$Positive_article == "0",]
STR_n_Pos_2014<-tapply(STR_2014_positive$shares, STR_2014_positive$channel, length)
STR_n_Neg_2014<-tapply(STR_2014_negative$shares, STR_2014_negative$channel, length)
t_STR_n_2014<-STR_n_Pos_2014+STR_n_Neg_2014

###estimated proportion of positive article in 2014
str_2014_prop <- STR_n_Pos_2014/(t_STR_n_2014)
prop_bar_2014 <- sum(str_2014_prop*(N_h2014/N_2014))
prop_bar_2014
### calculate SE for the estimate
str_2014_prop_bar_var <- (1-n_h2014_prop/N_h2014)*
  str_2014_prop*(1-str_2014_prop)/(t_STR_n_2014)
prop_bar_2014_se <- sqrt(sum(str_2014_prop_bar_var*
                               (N_h2014/N_2014)^2))
prop_bar_2014_se
detach(news_2014)

#Estimate of change in population proportion 
delta_STR_prop <- prop_bar_2014 - prop_bar_2013
delta_STR_prop

#SE of our estimate
delta_STR_prop_SE <- sqrt((prop_bar_2013_se)^2 + (prop_bar_2014_se)^2)
delta_STR_prop_SE

# 95% confidence interval for change of population proportion
str_prop_ci <- data.frame(
  lower_ci = delta_STR_prop - 1.96 * delta_STR_prop_SE,
  upper_ci = delta_STR_prop + 1.96 * delta_STR_prop_SE
)
str_prop_ci

# Since 95% confidence interval excludes 0, we come to the same conclusion that
# there may be a decrease of the proportion of positive articles from
# 2013 to 2014.

