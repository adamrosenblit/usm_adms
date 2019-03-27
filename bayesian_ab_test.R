# Bayesian AB Testing references
# https://cran.r-project.org/web/packages/bayesAB/bayesAB.pdf
# https://cran.r-project.org/web/packages/bayesAB/vignettes/introduction.html
# https://github.com/FrankPortman/bayesAB

# prior probabilities
# https://en.wikipedia.org/wiki/Prior_probability

# good explanation of Bernoulli as special case of Binomial distribution
# https://math.stackexchange.com/questions/838107/what-is-the-difference-and-relationship-between-the-binomial-and-bernoulli-distr

# another neat article on bayesian methods for testing hypothesis
# http://varianceexplained.org/r/bayesian_ab_baseball/

# Some readings on selecting Beta params, alpha and beta
# https://stats.stackexchange.com/questions/47771/what-is-the-intuition-behind-beta-distribution
# https://www.countbayesie.com/blog/2015/2/18/hans-solo-and-bayesian-priors
# https://www.countbayesie.com/blog/2015/4/25/bayesian-ab-testing

library(tidyverse)
library(bayesAB)

data <- read.csv('ab_data_2.csv')
View(data)

# pre-test flow A
pretest_webflow_A <- data %>% filter(test_flag=='pre_test' & webflow_id=='A')
# View(pretest_webflow_A)

# test flow A
test_webflow_A <- data %>% filter(test_flag=='test' & webflow_id=='A')
# View(test_webflow_A)

# test flow B 
test_webflow_B <- data %>% filter(test_flag=='test' & webflow_id=='B')
# View(test_webflow_B)

# reduce the pre-test flow A data to the session level
session_level_pretest_webflow_A <- pretest_webflow_A %>% 
    group_by(session_id) %>%
    summarize(is_order = max(completed_order_flag))    
# View(session_level_pretest_webflow_A)

# reduce the test flow A data to the session level
session_level_test_webflow_A <- test_webflow_A %>% 
    group_by(session_id) %>%
    summarize(is_order = max(completed_order_flag))    
# View(session_level_test_webflow_A)

# reduce the test flow B data to the session level
session_level_test_webflow_B <- test_webflow_B %>% 
    group_by(session_id) %>%
    summarize(is_order = max(completed_order_flag))    
# View(session_level_test_webflow_B)

# function to obtain some stats to be used in the hypothesis test
get_stats <- function(x) {
    conversions <-sum(x)
    sample_size <- length(x)
    conversion_rate <- round(conversions/sample_size, 4)
    variance <- conversion_rate*(1-conversion_rate)/sample_size
    lower_95 <- round(conversion_rate - 1.96*sqrt(variance), 4)
    upper_95 <- round(conversion_rate + 1.96*sqrt(variance), 4)
    my_stats <- list(conversions = conversions,
                     sample_size = sample_size,
                     conversion_rate = conversion_rate, 
                     variance = variance,
                     lower_95 = lower_95,
                     upper_95 = upper_95)
    return(my_stats)
}

pretest_A_stats <- get_stats(x=session_level_pretest_webflow_A$is_order)
pretest_A_stats
test_A_stats <- get_stats(x=session_level_test_webflow_A$is_order)
test_A_stats
test_B_stats <- get_stats(x=session_level_test_webflow_B$is_order)
test_B_stats

# hypothesis test 1... H0: pretest A conversion rate = test A conversion rate 
# p > 0.05, so we fail to reject the null hypothesis 
prop.test(x = c(pretest_A_stats$conversions, test_A_stats$conversions), 
          n = c(pretest_A_stats$sample_size, test_A_stats$sample_size),
          conf.level = 0.95,
          alternative = 'two.sided')

# hypothesis test 1... H0: test A conversion rate = test B conversion rate 
# p < 0.05, so we reject the null hypothesis 
prop.test(x = c(test_A_stats$conversions, test_B_stats$conversions), 
          n = c(test_A_stats$sample_size, test_B_stats$sample_size),
          conf.level = 0.95,
          alternative = 'two.sided')


# Instead of relying on a traditional hypothesis test...
#
# If we think about is_order as a vector of Bernoulli RV's, then we can use the beta distribution as the conjugate prior of p.  
#
# For clarity, we can imagine is_order as having been generated as follows,
#   is_order <- rbinom(n,1,p) 
# which implies that each,
#   is_order[i] ~ Bernoulli(p), 0 <= i <= n
#
# Scroll down to Details > Bernoulli
?bayesTest


# So, we need to find the (hyper)params that appropriately model p ~ Beta(alpha, beta).  To do so, we will 
# use our historical knowledge about p from the pretest A data. Specifically, we calculated the sample conversion 
# rate, aka the sample proportion p_hat, which estimates p, the population proportion, or true conversion probability.  
# 
#   p_hat = conversions / (conversions + drop_offs)
#         = 727 / (727 + 4274)
#         = 0.1454 
#
#   95% CI: (0.1356, 0.1552)
#
# We also know the mean of the Beta distribution is given by,
#
#   E(p) = alpha / (alpha + beta)
#
# which looks very much like our defintion of p_hat.  So, we want p ~ Beta(alpha=727, beta=4274).
#
# We'll add the point estimate p_hat as well as the bounds of 95% CI for reference.

alpha <- pretest_A_stats$conversions
beta <- pretest_A_stats$sample_size - pretest_A_stats$conversions

plot_beta <- plotBeta(alpha=alpha, beta=beta)
plot_beta + 
    geom_vline(xintercept=pretest_A_stats$lower_95, linetype='dashed', color = 'darkblue') +
    geom_vline(xintercept=pretest_A_stats$upper_95, linetype='dashed', color = 'darkblue') +
    geom_vline(xintercept=pretest_A_stats$conversion_rate, linetype='solid', color = 'darkred') +
    xlim(0.1, 0.2) 

# This is our AB Test object    
abTest_1 <- bayesTest(session_level_test_webflow_A$is_order, 
                      session_level_test_webflow_B$is_order, 
                      priors = c('alpha' = alpha, 'beta' = beta), 
                      n_samples = 1e5, 
                      distribution = 'bernoulli')
# basic info about our test
print(abTest_1)
# useful test results
summary(abTest_1)
# and vizualizations
plot(abTest_1)

# I'll be adding some more in-depth notes on interpretting these results.  I think that the conversion rate of webflow B should be something more realstic, 
# ie, closer to that of webflow A.  I feel like with such a huge difference in rates, and given the sample size is large, the observed is certainly significant.
# 
# I also may want to investigate modeling page interactions using a Poisson Distribution (Gamma prior), which will require adding 2 new attributes, 
#   - num_interactions
#   - total_possible_interactions
# where the former tells us how many interactions the user made on that page, and the latter is the total number of possible interactions they could have had.



