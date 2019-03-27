library(tidyverse)
library(truncnorm)

# number of sessions for webflow A
a <- 10000

# number of sessions for webflow B
b <- 5000

#create data for webflow A
session_id_a <- 100000:(100000 + a)
last_step_a <- ceiling(rtruncnorm(a, a = 0, b = 3, mean = 0, sd = 1.5))

dat_a <- as_tibble(cbind(session_id_a, last_step_a))

dat_a <- dat_a %>%
    mutate(page_num = map(last_step_a, ~ 1:.)) %>%
    unnest(page_num) %>%
    mutate(time_spent = 
               case_when(
                   page_num == 1 ~ rtruncnorm(n(), a = 0, b = 6, mean = 4, sd = 2),
                   page_num == 2 ~ rtruncnorm(n(), a = 0, b = 3, mean = 2, sd = 1),
                   page_num == 3 ~ 0),
           page_name = 
               case_when(
                   page_num == 1 ~ 'order_details',
                   page_num == 2 ~ 'payment',
                   page_num == 3 ~ 'confirmation'),
           webflow_id = 'A',
           test_flag = ifelse(session_id_a <= 100000 + (a/2), 'pre_test', 'test'),
           completed_order_flag = ifelse(page_num == 3, 1, 0)) %>%
    select(test_flag, webflow_id, session_id = session_id_a, page_num, page_name, time_spent, completed_order_flag)

# conv_a <- dat_a %>% summarise(sum(completed_order_flag/n())) %>% pull() 
conv_a <- dat_a %>% summarise(sum(completed_order_flag)/n_distinct(session_id)) %>% pull()       
conv_a

# create data for webflow B
session_id_b <- 200000:(200000 + b)
# last_step_b <- ceiling(rtruncnorm(b, a = 0, b = 5, mean = 7.5, sd = 2))
last_step_b <- ceiling(rtruncnorm(b, a = 0, b = 5, mean = 0, sd = 4.5))

dat_b <- as_tibble(cbind(session_id_b, last_step_b))

dat_b <- dat_b %>%
    mutate(page_num = map(last_step_b, ~ 1:.)) %>%
    unnest(page_num) %>%
    mutate(time_spent = 
               case_when(
                   page_num == 1 ~ rtruncnorm(n(), a = 0, b = 4, mean = 2, sd = 2),
                   page_num == 2 ~ rtruncnorm(n(), a = 0, b = 3, mean = 2, sd = 1),
                   page_num == 3 ~ rtruncnorm(n(), a = 0, b = 3, mean = 2, sd = 1),
                   page_num == 4 ~ rtruncnorm(n(), a = 0, b = 3, mean = 2, sd = 1),
                   page_num == 5 ~ 0),
           page_name = 
               case_when(
                   page_num == 1 ~ 'product_selection',
                   page_num == 2 ~ 'personalization',
                   page_num == 3 ~ 'recipient_details',
                   page_num == 4 ~ 'payment',
                   page_num == 5 ~ 'confirmation'),
           webflow_id = 'B',
           test_flag = 'test',
           completed_order_flag = ifelse(page_num == 5, 1, 0)) %>%
    select(test_flag, webflow_id, session_id = session_id_b, page_num, page_name, time_spent, completed_order_flag)

conv_b <- dat_b %>% summarise(sum(completed_order_flag)/n_distinct(session_id)) %>% pull()
conv_b

dat <- bind_rows(dat_a, dat_b)

# write_csv(dat, 'ab_data.csv')
write_csv(dat, 'ab_data_2.csv')