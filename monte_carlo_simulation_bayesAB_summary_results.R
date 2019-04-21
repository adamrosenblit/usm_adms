# uses data objects from bayesian_ab_test.R, so make sure to run that code first

n.trials <- 1e5
prior.alpha <- alpha
prior.beta <- beta
a.samples <- rbeta(n.trials,
                   test_A_stats$conversions+prior.alpha,
                   (test_A_stats$sample_size-test_A_stats$conversions)+prior.beta)
b.samples <- rbeta(n.trials,
                   720+prior.alpha,
                   (5001-720)+prior.beta)

# P(A > B)
p.a_superior <- sum(a.samples > b.samples)/n.trials
p.a_superior

# Credible Interval on (A - B) / B 
credible_interval <- quantile((a.samples-b.samples)/b.samples, c(.05,.95))
credible_interval

# A <- session_level_test_webflow_A$is_order
# B <- session_level_test_webflow_B$is_order
# credible_interval_point_estimate = ((sum(A)/length(A))-(sum(B)/length(B)))/(sum(B)/length(B))
# credible_interval_point_estimate 

# Posterior Expected Loss for choosing B over A
BoverA <- b.samples > a.samples
loss <- (b.samples-a.samples)/a.samples
PEL <- mean(BoverA) * mean(loss[BoverA])
PEL


