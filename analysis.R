library(brms)
library(rethinking)
library(dplyr)
library(ggthemes)
library(patchwork)
library(tidyr)
library(extraDistr)

# Description of data
# V, A, D are emotions, feelings, moods, respectively.
# a. EDU is ordinal 6 , and represents participants' educational level
# (None < Some bachelor studies < Bachelor degree < Some master studies < 
# Master degree < Ph. D.)
# b. MAJOR_index är nominal-6, and represents participants' education area 
# (Business Administration, Computer Science, Electrical Engineering, 
# Engineering Physics, None, SoftwareEngineering)
# c. ROLE_index nominal-5, represents participants' work title 
# (Architect, Developer, Manager, Senior Developer, Tester)
# d. EXAMPLE_index nominal-10, represents which scenario A-E
# and, thus, level nivå (high or low technical debt.
# e. LANG_index nominal-10, programming language most experience in Ada, C, C#, 
# C++, Java, JavaScript, Mule, Python, Ruby, or SAP.
# f. ID_index nominal-40. ID of participant
# g. EXP ratio-1-35 professional experience in programming (years, rounded down) 
# h. ENTITIES ratio-2-13. Number of logical abstractions in the example 

d <- read.csv("./data.csv")

dat <- list(
  V = d$V,
  A = d$A,
  D = d$D,
  EDU = factor(d$EDU, levels = c("1","2","3","4","5","6","7"), ordered=TRUE), # ordinal 1-7 int (since it's monotonic)
  EXAMPLE_idx = d$EXAMPLE, # nominal 1-10 factor
  ID_idx = as.integer(d$ID), # subject ID
  MAJOR_idx = d$MAJOR, # nominal 1-6 factor
  ROLE_idx = d$ROLE, # nominal 1-5 factor
  LANG_idx = d$LANG, # nominal 1-10 factor
  EXP = scale(d$EXP), # 1-35 years of experience
  ENTITIES = scale(d$ENTITIES) # 2-13 logical abstractions
)

################################################################################
# Let's look at some priors for our parameters we want to estimate
# cumulative outcome
################################################################################
simplehist(dat$V, xlim=c(1,9), ylim=c(0,50), xlab = "response for V", bty="n")
simplehist(dat$A, xlim=c(1,9), ylim=c(0,50), xlab = "response for A", bty="n")
simplehist(dat$D, xlim=c(1,9), ylim=c(0,50), xlab = "response for D", bty="n")

# discrete proportion of each response value in V, A, and D
pr_k_v <- table(dat$V) / nrow(d)
pr_k_a <- table(dat$A) / nrow(d)
pr_k_d <- table(dat$D) / nrow(d)

# cumsum converts to cumulative proportions 
cum_pr_k_v <- cumsum(pr_k_v)
cum_pr_k_a <- cumsum(pr_k_a)
cum_pr_k_d <- cumsum(pr_k_d)

plot(1:9, cum_pr_k_v, type="b", xlab="response", 
     ylab="cumulative proportion for Emotions (V)", ylim=c(0,1), bty="n", xaxt="n")
axis(1, seq(1,9,1))

plot(1:9, cum_pr_k_a, type="b", xlab="response", 
     ylab="cumulative proportion for Feelings (A)", ylim=c(0,1), bty="n", xaxt="n")
axis(1, seq(1,9,1))

plot(1:9, cum_pr_k_d, type="b", xlab="response", 
     ylab="cumulative proportion for Moods (D)", ylim=c(0,1), bty="n", xaxt="n")
axis(1, seq(1,9,1))

# log-cumulative-odds
lco_v <- logit_scaled(cum_pr_k_v)
lco_a <- logit_scaled(cum_pr_k_a)
lco_d <- logit_scaled(cum_pr_k_d)

plot(1:9, lco_v, type="b", ylim = c(-4,4), xlim = c(1,9), xlab="response for V", 
     ylab="log-cumulative-odds for Emotions (V)", xaxt="n", bty = "n")
axis(1, seq(1,9,1))

plot(1:9, lco_a, type="b", ylim = c(-4,4), xlim = c(1,9), xlab="response for A", 
     ylab="log-cumulative-odds for Feelings (A)", xaxt="n", bty = "n")
axis(1, seq(1,9,1))

plot(1:9, lco_d, type="b", ylim = c(-4,4), xlim = c(1,9), xlab="response for D", 
     ylab="log-cumulative-odds for Moods (D)", xaxt="n", bty = "n")
axis(1, seq(1,9,1))

# So now we have a feeling for the response variable.

################################################################################
# Let's look at the priors now. Later below we'll do prior predictive checks.
################################################################################
# So for each lm in our multivariate model we have the following priors:
# mo(EDU) - Dirichlet prior (we set {2,2,2,2,2,2} which is very weak)
# EXAMPLE_idx - N(0,1) times 10 factors. Means (1*10)^2 in variance
# MAJOR_idx   - N(0,1) times 6 factors.
# ROLE_idx    - N(0,1) times 5 factors.
# LANG_idx    - N(0,1) times 10 factors.
# ENTITIES    - N(0,1) 
# EXP         - N(0,1)
# ID_idx      - varying intercept
#
# We have basically this in our prior sensitivity analysis
N <- 100
SD <- 0.5 # Changed to 0.1 and 1 also and 0.5 is definitely enough.
intercept <- rnorm(N, 0, 1)
Edu <- rdirichlet(n = N, alpha = c(2,2,2,2,2,2))
Example1 <- rnorm(N, 0, SD)
Example2 <- rnorm(N, 0, SD)
Example3 <- rnorm(N, 0, SD)
Example4 <- rnorm(N, 0, SD)
Example5 <- rnorm(N, 0, SD)
Example6 <- rnorm(N, 0, SD)
Example7 <- rnorm(N, 0, SD)
Example8 <- rnorm(N, 0, SD)
Example9 <- rnorm(N, 0, SD)
Example10 <- rnorm(N, 0, SD)
Major1 <- rnorm(N, 0, SD)
Major2 <- rnorm(N, 0, SD)
Major3 <- rnorm(N, 0, SD)
Major4 <- rnorm(N, 0, SD)
Major5 <- rnorm(N, 0, SD)
Major6 <- rnorm(N, 0, SD)
Role1 <- rnorm(N, 0, SD)
Role2 <- rnorm(N, 0, SD)
Role3 <- rnorm(N, 0, SD)
Role4 <- rnorm(N, 0, SD)
Role5 <- rnorm(N, 0, SD)
Lang1 <- rnorm(N, 0, SD)
Lang2 <- rnorm(N, 0, SD)
Lang3 <- rnorm(N, 0, SD)
Lang4 <- rnorm(N, 0, SD)
Lang5 <- rnorm(N, 0, SD)
Lang6 <- rnorm(N, 0, SD)
Lang7 <- rnorm(N, 0, SD)
Lang8 <- rnorm(N, 0, SD)
Lang9 <- rnorm(N, 0, SD)
Lang10 <- rnorm(N, 0, SD)
Entities1 <- rnorm(N, 0, SD)
Entities2 <- rnorm(N, 0, SD)
Entities3 <- rnorm(N, 0, SD)
Entities4 <- rnorm(N, 0, SD)
Entities5 <- rnorm(N, 0, SD)
Entities6 <- rnorm(N, 0, SD)
Entities7 <- rnorm(N, 0, SD)
Exp <- rnorm(N, 0, SD)

plot(NULL, xlim=c(-2,2), ylim=c(0,1))

for(i in 1:N)
  curve(inv_logit_scaled(intercept[i]*x + Edu[i]*x + Example1[i]*x + Example2[i]*x + 
                           Example3[i]*x + Example4[i]*x + Example5[i]*x + 
                           Example6[i]*x +Example7[i]*x + Example8[i]*x + 
                           Example9[i]*x + Example10[i]*x +
                           Major1[i]*x + Major2[i]*x + Major3[i]*x + 
                           Major4[i]*x + Major5[i]*x + Major6[i]*x +
                           Role1[i]*x + Role2[i]*x + Role3[i]*x + Role4[i]*x + 
                           Role5[i]*x + 
                           Lang1[i]*x + Lang2[i]*x + Lang3[i]*x + Lang4[i]*x + 
                           Lang5[i]*x + Lang6[i]*x + Lang7[i]*x + Lang8[i]*x + 
                           Lang9[i]*x + Lang10[i]*x + 
                           Entities1[i]*x + Entities2[i]*x + Entities3[i]*x + 
                           Entities4[i]*x + Entities5[i]*x + Entities6[i]*x + 
                           Entities7[i]*x + Exp[i]*x), 
        add=TRUE)

# Next, what does our Dirichlet prior imply?
x <- seq(0, 1, 0.001)

#(6,6,6,6,6,6,6)
p6 <- data.frame(x, y = dbeta(x, 6, 30)) %>%
  ggplot(aes(x, y)) + geom_smooth(stat = "identity", size=0.25, col = "black") +
  xlab("") + ylab("") + coord_cartesian(ylim=c(0, 7)) +
  theme_tufte() +
  theme(axis.title.y=element_blank(),
       axis.text.y=element_blank(),
       axis.ticks.y=element_blank())

#(5,5,5,5,5,5)
p5 <- data.frame(x, y = dbeta(x, 5, 25)) %>%
  ggplot(aes(x, y)) + geom_smooth(stat = "identity", size=0.25, col = "black") +
  xlab("") + ylab("") + coord_cartesian(ylim=c(0, 7)) +
  theme_tufte()

#(4,4,4,4,4,4)
p4 <- data.frame(x, y = dbeta(x, 4, 20)) %>%
  ggplot(aes(x, y)) + geom_smooth(stat = "identity", size=0.25, col = "black") +
  xlab("") + ylab("") + coord_cartesian(ylim=c(0, 7)) +
  theme_tufte() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#(3,3,3,3,3,3)
p3 <- data.frame(x, y = dbeta(x, 3, 15)) %>%
  ggplot(aes(x, y)) + geom_smooth(stat = "identity", size=0.25, col = "black") +
  xlab(expression(zeta[1])) + ylab("") + coord_cartesian(ylim=c(0, 7)) +
  theme_tufte() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#(2,2,2,2,2,2)
p2 <-  data.frame(x, y = dbeta(x, 2, 10)) %>%
  ggplot(aes(x, y)) + geom_smooth(stat = "identity", size=0.25, col = "black") +
  xlab("") + ylab("") + coord_cartesian(ylim=c(0, 7)) +
  theme_tufte() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#(1,1,1,1,1,1)
p1 <-  data.frame(x, y = dbeta(x, 1, 5)) %>%
  ggplot(aes(x, y)) + geom_smooth(stat = "identity", size=0.25, col = "black") +
  xlab("") + ylab("") + coord_cartesian(ylim=c(0, 7)) +
  theme_tufte() #+
  #theme(axis.title.x=element_blank(),
  #      axis.text.x=element_blank(),
  #      axis.ticks.x=element_blank())

p1 + p3 + p6

# So for each item \zeta in our simplex \alpha_0 the top right picture shows
# that our prior is very broad. Only the top left is broader so to speak.
# Using {2,2,2,2,2,2} is like using a very flat prior. We really don't have any
# good prior knowledge on how this simplex would look like so this is it.

# We can't use projpred since it's cumulative likelihood.
# Let us build models step by step and use LOO in the end for model comparison!
# We model in brms using family=cumulative("logit") and predictor as mo(EDU) 
# since it's a Dirichlet process.
pri <- c(prior(cauchy(0, 1), class=sd, resp=A),
         prior(cauchy(0, 1), class=sd, resp=V),
         prior(cauchy(0, 1), class=sd, resp=D),
         prior(normal(0, 0.5), class=b),
         prior(normal(0, 1), class=Intercept),
         prior(dirichlet(c(2, 2, 2, 2, 2)), 
               class = "simo", coef = "moEDU1", resp="A"),
         prior(dirichlet(c(2, 2, 2, 2, 2)), 
               class = "simo", coef = "moEDU1", resp="D"),
         prior(dirichlet(c(2, 2, 2, 2, 2)), 
               class = "simo", coef = "moEDU1", resp="V"))

# Tested and M is the model to use. Commented out the other models for now.
# The results from running all models can be seen further below.
# m1 <- brm(mvbind(D,A,V) ~ mo(EDU) + (1|ID_idx), 
#          prior = pri, 
#          data=dat, family=cumulative, cores=2, chains=4, iter=2e3,
#          sample_prior = TRUE)
# m1 <- add_criterion(m1, "loo")
# 
# m2 <- brm(mvbind(D,A,V) ~ mo(EDU) + EXAMPLE_idx + (1|ID_idx), 
#          prior = pri, 
#          data=dat, family=cumulative, cores=2, chains=4, iter=2e3,
#          sample_prior = TRUE)
# m2 <- add_criterion(m2, "loo")
# 
# m3 <- brm(mvbind(D,A,V) ~ mo(EDU) + EXAMPLE_idx + MAJOR_idx + (1|ID_idx), 
#          prior = pri, 
#          data=dat, family=cumulative, cores=2, chains=4, iter=5e3,
#          sample_prior = TRUE)
# m3 <- add_criterion(m3, "loo")
# 
# m4 <- brm(mvbind(D,A,V) ~ mo(EDU) + EXAMPLE_idx + MAJOR_idx + ROLE_idx + 
#             (1|ID_idx), 
#          prior = pri, 
#          data=dat, family=cumulative, cores=2, chains=4, iter=2e3,
#          sample_prior = TRUE)
# m4 <- add_criterion(m4, "loo")
# 
# m5 <- brm(mvbind(D,A,V) ~ mo(EDU) + EXAMPLE_idx + MAJOR_idx + ROLE_idx + 
#             LANG_idx + (1|ID_idx), 
#          prior = pri, 
#          data=dat, family=cumulative, cores=2, chains=4, iter=2e3,
#          sample_prior = TRUE)
# m5 <- add_criterion(m5, "loo")
# 
# m6 <- brm(mvbind(D,A,V) ~ mo(EDU) + EXAMPLE_idx + MAJOR_idx + ROLE_idx + 
#             LANG_idx + ENTITIES + (1|ID_idx), 
#          prior = pri, 
#          data=dat, family=cumulative, cores=2, chains=4, iter=2e3,
#          sample_prior = TRUE)
# m6 <- add_criterion(m6, "loo")
# 
M <- brm(mvbind(D,A,V) ~ mo(EDU) + EXAMPLE_idx + MAJOR_idx + ROLE_idx + 
           LANG_idx + ENTITIES + EXP + (1|ID_idx), prior = pri, data=dat, 
         family=cumulative, cores=2, chains=4, iter=5e3, sample_prior = "yes",
         refresh = 0, silent = TRUE, seed = 061215)
#M <- add_criterion(M, "loo")

# Sample the same as above but only using the priors, no data using 
# sample_prior = "only"
M_prio <- brm(mvbind(D,A,V) ~ mo(EDU) + EXAMPLE_idx + MAJOR_idx + ROLE_idx + 
           LANG_idx + ENTITIES + EXP + (1|ID_idx), prior = pri, data=dat, 
         family=cumulative, cores=2, chains=4, iter=5e3, sample_prior = "only",
         refresh = 0, silent = TRUE, seed = 061215, 
         control = list(adapt_delta=0.95))


#loo_compare(m1, m2, m3, m4, m5, m6, M)
#    elpd_diff se_diff
# m5  0.0       0.0   
# m2 -1.2       4.0   
# m1 -1.4       5.2   
# m6 -2.0       1.4   
# m3 -2.0       4.0   
# m4 -2.3       3.7   
# M  -3.1       2.3
# In short, there's not even a difference in out of sample prediction between 
# #1 and #7 model. We can thus use the more complex models to answer
# more questions that we have (related to the predictors we include).
# -3.1 + c(-1,1)*2.3*1.96
# [1] -7.608  1.408
# So M is our model for now

summary(M) # Check ESS and \widehat{R}
# Some things to look at perhaps (95% uncertainty interval):
# D_EXAMPLE_idxBL       -1.43    -0.10
# V_EXAMPLE_idxBH        0.04     1.42
# V_EXAMPLE_idxDL       -1.53    -0.15
# V_EXP                 -0.05     0.56

# But the above is really not that important - it's more important to look at
# marginal effects (later down below), combined with looking at contrasts from
# posterior_predict()

# Check model fit
library(bayesplot)
p1 <- pp_check(M, type = "bars", resp = "V", nsamples = 50) + theme_tufte() + 
  theme(legend.position = "none", panel.grid.major.y = element_line(), 
        axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from=0,to=50,by=10))
p2 <- pp_check(M, type = "bars", resp = "A", nsamples = 50) + theme_tufte() + 
  theme(legend.position = "none", panel.grid.major.y = element_line(),
        axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  scale_y_continuous(breaks = seq(from=0,to=50,by=10))
p3 <- pp_check(M, type = "bars", resp = "D", nsamples = 50) + theme_tufte() + 
  theme(legend.position = "none", panel.grid.major.y = element_line(),
        axis.title.y = element_blank()) + 
  scale_y_continuous(breaks = seq(from=0, to=50, by=10)) +
  scale_x_continuous(breaks = seq(from=1, to=9))

p1 / p2 / p3

plot(M) # Check chains etc. We're estimating 112 params so it's many plots...

# Let's look at the cumulative probabilities for A, V, and D intercepts
posterior_samples(M) %>% 
  select(starts_with("b_A_Intercept"), 
         starts_with("b_D_Intercept"), 
         starts_with("b_V_Intercept")) %>% 
  mutate_all(inv_logit_scaled) %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(mean = mean(value),
            sd   = sd(value),
            ll   = quantile(value, probs = .025),
            ul   = quantile(value, probs = .975))
################################################################################
# Posterior predictive checks
################################################################################

post <- posterior_samples(M)

# predictive distribution - check sanity.
marginal_effects(M_prio)
# Looks sane

# Our posterior distribution
marginal_effects(M)
# Some interesting aspects
# D, V, A vs. ENTITIES -> When entities increase so does D, A, and V
# D and A goes down with experience, while V is the opposite
# EDU has very little effect
# I think using the marginal_effects() is a good thing here since it is a bit
# intuitive.

################################################################################
# Check how much posterior has moved from priors for the sd of A, D and V in the 
# multivariate model M. Below we generate our priors directly from dhcauchy()
################################################################################
# A
p1 <- tibble(x = seq(from = 0, to = 10, by = .5)) %>% 
  ggplot() +
  # the prior
  geom_ribbon(aes(x = x, ymin = 0, ymax = dhcauchy(x, sigma = 1)),
              fill = "black", alpha = 1/10) +
  # the posterior
  geom_density(data = post,
               aes(x = sd_ID_idx__A_Intercept), 
               fill = "black", alpha = 1/5, size = 0) +
  xlab(expression(sigma[A])) +
  theme_tufte()

# D
p2 <- tibble(x = seq(from = 0, to = 10, by = .5)) %>% 
  ggplot() +
  # the prior
  geom_ribbon(aes(x = x, ymin = 0, ymax = dhcauchy(x, sigma = 1)),
              fill = "black", alpha = 1/10) +
  # the posterior
  geom_density(data = post,
               aes(x = sd_ID_idx__D_Intercept), 
               fill = "black", alpha = 1/5, size = 0) +
  xlab(expression(sigma[D])) +
  theme_tufte()

# V
p3 <- tibble(x = seq(from = 0, to = 10, by = .5)) %>% 
  ggplot() +
  # the prior
  geom_ribbon(aes(x = x, ymin = 0, ymax = dhcauchy(x, sigma = 1)),
              fill = "black", alpha = 1/10) +
  # the posterior
  geom_density(data = post,
               aes(x = sd_ID_idx__V_Intercept), 
               fill = "black", alpha = 1/5, size = 0) +
  xlab(expression(sigma[V])) +
  theme_tufte()

p1 + p2 + p3

################################################################################
# Hypothesis testing
################################################################################
# Using Bayes Factor here for hypothesis testing.
# We compare Evid.Ratio when running hypothesis testing with the below values:
# ** >10 Strong evidence for H1
# * 3-10 Moderate evidence for H1
# ? 1-3 Anecdotal evidence for H1
# ? 1/3-1 Anecdotal evidence for H0
# * 1/30 – 1/10 Moderate evidence for H0
# ** <1/10 Strong evidence for H0

# This is what we're interested in mainly for now:
# D_EXAMPLE_idxBL       -1.44    -0.11
# V_EXAMPLE_idxBH        0.06     1.41
# V_EXAMPLE_idxDL       -1.52    -0.15
# V_EXP                 -0.06     0.56

# for the EXAMPLE parameters we'll check high against low, while for EXP we'll
# check against 0

hypothesis(M, "D_EXAMPLE_idxBL < D_EXAMPLE_idxBH", class = "b")
# Moderate evidence for H1, i.e., BH is larger perhaps
plot(hypothesis(M, "D_EXAMPLE_idxBL < D_EXAMPLE_idxBH", class = "b"))

hypothesis(M, "V_EXAMPLE_idxBL < V_EXAMPLE_idxBH", class = "b")
# Anecdotal evidence for H1, i.e., BH is larger perhaps
plot(hypothesis(M, "V_EXAMPLE_idxBL < V_EXAMPLE_idxBH", class = "b"))

hypothesis(M, "V_EXAMPLE_idxDL < V_EXAMPLE_idxDH", class = "b")
# Strong evidence for H1, i.e., DH probably larger
plot(hypothesis(M, "V_EXAMPLE_idxDL < V_EXAMPLE_idxDH", class = "b"))

hypothesis(M, "V_EXP > 0", class = "b")
# Strong evidence for H1, i.e., V_EXP probably positive
plot(hypothesis(M, "V_EXP > 0", class = "b"))

################################################################################
# 
# We could also fix some values and look at mean response rates.
#
################################################################################
# Remember, our model had six fixed effects: 
# EXAMPLE_idx, MAJOR_idx, ROLE_idx, LANG_idx, ENTITIES, and EXP
# Let's use a function to create fitted() response values for A, V, 
# and D (where each consist of a Likert 1-9).

avg_resp <- function(resp, example){
  
  if (resp != "A" & resp != "D" & resp != "V")
    stop("Only outcomes A, V or D are supported.")  
  
  nd <-
    tibble(EXAMPLE_idx = example,
           MAJOR_idx   = c("SE"), # software engineer (largest group, n=90)
           EDU = 3, # Bachelor (largest group, n=85)
           ROLE_idx = c("D"), # developer (largest group, n=125)
           LANG_idx = c("CS"), # C# (largest group, n=100)
           ENTITIES = 0, # we've scaled this already
           EXP = 0 # same here
    )
  
  max_iter <- 100
  resp <- resp
  
  fitted(M, 
         newdata = nd, 
         subset  = 1:max_iter,
         re_formula = NA,
         resp = resp,
         summary = FALSE) %>% 
    as_tibble() %>%
    gather() %>%
    mutate(iter = rep(1:max_iter, times = 18)) %>%
    select(iter, everything()) %>% 
    separate(key, into = c("intention", "rating")) %>% 
    mutate(intention = intention %>% as.double(),
           rating    =  rating %>% as.double()) %>%
    mutate(intention = intention -1) %>%
    rename(pk = value) %>% 
    mutate(`pk:rating` = pk * rating) %>% 
    group_by(iter, intention) %>% 
    arrange(iter, intention, rating) %>% 
    mutate(probability = cumsum(pk)) %>% 
    filter(rating < 9) %>%
    summarise(mean_rating = sum(`pk:rating`))
}

# remember:
# D, V, A vs. ENTITIES -> When entities increase so does D, A, and V
# D and A goes down with EXP, while V is the opposite
# EDU has very little effect. 
#
# Now let's use our function above to plot the *average* response for the 
# combinations above that had strong evidence, i.e., 
# As before, some things to look at perhaps (95% uncertainty interval):
# D_EXAMPLE_idxBL       -1.44    -0.11
# V_EXAMPLE_idxBH        0.06     1.41
# V_EXAMPLE_idxDL       -1.52    -0.15
# V_EXP                 -0.06     0.56

# average in our case is the largest category or median in each predictor
# (can be changed in the function above)
# major = SE n=90
# edu = bachelor n=85
# role = developer n=125
# lang = C# n=100
# entities = 5 (median)
# exp = 8 (median)

# In short, this is just another view we can take on this. After this we'll look
# at raw effects sizes

example = c("BL", "BH")
p1 <- avg_resp(resp = "D",
               example  = example) %>%
  ggplot(aes(x = intention, y = mean_rating, group = iter)) +
  geom_line(alpha = 1 / 10, color = "black") +
  scale_x_continuous("outcome D", breaks = 0:1, labels = c(example[1], example[2])) +
  scale_y_continuous("", limits=c(3,6)) +
  theme_hc() +
  theme(
  #   legend.position = "none",
  plot.subtitle = element_text(size = 10),
  axis.title = element_text(size = 9)
  ) 

example = c("BL", "BH")
p2 <- avg_resp(resp = "V",
               example  = example) %>%
  ggplot(aes(x = intention, y = mean_rating, group = iter)) +
  geom_line(alpha = 1 / 10, color = "black") +
  scale_x_continuous("outcome V", breaks = 0:1, labels = c(example[1], example[2])) +
  scale_y_continuous("", limits=c(3,6)) +
  #coord_cartesian(ylim = 2:6) +
  theme_hc() +
  theme(
    #legend.position = "none",
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

example = c("DL", "DH")
p3 <- avg_resp(resp = "V",
               example  = example) %>%
  ggplot(aes(x = intention, y = mean_rating, group = iter)) +
  geom_line(alpha = 1 / 10, color = "black") +
  scale_x_continuous("outcome V", breaks = 0:1, labels = c(example[1], example[2])) +
  scale_y_continuous("", limits=c(3,6)) +
  #coord_cartesian(ylim = 2:6) +
  theme_hc() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
  #   legend.position = "none",
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 9)
  ) 

p1+p2+p3
# grid.arrange(p1, p2, p3, ncol = 3)

################################################################################
# Effect sizes 
################################################################################
#
# https://discourse.mc-stan.org/t/contrasts-in-brms/7824
# The general answer to all these questions is to compare the posterior 
# predictions of the model at different levels of the factor. 
# For example, let's look at some of our more interesting effects.
# D_EXAMPLE_idxBL       -1.44    -0.11
# V_EXAMPLE_idxBH        0.06     1.41
# V_EXAMPLE_idxDL       -1.52    -0.15
# V_EXP                 -0.05     0.56

# posterior_predict bails when using a list() so change to df
df <- as.data.frame(dat)
df$EXAMPLE_idx <- as.factor(df$EXAMPLE_idx)
df$EDU <- droplevels(df$EDU)

# For level BH and BL check difference between them
data_1 <- df[df$EXAMPLE_idx == levels(df$EXAMPLE_idx)[4], ] # BL
PPD_1 <- posterior_predict(M, newdata = data_1)
data_2 <- data_1
data_2$EXAMPLE_idx[1:nrow(data_2)]  <- levels(df$EXAMPLE_idx)[3] # BH
PPD_2 <- posterior_predict(M, newdata = data_2)
PPD_diff <- PPD_2 - PPD_1 # Difference(!) between two levels

# remember, we had three outcomes D, A, and V, so store D & V separately
outcome_d <- PPD_diff[,,1] # D
outcome_v <- PPD_diff[,,3] # V

# https://discourse.mc-stan.org/t/posterior-linpred-with-ordinal-models/2260/3
summary(rowMeans(outcome_d))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.8421 -0.1579  0.4211  0.4389  1.0526  3.7895
# so BH is 0.4 larger in median compared to BL

summary(rowMeans(outcome_v))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.4211 -0.2632  0.2632  0.2775  0.8421  3.1053
# so BH is ~0.3 larger in median compare to BL

# and now V and DL vs. DH
data_1 <- df[df$EXAMPLE_idx == levels(df$EXAMPLE_idx)[8], ] # DL
PPD_1 <- posterior_predict(M, newdata = data_1)
data_2 <- data_1
data_2$EXAMPLE_idx[1:nrow(data_2)]  <- levels(df$EXAMPLE_idx)[7] # DH
PPD_2 <- posterior_predict(M, newdata = data_2)
PPD_diff <- PPD_2 - PPD_1 # Difference(!) between two levels

# remember, we had three outcomes D, A, and V, so store V separately
outcome_v <- PPD_diff[,,3] # V
summary(rowMeans(outcome_v))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.150   0.450   1.000   1.011   1.550   4.250
# so DH is 1 larger than DL. But as is evident from Qu. we have uncertainty...

# finally, let's have a look at V_EXP
foo <- table(post$b_V_EXP>0)
foo[2]/10000
#   TRUE 
# 0.9451
# So, in ~95% of the cases V_EXP is above 0

################################################################################
# Compare EXP with H and L scenarios in categories (V, A, D) that are sign
################################################################################
# Need to create a function where we set EXP to low and high (5 yrs and 30 yrs?) 
# Finally, a scenario where we test EXP low and high, and step through 
# LANG for each scenario where the contrast is larger.

# We'll do this several times so might as well create a function.
# As input we give one level 1-10 and a year:
# > levels(df$EXAMPLE_idx)
# [1] "AH" "AL" "BH" "BL" "CH" "CL" "DH" "DL" "EH" "EL"
#
# We'll just use the globally defined df and d for now.
comp_exp <- function(level, yrs){
  
  if (level > 10 | level < 1 | yrs < 0)
    stop("Set correct level and/or use only Z^+ for years.")    

  # pick out all rows where EXAMPLE_idx = level
  # > levels(df$EXAMPLE_idx)
  # [1] "AH" "AL" "BH" "BL" "CH" "CL" "DH" "DL" "EH" "EL"
  dat1yrs <- df[df$EXAMPLE_idx == levels(df$EXAMPLE_idx)[level], ]
  
  # Change EXP to yrs (check mean and sd on original dataset, since we 
  # standardized)
  dat1yrs$EXP[1:nrow(dat1yrs)] <- (yrs-mean(d$EXP))/sd(d$EXP)
  
  # predict responses based on the fitted model
  posterior_predict(M, newdata = dat1yrs)
}

################################################################################
# Let's start with comparing EXP = 5 and EXP = 30 for:
# D_EXAMPLE_idxBH vs BL
# V_EXAMPLE_idxBH vs BL
#
# First, level BH (3) and BL (4) and 5 vs. 30 years (for D, A, and V)
################################################################################

yrs1 <- 5
yrs2 <- 30

# BH
yrs_low <- comp_exp(3, yrs1)
yrs_high <- comp_exp(3,yrs2)

# what's the difference?
PPD_diff <- yrs_high - yrs_low

# D Below implies that yrs_low is larger by ~0.3
summary(rowMeans(PPD_diff[,,1]))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3.4762 -0.9524 -0.3333 -0.3292  0.2857  2.8571 

# A yrs_low is ~0.6 larger
summary(rowMeans(PPD_diff[,,2]))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3.5238 -1.0952 -0.5714 -0.5559  0.0000  2.9524 

# V yrs_high is ~0.5 larger
summary(rowMeans(PPD_diff[,,3]))
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -2.14286  0.04762  0.52381  0.52470  1.00000  3.09524

# BL
yrs_low <- comp_exp(4, 5)
yrs_high <- comp_exp(4, 30)

PPD_diff <- yrs_high - yrs_low
# D
summary(rowMeans(PPD_diff[,,1]))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3.6316 -0.8947 -0.3158 -0.2887  0.3158  3.3158

# A
summary(rowMeans(PPD_diff[,,2]))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -3.6316 -1.1579 -0.5789 -0.5604  0.0000  3.1053

# V
summary(rowMeans(PPD_diff[,,3]))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.3158  0.3684  0.8947  0.8949  1.4737  4.2632

# In short, 
# * Going from BH to BL (for D) we see that the difference between yrs_high and 
# yrs_low is virtually the same (-0.33 \approx -0.31)
# * Going from BH to BL (for A) we see that the difference between high and low 
# is virtually the same (-0.57 \approx -0.58)
# * Going from BH to BL (for V) we see that the diff between H and L increases
# (0.89 > 0.52), i.e., in BL there's a larger difference between the mean, comp
# to BH where the difference is 0.52 between 5 and 30 years of experience.

################################################################################
# Level DH (7) and DL (8) and 5 vs. 30 years (for V)
# V_EXAMPLE_idxDH vs DL
################################################################################

# DH
yrs_low <- comp_exp(7, yrs1)
yrs_high <- comp_exp(7,yrs2)

# what's the difference?
PPD_diff <- yrs_high - yrs_low

# V yrs_high is 0.9 larger
summary(rowMeans(PPD_diff[,,3]))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.2500  0.3500  0.9250  0.9154  1.5000  4.2000

# DL
yrs_low <- comp_exp(8, yrs1)
yrs_high <- comp_exp(8, yrs2)

PPD_diff <- yrs_high - yrs_low
# V
summary(rowMeans(PPD_diff[,,3]))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.4500  0.3500  0.9000  0.8975  1.4500  4.0000

# In short, there's not much to talk about in difference (in V) going from DH 
# to DL when looking at years.

