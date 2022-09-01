## ---- change-in-log-odds ----

log.odds.p = coefs.df %>%
  filter(parameter != "(Intercept)") %>%
  mutate(pretty.parameter = fct_reorder(pretty.parameter, est),
         signif = case_when(p > 0.05 ~ "Not significant",
                            est > 0 ~ "Positive",
                            est < 0 ~ "Negative"),
         signif = fct_relevel(signif, "Positive", "Not significant",
                              "Negative")) %>%
  ggplot(aes(x = pretty.parameter, color = signif)) +
  geom_linerange(aes(ymin = est + (qnorm(0.025) * se),
                     ymax = est + (qnorm(0.975) * se)),
                 size = 1) +
  geom_linerange(aes(ymin = est + (qnorm(0.25) * se),
                     ymax = est + (qnorm(0.75) * se)),
                 size = 2) +
  geom_point(aes(y = est), size = 3) +
  geom_hline(yintercept = 0) +
  scale_color_manual("Relationship to\nlog odds of passing",
                     values = c(good.color, neutral.color, bad.color)) +
  labs(x = "", y = "Change in log odds",
       title = "Estimated relationships between\nstudent characteristics\nand log odds of passing") +
  coord_flip(clip = "off")

# ---- change-in-log-odds-adjusted-axis ----

secret.log.odds.p = coefs.df %>%
  filter(parameter != "(Intercept)") %>%
  mutate(pretty.parameter = fct_reorder(pretty.parameter, est),
         signif = case_when(p > 0.05 ~ "Not significant",
                            est > 0 ~ "Positive",
                            est < 0 ~ "Negative"),
         signif = fct_relevel(signif, "Positive", "Not significant", "Negative")) %>%
  ggplot(aes(x = pretty.parameter, color = signif)) +
  geom_linerange(aes(ymin = est + (qnorm(0.025) * se), ymax = est + (qnorm(0.975) * se)), size = 1) +
  geom_linerange(aes(ymin = est + (qnorm(0.25) * se), ymax = est + (qnorm(0.75) * se)), size = 2) +
  geom_point(aes(y = est), size = 3) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    breaks = c(-1, 0, 1), #<<
    labels = c("← Lower", #<<
               "Same", #<<
               "Higher →") #<<
  ) +
  scale_color_manual("Relationship to\nchance of passing",
                     values = c(good.color, neutral.color, bad.color)) +
  labs(x = "", y = "Chance of passing",
       title = "Estimated relationships between\nstudent characteristics\nand chance of passing") +
  coord_flip()
