## ---- odds-ratio-adjusted-axis ----
# Plot the exponentiated coefficients (% change in odds ratio).
coefs.df %>%
  filter(parameter != "(Intercept)") %>%
  mutate(pretty.parameter = fct_reorder(pretty.parameter, est),
         lower.95 = est + (qnorm(0.025) * se),
         lower.50 = est + (qnorm(0.25) * se),
         upper.50 = est + (qnorm(0.75) * se),
         upper.95 = est + (qnorm(0.975) * se),
         signif = case_when(p > 0.05 ~ "Not significant",
                            est > 0 ~ "Positive",
                            est < 0 ~ "Negative"),
         signif = fct_relevel(signif, "Positive", "Not significant",
                              "Negative")) %>%
  mutate(across(matches("est|lower|upper"), ~ exp(.))) %>%
  ggplot(aes(x = pretty.parameter, color = signif)) +
  geom_linerange(aes(ymin = lower.95, ymax = upper.95), size = 1) +
  geom_linerange(aes(ymin = lower.50, ymax = upper.50), size = 2) +
  geom_point(aes(y = est), size = 3) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual("Relationship to\nodds ratio of passing",
                     values = c(good.color, neutral.color, bad.color)) +
  labs(x = "", y = "% change in odds ratio",
       title = "Estimated relationships between student characteristics and odds ratio of passing") +
  coord_flip() +
  theme_bw()
