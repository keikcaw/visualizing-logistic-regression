## ---- probability-relative-to-some-baseline-no-arrows ----
# Plot probability of passing relative to "baseline" probability (with
# confidence intervals).  Note that these probability estimates ignore the
# uncertainty in the estimate of the intercept.
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
  mutate(across(matches("est|lower|upper"),
                ~ invlogit(. + coefs.df$est[coefs.df$parameter == "(Intercept)"]))) %>%
  ggplot(aes(x = pretty.parameter, color = signif)) +
  geom_linerange(aes(ymin = lower.95, ymax = upper.95), size = 1) +
  geom_linerange(aes(ymin = lower.50, ymax = upper.50), size = 2) +
  geom_point(aes(y = est), size = 3) +
  geom_hline(yintercept = invlogit(coefs.df$est[coefs.df$parameter == "(Intercept)"])) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  scale_color_manual("Relationship to\nprobability of passing",
                     values = c(good.color, neutral.color, bad.color)) +
  labs(x = "", y = "Probability of passing",
       title = "Estimated relationships between student characteristics and probability of passing") +
  coord_flip() +
  theme_bw()

## ---- probability-relative-to-some-baseline-with-arrows ----
# Plot probability of passing relative to "baseline" probability (with arrows).
coefs.df %>%
  filter(parameter != "(Intercept)") %>%
  mutate(pretty.parameter = fct_reorder(pretty.parameter, est),
         signif = case_when(p > 0.05 ~ "Not significant",
                            est > 0 ~ "Positive",
                            est < 0 ~ "Negative"),
         signif = fct_relevel(signif, "Positive", "Not significant",
                              "Negative"),
         est = invlogit(est + coefs.df$est[coefs.df$parameter == "(Intercept)"])) %>%
  ggplot(aes(x = invlogit(coefs.df$est[coefs.df$parameter == "(Intercept)"]),
             xend = est,
             y = pretty.parameter, yend = pretty.parameter,
             color = signif)) +
  geom_segment(size = 1,
               arrow = arrow(length = unit(0.1, "in"), type = "closed")) +
  geom_vline(xintercept = invlogit(coefs.df$est[coefs.df$parameter == "(Intercept)"])) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  scale_color_manual("Relationship to\nprobability of passing",
                     values = c(good.color, neutral.color, bad.color)) +
  labs(x = "Probability of passing", y = "",
       title = "Estimated relationships between student characteristics and probability of passing") +
  theme_bw()
