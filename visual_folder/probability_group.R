## ---- probability-relative-to-some-baseline-and-group-no-arrows ----
# Plot probability of passing relative to "baseline" probability for several
# groups (with confidence intervals).  Note that these probability estimates
# ignore the uncertainty in the estimates of the intercept and the group
# parameters.
expand.grid(pet = c("None", "Dog", "Cat", "Fish"),
            other.parameter = coefs.df %>%
              filter(!grepl("pet\\.type|Intercept", parameter)) %>%
              pull(parameter)) %>%
  mutate(pet.parameter = paste("pet.type", str_to_lower(pet), sep = "")) %>%
  left_join(coefs.df, by = c("pet.parameter" = "parameter")) %>%
  mutate(pretty.parameter = coalesce(pretty.parameter, "Pet: None"),
         mu = coefs.df$est[coefs.df$parameter == "(Intercept)"] +
           coalesce(est, 0),
         baseline.mu = mu) %>%
  dplyr::select(pet, other.parameter, mu, baseline.mu) %>%
  left_join(coefs.df, by = c("other.parameter" = "parameter")) %>%
  mutate(pretty.parameter = fct_reorder(pretty.parameter, est),
         mu = mu + est,
         lower.95 = mu + (qnorm(0.025) * se),
         lower.50 = mu + (qnorm(0.25) * se),
         upper.50 = mu + (qnorm(0.75) * se),
         upper.95 = mu + (qnorm(0.975) * se),
         signif = case_when(p > 0.05 ~ "Not significant",
                            est > 0 ~ "Positive",
                            est < 0 ~ "Negative"),
         signif = fct_relevel(signif, "Positive", "Not significant",
                              "Negative")) %>%
  mutate(across(matches("mu|lower|upper"), ~ invlogit(.))) %>%
  ggplot(aes(x = pretty.parameter, color = signif)) +
  geom_linerange(aes(ymin = lower.95, ymax = upper.95), size = 1) +
  geom_linerange(aes(ymin = lower.50, ymax = upper.50), size = 2) +
  geom_point(aes(y = mu), size = 3) +
  geom_hline(aes(yintercept = baseline.mu)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  scale_color_manual("Relationship to\nprobability of passing",
                     values = c("chartreuse3", "gray", "brown3")) +
  facet_wrap(~ pet) +
  labs(x = "", y = "Probability of passing",
       title = "Estimated relationships between student characteristics and probability of passing",
       subtitle = "By type of pet") +
  coord_flip() +
  theme_bw() +
  theme(legend.pos = "bottom")

## ---- probability-relative-to-some-baseline-and-group-with-arrows ----
# Plot probability of passing relative to "baseline" probability for several
# groups (with arrows).
expand.grid(pet = c("None", "Dog", "Cat", "Fish"),
            other.parameter = coefs.df %>%
              filter(!grepl("pet\\.type|Intercept", parameter)) %>%
              pull(parameter)) %>%
  mutate(pet.parameter = paste("pet.type", str_to_lower(pet), sep = "")) %>%
  left_join(coefs.df, by = c("pet.parameter" = "parameter")) %>%
  mutate(pretty.parameter = coalesce(pretty.parameter, "Pet: None"),
         mu = coefs.df$est[coefs.df$parameter == "(Intercept)"] +
           coalesce(est, 0),
         baseline.mu = mu) %>%
  dplyr::select(pet, other.parameter, mu, baseline.mu) %>%
  left_join(coefs.df, by = c("other.parameter" = "parameter")) %>%
  mutate(pretty.parameter = fct_reorder(pretty.parameter, est),
         mu = mu + est,
         signif = case_when(p > 0.05 ~ "Not significant",
                            est > 0 ~ "Positive",
                            est < 0 ~ "Negative"),
         signif = fct_relevel(signif, "Positive", "Not significant",
                              "Negative")) %>%
  mutate(across(matches("mu"), ~ invlogit(.))) %>%
  ggplot(aes(x = baseline.mu, xend = mu, y = pretty.parameter,
             yend = pretty.parameter, color = signif)) +
  geom_segment(size = 1,
               arrow = arrow(length = unit(0.1, "in"), type = "closed")) +
  geom_vline(aes(xintercept = baseline.mu)) +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  scale_color_manual("Relationship to\nprobability of passing",
                     values = c("chartreuse3", "gray", "brown3")) +
  facet_wrap(~ pet) +
  labs(x = "Probability of passing", y = "",
       title = "Estimated relationships between student characteristics and probability of passing",
       subtitle = "By type of pet") +
  theme_bw() +
  theme(legend.pos = "bottom")
