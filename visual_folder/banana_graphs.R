## ---- banana-graph ----

est = coefs.df$est[coefs.df$parameter == "pet.typefish"]
se = coefs.df$se[coefs.df$parameter == "pet.typefish"]
effect.color = case_when(coefs.df$p[coefs.df$parameter == "pet.typefish"] > 0.05 ~ neutral.color,
                         est > 0 ~ good.color,
                         T ~ bad.color)
banana.p = data.frame(x = seq(0.01, 0.99, 0.01),
                      upper.95 = 0.975,
                      upper.50 = 0.75,
                      median = 0.5,
                      lower.50 = 0.25,
                      lower.95 = 0.025) %>%
  mutate(across(matches("median|upper|lower"), #<<
                function(q) { #<<
                  current.x = get("x") #<<
                  invlogit(logit(current.x) + #<<
                             est + #<<
                             (qnorm(q) * se)) #<<
                })) %>% #<<
  ggplot(aes(x = x, group = 1)) +
  geom_segment(x = 0, xend = 1, y = 0, yend = 1) +
  geom_ribbon(aes(ymin = lower.95, ymax = upper.95),
              fill = effect.color, alpha = 0.2) +
  geom_ribbon(aes(ymin = lower.50, ymax = upper.50),
              fill = effect.color, alpha = 0.4) +
  geom_line(aes(y = median), color = effect.color) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Baseline probablity of passing",
       y = "Probability of passing with effect",
       title = gsub("\n", " ",
                    coefs.df$pretty.parameter[coefs.df$parameter == "pet.typefish"]),
       subtitle = "Estimated relationship to probability of passing")

## ---- banana-graph-multiple ----

banana.multiple.p =
  expand.grid(x = seq(0.01, 0.99, 0.01), #<<
              pet = c("fish", "dog", "cat")) %>% #<<
  mutate(pet = paste("pet.type", pet, sep = ""),
         upper.95 = 0.975,
         upper.50 = 0.75,
         median = 0.5,
         lower.50 = 0.25,
         lower.95 = 0.025) %>%
  inner_join(coefs.df, #<<
             by = c("pet" = "parameter")) %>% #<<
  mutate(across(matches("median|upper|lower"),
                function(q) {
                  current.x = get("x")
                  invlogit(logit(current.x) + est + (qnorm(q) * se))
                })) %>%
  mutate(effect.color = case_when(p > 0.05 ~ neutral.color,
                                  est > 0 ~ good.color,
                                  T ~ bad.color)) %>%
  ggplot(aes(x = x, color = effect.color, fill = effect.color, group = 1)) +
  geom_segment(x = 0, xend = 1, y = 0, yend = 1, color = "black") +
  geom_ribbon(aes(ymin = lower.95, ymax = upper.95), color = NA, alpha = 0.2) +
  geom_ribbon(aes(ymin = lower.50, ymax = upper.50), color = NA, alpha = 0.4) +
  geom_line(aes(y = median)) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(x = "Baseline probablity of passing",
       y = "Probability of passing with effect",
       title = "Estimated relationship to\nprobability of passing") +
  facet_wrap(~ pretty.parameter, ncol = 1) #<<
