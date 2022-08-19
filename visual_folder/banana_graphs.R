# ---------------------------- Banana Graph ------------------------------------
library(patchwork)
library(logitnorm)

## ---- create-banana-graph ----
# Function that creates a banana graph.  It assumes that the coefficients of the
# model are stored in an object called `coefs.df`.  The argument is a string
# that identifies the predictor of interest.
create.banana.graph <- function(predictor) {
  est = coefs.df$est[coefs.df$parameter == predictor]
  se = coefs.df$se[coefs.df$parameter == predictor]
  effect.color = case_when(coefs.df$p[coefs.df$parameter == predictor] > 0.05 ~ neutral.color,
                           est > 0 ~ good.color,
                           T ~ bad.color)
  data.frame(x = seq(0.01, 0.99, 0.01),
             upper.95 = 0.975,
             upper.50 = 0.75,
             median = 0.5,
             lower.50 = 0.25,
             lower.95 = 0.025) %>%
    mutate(across(matches("median|upper|lower"),
                  function(q) {
                    current.x = get("x")
                    invlogit(logit(current.x) + est + (qnorm(q) * se))
                  })) %>%
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
                      coefs.df$pretty.parameter[coefs.df$parameter == predictor]),
         subtitle = "Estimated relationship to probability of passing") +
    theme_bw()
}

## ---- banana-graph-bare ----
# create a banana graph for students who own a pet fish
pet.fish = create.banana.graph("pet.typefish")
pet.fish

## ---- banana-graph-annotated ----
pet.fish +
  annotate("segment", x = 0.4, xend = 0.4, y = 0.4,
           yend = invlogit(logit(0.4) +
                             coefs.df$est[coefs.df$parameter == "pet.typefish"]),
           color = "black", size = 0.5,
           arrow = arrow(type = "closed", length = unit(0.02, units = "npc"))) +
  annotate("text", x = 0.1, y = 0.7, size = 2.90, hjust = 0,
           label = str_wrap(paste("Some student who does not own a pet fish has a 40% chance of passing (x-axis value).",
                                  " However, if that same student did own a pet fish,",
                                  " their predicted probability of passing would be ",
                                  round(invlogit(logit(0.4) +
                                                   coefs.df$est[coefs.df$parameter == "pet.typefish"]) * 100),
                                  "% (y-axis value).",
                                  sep = ""),
                            60))

#-----------------------Banana Graph figure sketches ---------------------------
## ---- banana-graph-multiple ----
expand.grid(x = seq(0.01, 0.99, 0.01),
            pet = c("fish", "dog", "cat")) %>%
  mutate(pet = paste("pet.type", pet, sep = ""),
         upper.95 = 0.975,
         upper.50 = 0.75,
         median = 0.5,
         lower.50 = 0.25,
         lower.95 = 0.025) %>%
  inner_join(coefs.df, by = c("pet" = "parameter")) %>%
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
       title = "Estimated relationship to probability of passing") +
  facet_wrap(~ pretty.parameter) +
  theme_bw()

## ---- banana-graph-less-polished ----
library(patchwork)
pet.fish + pet.dog + pet.cat
