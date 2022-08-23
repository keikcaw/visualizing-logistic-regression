## ---- extra-passes ----
# Plot the predicted number of extra "passes" among students who got tutoring,
# relative to the number we think would have passed if they hadn't gotten
# tutoring.  Account for uncertainty both in the parameter estimates and in the
# outcomes for individual observations.
with(
  list(temp.df = expand.grid(draw = 1:1000,
                             id = df$id[df$tutoring]) %>%
         left_join(df, by = "id") %>%
         mutate(tutoring = F)),
  {
    temp.df %>%
      mutate(mu = model.matrix(pass.m, data = temp.df) %*%
               rnorm(nrow(coefs.df), mean = coefs.df$est, sd = coefs.df$se),
             pred = runif(n()) < invlogit(mu)) %>%
      group_by(draw) %>%
      summarise(pred.passed = sum(pred)) %>%
      ungroup() %>%
      mutate(extra.passed = sum(df$passed & df$tutoring) - pred.passed) %>%
      ggplot(aes(x = extra.passed)) +
      geom_histogram() +
      geom_vline(xintercept = 0) +
      labs(x = "Number of extra students who passed because of tutoring",
           y = "Number of simulations",
           title = "Estimated number of extra students who passed because of tutoring") +
      theme_bw()
  }
)


## ---- extra-passes-by-group ----
# Plot the predicted number of extra "passes" among students who got tutoring,
# relative to the number we think would have passed if they hadn't gotten
# tutoring, by group.  Account for uncertainty both in the parameter estimates
# and in the outcomes for individual observations.
with(
  list(temp.df = expand.grid(draw = 1:1000,
                             id = df$id[df$tutoring]) %>%
         left_join(df, by = "id") %>%
         mutate(tutoring = F)),
  {
    temp.df %>%
      mutate(mu = model.matrix(pass.m, data = temp.df) %*%
               rnorm(nrow(coefs.df), mean = coefs.df$est, sd = coefs.df$se),
             pred = runif(n()) < invlogit(mu)) %>%
      group_by(pet.type, draw) %>%
      summarise(pred.passed = sum(pred),
                .groups = "keep") %>%
      ungroup() %>%
      left_join(df %>%
                  filter(tutoring) %>%
                  group_by(pet.type) %>%
                  summarise(actual.passed = sum(passed)) %>%
                  ungroup(),
                by = "pet.type") %>%
      mutate(pet.type = str_to_title(pet.type),
             extra.passed = actual.passed - pred.passed) %>%
      group_by(pet.type) %>%
      summarise(lower.95 = quantile(extra.passed, 0.025),
                lower.50 = quantile(extra.passed, 0.25),
                median = median(extra.passed),
                upper.50 = quantile(extra.passed, 0.75),
                upper.95 = quantile(extra.passed, 0.975)) %>%
      ungroup() %>%
      mutate(pet.type = fct_reorder(pet.type, median)) %>%
      ggplot(aes(x = pet.type)) +
      geom_linerange(aes(ymin = lower.95, ymax = upper.95), size = 1) +
      geom_linerange(aes(ymin = lower.50, ymax = upper.50), size = 2) +
      geom_point(aes(y = median), size = 3) +
      geom_hline(yintercept = 0) +
      labs(x = "Pet type",
           y = "Number of extra students who passed because of tutoring",
           title = "Estimated number of extra students who passed because of tutoring",
           subtitle = "By type of pet") +
      coord_flip() +
      theme_bw()
  }
)

## ---- potential-passes-by-group ----
# Plot the predicted number of "passes" among students who DIDN'T get tutoring,
# compared to the number we think would have passed if they HAD gotten tutoring,
# by group.  Account for uncertainty both in the parameter estimates and in the
# outcomes for individual observations.
with(
  list(temp.df = expand.grid(draw = 1:1000,
                             id = df$id[!df$tutoring]) %>%
         left_join(df, by = "id") %>%
         mutate(tutoring = T)),
  {
    temp.df %>%
      mutate(mu = model.matrix(pass.m, data = temp.df) %*%
               rnorm(nrow(coefs.df), mean = coefs.df$est, sd = coefs.df$se),
             pred = runif(n()) < invlogit(mu)) %>%
      group_by(pet.type, draw) %>%
      summarise(n.passed = sum(pred),
                .groups = "keep") %>%
      ungroup() %>%
      group_by(pet.type) %>%
      summarise(lower.95 = quantile(n.passed, 0.025),
                lower.50 = quantile(n.passed, 0.25),
                upper.50 = quantile(n.passed, 0.75),
                upper.95 = quantile(n.passed, 0.975),
                n.passed = median(n.passed)) %>%
      ungroup() %>%
      mutate(pass.type = "Predicted") %>%
      bind_rows(df %>%
                  filter(!tutoring) %>%
                  group_by(pet.type) %>%
                  summarise(n.passed = sum(passed)) %>%
                  ungroup() %>%
                  mutate(pass.type = "Actual")) %>%
      mutate(pet.type = str_to_title(pet.type),
             pet.type = fct_reorder(pet.type, n.passed, max)) %>%
      ggplot(aes(x = pet.type, color = pass.type, shape = pass.type)) +
      geom_linerange(aes(ymin = lower.95, ymax = upper.95), size = 1) +
      geom_linerange(aes(ymin = lower.50, ymax = upper.50), size = 2) +
      geom_point(aes(y = n.passed), size = 3) +
      scale_color_manual(values = c("red", "black")) +
      scale_shape_manual(values = c(18, 16)) +
      guides(color = guide_legend(override.aes = list(linetype = 0)),
             shape = guide_legend(override.aes = list(linetype = 0))) +
      labs(x = "Pet type",
           y = "Number of untutored students predicted to pass with tutoring",
           color = "", shape = "",
           title = "Estimated number of untutored students who would have passed with tutoring",
           subtitle = "By type of pet") +
      expand_limits(y = 0) +
      coord_flip() +
      theme_bw()
  }
)
