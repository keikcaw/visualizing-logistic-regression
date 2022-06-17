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
      mutate(mu = model.matrix(pass.m, data = temp.df) %*% coefs.df$est,
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
      mutate(mu = model.matrix(pass.m, data = temp.df) %*% coefs.df$est,
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
