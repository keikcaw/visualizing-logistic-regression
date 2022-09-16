## ---- extra-passes ----

extra.p = with(
  list(
    temp.df = map_dfr( #<<
      1:5000, #<<
      function(d) { #<<
        data.frame( #<<
          draw = d, #<<
          mu = model.matrix( #<<
            pass.m, #<<
            data = df %>% #<<
              filter(tutoring) %>% #<<
              mutate(tutoring = F) #<<
          ) %*% #<<
            rnorm(nrow(coefs.df), #<<
                  mean = coefs.df$est, #<<
                  sd = coefs.df$se) #<<
        ) #<<
      } #<<
    ) #<<
  ),
  {
    temp.df %>%
      mutate(pred = runif(n()) < invlogit(mu)) %>%
      group_by(draw) %>%
      summarise(pred.passed = sum(pred)) %>%
      ungroup() %>%
      mutate(extra.passed = #<<
               sum(df$passed & df$tutoring) #<<
               - pred.passed) %>% #<<
      ggplot(aes(x = extra.passed)) +
      geom_histogram(fill = "gray") +
      geom_vline(xintercept = 0) +
      labs(x = "Number of extra students\nwho passed because of tutoring",
           y = "Number of simulations",
           title = "Estimated number of extra students\nwho passed because of tutoring")
  }
)

## ---- extra-passes-by-group ----

extra.group.p = with(
  list(
    temp.df = map_dfr(1:5000,
                      function(d) { data.frame(draw = d,
                                               pet.type = df$pet.type[df$tutoring],
                                               mu = model.matrix(pass.m, data = df %>% filter(tutoring) %>% mutate(tutoring = F)) %*%
                                                 rnorm(nrow(coefs.df), mean = coefs.df$est, sd = coefs.df$se)) })
  ),
  { temp.df %>%
      mutate(pred = runif(n()) < invlogit(mu)) %>%
      group_by(pet.type, draw) %>% #<<
      summarise(pred.passed = sum(pred), .groups = "keep") %>%
      ungroup() %>%
      left_join(df %>% #<<
                  filter(tutoring) %>% #<<
                  group_by(pet.type) %>% #<<
                  summarise(actual.passed = #<<
                              sum(passed)) %>% #<<
                  ungroup(), #<<
                by = "pet.type") %>% #<<
      mutate(pet.type = str_to_title(pet.type), extra.passed = actual.passed - pred.passed) %>%
      group_by(pet.type) %>%
      summarise(lower.95 = quantile(extra.passed, 0.025), lower.50 = quantile(extra.passed, 0.25), median = median(extra.passed), upper.50 = quantile(extra.passed, 0.75), upper.95 = quantile(extra.passed, 0.975)) %>%
      ungroup() %>%
      mutate(pet.type = fct_reorder(pet.type, median)) %>%
      ggplot(aes(x = pet.type)) +
      geom_linerange(aes(ymin = lower.95, ymax = upper.95), size = 1) +
      geom_linerange(aes(ymin = lower.50, ymax = upper.50), size = 2) +
      geom_point(aes(y = median), size = 3) +
      geom_hline(yintercept = 0) +
      labs(subtitle = "By type of pet", x = "Pet type",
           y = "Number of extra students\nwho passed because of tutoring",
           title = "Estimated number of extra students\nwho passed because of tutoring") +
      coord_flip() }
)

## ---- potential-passes-by-group ----

potential.group.p = with(
  list(
    temp.df = map_dfr(1:5000,
                      function(d) { data.frame(draw = d,
                                               pet.type = df$pet.type[!df$tutoring],
                                               mu = model.matrix(pass.m, data = df %>% filter(!tutoring) %>% mutate(tutoring = T)) %*%
                                                 rnorm(nrow(coefs.df), mean = coefs.df$est, sd = coefs.df$se)) })
  ),
  { temp.df %>%
      mutate(pred = runif(n()) < invlogit(mu)) %>%
      group_by(pet.type, draw) %>%
      summarise(n.passed = sum(pred), #<<
                .groups = "keep") %>% #<<
      ungroup() %>%
      group_by(pet.type) %>%
      summarise(lower.95 = quantile(n.passed, 0.025), lower.50 = quantile(n.passed, 0.25), upper.50 = quantile(n.passed, 0.75), upper.95 = quantile(n.passed, 0.975), n.passed = median(n.passed)) %>%
      ungroup() %>%
      mutate(pass.type = "Predicted") %>% #<<
      bind_rows( #<<
        df %>% #<<
          filter(!tutoring) %>% #<<
          group_by(pet.type) %>% #<<
          summarise(n.passed = sum(passed)) %>% #<<
          ungroup() %>% #<<
          mutate(pass.type = "Actual") #<<
      ) %>% #<<
      mutate(pet.type = str_to_title(pet.type), pet.type = fct_reorder(pet.type, n.passed, max)) %>%
      ggplot(aes(x = pet.type, color = pass.type, shape = pass.type)) +
      geom_linerange(aes(ymin = lower.95, ymax = upper.95), size = 1, show.legend = F) + geom_linerange(aes(ymin = lower.50, ymax = upper.50), size = 2, show.legend = F) +
      geom_point(aes(y = n.passed), size = 3) +
      scale_color_manual(values = c("red", "black")) + scale_shape_manual(values = c(18, 16)) +
      labs(x = "Pet type", color = "", shape = "", subtitle = "By type of pet",
           y = "Number of untutored students\npredicted to pass with tutoring",
           title = "Estimated number of untutored students\nwho would have passed with tutoring") +
      expand_limits(y = 0) + coord_flip() }
)
