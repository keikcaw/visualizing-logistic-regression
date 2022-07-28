## ---- odds-ratio ----
coefs.df %>%
    mutate(odds = exp(est),
           lower.ninety.five = exp(est + (qnorm(0.025)*se)),
           lower.fifty = exp(est + (qnorm(0.25)*se)),
           median = exp(est + (qnorm(0.5)*se)),
           upper.fifty = exp(est + (qnorm(0.75)*se)),
           upper.ninety.five = exp(est + (qnorm(0.975)*se)),
           color = case_when((est + qnorm(0.025)*se) > 0 ~ "Positive",
                             (est + qnorm(0.975)*se) < 0 ~ "Negative",
                             T ~ "None"),
           color = fct_relevel(color, "Positive", "None", "Negative")) %>%
    ggplot(aes(x = pretty.parameter, y = median, color = color)) +
    geom_point(size = 3) +
    geom_linerange(aes(ymin = lower.fifty, ymax = upper.fifty), size = 2) +
    geom_linerange(aes(ymin = lower.ninety.five, ymax = upper.ninety.five), size = 1) +
    scale_color_manual(values = c("#009DD8", "#757575", "#EBAC20")) +
    labs(x = "Predictor", y = "Odds Ratio", color = "Effect Type") +
    #scale_x_discrete(labels = pretty.parameter) +
    scale_y_discrete(limits = c(0:5)) +
    geom_hline(yintercept = 1) +
    coord_flip() +
    facet_wrap(~ 'Odds Ratio Forest Plot')
