# pull out the coefficients from the model
get.coef.df <- summary(pass.m)$coefficients %>%
    data.frame(check.names =F) %>%
    rownames_to_column("predictor") %>%
    rename(slope = Estimate,
           std.error = 'Std. Error')



# --------------------------- Odds ratio plot ----------------------------------

get.coef.df %>%
    mutate(odds = exp(slope),
           lower.ninety.five = exp(slope + (qnorm(0.025)*std.error)),
           lower.fifty = exp(slope + (qnorm(0.25)*std.error)),
           median = exp(slope + (qnorm(0.5)*std.error)),
           upper.fifty = exp(slope + (qnorm(0.75)*std.error)),
           upper.ninety.five = exp(slope + (qnorm(0.975)*std.error)),
           color = case_when((slope + qnorm(0.025)*std.error) > 0 ~ "Positive",
                             (slope + qnorm(0.975)*std.error) < 0 ~ "Negative",
                             T ~ "None"),
           color = fct_relevel(color, "Positive", "None", "Negative")) %>%
    ggplot(aes(x = predictor, y = median, color = color)) +
    geom_point(size = 3) +
    geom_linerange(aes(ymin = lower.fifty, ymax = upper.fifty), size = 2) +
    geom_linerange(aes(ymin = lower.ninety.five, ymax = upper.ninety.five), size = 1) +
    scale_color_manual(values = c("#009DD8", "#757575", "#EBAC20")) +
    labs(x = "Predictor", y = "Odds Ratio", color = "Effect Type") +
    scale_x_discrete(labels = c("tutoringTRUE" = "Tutoring: yes",
                                "pet.typefish" = "Pet type: fish",
                                "pet.typedog" = "Pet type: dog",
                                "pet.typecat" = "Pet type: cat",
                                "macTRUE" = "Using a Mac: yes",
                                "glassesTRUE" = "Wear glasses: yes",
                                "favorite.colorred" = "Favorite color: red",
                                "favorite.colororange" = "Favorite color: orange",
                                "favorite.colorgreen" = "Favorite color: green",
                                "cs.prior.gpa" = "Prior undergraduate GPA",
                                "cs.height" = "Height",
                                "(Intercept)" = "Intercept")) +
    scale_y_discrete(limits = c(0:5)) +
    geom_hline(yintercept = 1) +
    coord_flip() +
    facet_wrap(~ 'Odds Ratio Forest Plot')


