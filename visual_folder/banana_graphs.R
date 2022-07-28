library(cowplot)
library(grid)
library(gridExtra)
library(patchwork)

## ---- create-banana-graph ----
# log odds to probability function
log.to.prob <- function(value) {
    probability <- (exp(value)/(1+exp(value)))
    return(probability)
}

# compute logit function
k.logit <- function(value) {
    logitvalue <- (log(value/(1-value)))
    return(logitvalue)
}

# create banana graph
create.banana.graph <- function(df, coefficient) {
    coefficient <- deparse(substitute(coefficient))

    # first get the coefficient of interest
    get.coefficient <- as.numeric(
        df %>%
            filter(parameter == coefficient) %>%
            dplyr::select(est)
    )

    # get the standard error for the coefficient of interest
    get.std.error <- as.numeric(
        df %>%
            filter(parameter == coefficient) %>%
            dplyr::select(se)
    )
    # use those values to create a data frame that will be used by the graph
    result <- as.data.frame(1:99) %>%
        rename(x.axis = '1:99') %>%
        mutate(x.axis = x.axis/100,
               logits = k.logit(x.axis),
               value = get.coefficient + logits,
               prob = log.to.prob(value),
               lower.ninety.five = log.to.prob(value + (qnorm(0.025)*get.std.error)),
               lower.fifty = log.to.prob(value + (qnorm(0.25)*get.std.error)),
               median = log.to.prob(value + (qnorm(0.5)*get.std.error)),
               upper.fifty = log.to.prob(value + (qnorm(0.75)*get.std.error)),
               upper.ninety.five = log.to.prob(value + (qnorm(0.975)*get.std.error)),
               effect = case_when((get.coefficient + qnorm(0.025)*get.std.error) > 0 ~ "Positive",
                                  (get.coefficient + qnorm(0.975)*get.std.error) < 0 ~ "Negative",
                                  T ~ "None"),
               effect.color = case_when(effect == "Positive" ~ "#009DD8",
                                        effect == "None" ~ "#757575",
                                        effect == "Negative" ~ "#EBAC20")) %>%
        ggplot() +
        geom_line(aes(x = x.axis, y = median, color = effect.color), size = 1) +
        geom_line(aes(x = x.axis, y = x.axis), linetype = "dashed") +
        geom_line(aes(x = x.axis, y = lower.fifty, color = effect.color), alpha = 0.5)+
        geom_line(aes(x = x.axis, y = upper.fifty, color = effect.color),alpha = 0.5) +
        geom_ribbon(aes(x = x.axis, ymax= upper.fifty, ymin = lower.fifty,
                        fill = effect.color), alpha = 0.5) +
        geom_line(aes(x = x.axis, y = lower.ninety.five,color = effect.color), alpha = 0.2) +
        geom_line(aes(x = x.axis, y = upper.ninety.five, color = effect.color), alpha = 0.2) +
        geom_ribbon(aes(x = x.axis, ymax = upper.ninety.five, ymin = lower.ninety.five,
                        fill = effect.color), alpha = 0.2) +
        scale_fill_identity() +
        scale_color_identity(guide = 'legend',
                             name = "Effect Type",
                             labels = c("#009DD8" = "Positive",
                                        "#757575" = "None",
                                        "#EBAC20" = "Negative")) +
        scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
        scale_x_continuous(limits = c(0, 1), labels = scales::percent_format()) +
        theme(legend.position = "bottom")
    # +
    #     theme(axis.title = element_blank())

    return(result)
}






## ---- banana-graph-bare ----
# create a banana graph for students who own a pet fish
create.banana.graph(coefs.df, pet.typefish) +
    facet_wrap(~ 'Fish') +
    xlab('Predicted probability of student passing if they do not own a pet fish') +
    ylab('Predicted probability of student passing if they do own a pet fish')

## ---- banana-graph-annotated ----
create.banana.graph(coefs.df, pet.typefish) +
    facet_wrap(~ 'Fish') +
    xlab('Predicted probability of student passing if they do not own a pet fish') +
    ylab('Predicted probability of student passing if they do own a pet fish') +
    annotate("pointrange", x = 0.4, y = 0.165, ymin = 0.17, ymax =0.4,
             color = "black", size = 0.5) +
    annotate("text", x = 0.41, y = 0.05, size = 2.65, hjust = 0,
             label ="Example point:\nSome student who does not own a pet fish has a 40% chance of passing (x-axis\nvalue). However, if that same student did own a pet fish then their predicted\nprobability of passing would be approximately 17% (y-axis value). For this particular\nstudent, owning a pet fish decreased their chances of passing by 23% (distance\nfrom the ndashed line to the black point).")





## ---- banana-graph-multiple ----
y.grob <- textGrob("Predicted probability of the student passing if they own the pet",
                   gp = gpar(col = "black", font.size = 9), rot = 90)
x.grob <- textGrob("Predicted probability of the student passing if they do not own the pet",
                   gp = gpar(col = "black", font.size = 11))

grid.arrange(
    arrangeGrob(
        plot_grid(
            (wrap_plots(create.banana.graph(coefs.df, pet.typefish) +
                            facet_wrap(~ 'Fish') + 
                            theme(axis.title = element_blank()),
                        create.banana.graph(coefs.df, pet.typedog) +
                            facet_wrap(~ 'Dog') + 
                            theme(axis.title = element_blank(),
                                  legend.title = element_blank()),
                        create.banana.graph(coefs.df, pet.typecat) +
                            facet_wrap(~ 'Cat') + 
                            theme(axis.title = element_blank(),
                                  legend.title = element_blank()),
                        nrow = 1) +
                 plot_annotation(title = "Passing based on the pet a student owns")
             & theme(title = element_text(face = 'bold')))
        ),
        left = y.grob, bottom = x.grob
    )
)


## ---- banana-graph-less-polished ----
plot_grid(
    pet.fish +
        facet_wrap(~ 'Fish') +
        xlab('Predicted probability of the student passing if they do not own a pet fish') +
        ylab('Predicted probability of the student passing if they do own a pet fish'),
    NULL,
    pet.dog +
        facet_wrap(~ 'Dog') +
        xlab('Predicted probability of the student passing if they do not own a pet dog') +
        ylab('Predicted probability of the student passing if they do own a pet dog') +
        theme(legend.title = element_blank()),
    NULL,
    pet.cat +
        xlab('Predicted probability of the student passing if they do not own a pet cat') +
        ylab('Predicted probability of the student passing if they do own a pet cat') +
        theme(legend.title = element_blank()),
    nrow = 1,
    rel_widths = c(1, 0.25, 1, 0.25, 1)
)

