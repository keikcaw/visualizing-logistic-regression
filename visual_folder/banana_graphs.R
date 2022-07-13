# ---------------------------- Banana Graph ------------------------------------
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
            filter(predictor == coefficient) %>%
            dplyr::select(slope)
    )

    # get the standard error for the coefficient of interest
    get.std.error <- as.numeric(
        df %>%
            filter(predictor == coefficient) %>%
            dplyr::select(std.error)
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





# ---- tutoring
create.banana.graph(get.coef.df, tutoringTRUE) +
    facet_wrap(~ 'Tutoring') +
    xlab('Predicted probability of student passing without tutoring') +
    ylab('Predicted probability of student passing with tutoring') +
    labs(color = "Effect")

# ---- pet type
pet.fish <- create.banana.graph(get.coef.df, pet.typefish) +
    facet_wrap(~ 'Fish')
pet.dog <- create.banana.graph(get.coef.df, pet.typedog) +
    facet_wrap(~ 'Dog')
pet.cat <- create.banana.graph(get.coef.df, pet.typecat) +
    facet_wrap(~ 'Cat')

# --- mac/glasses
create.banana.graph(get.coef.df, macTRUE) +
    facet_wrap(~ 'Mac User') +
    xlab('Predicted probability of student passing if they do not use a Mac') +
    ylab('Predicted probability of student passing if they do own a pet fish')
create.banana.graph(get.coef.df, glassesTRUE) +
    facet_wrap(~ 'Wear Glasses') +
    xlab('Predicted probability of student passing if they do not wear glasses') +
    ylab('Predicted probability of student passing if they do wear glasses')


# --- colors
create.banana.graph(get.coef.df, favorite.colorred)
create.banana.graph(get.coef.df, favorite.colororange)
create.banana.graph(get.coef.df, favorite.colorgreen)




#-----------------------Banana Graph figure sketches ---------------------------
###### To show that we can aid in the interpretation of the graph by annotating
pet.fish +
    xlab('Predicted probability of student passing if they do not own a pet fish') +
    ylab('Predicted probability of student passing if they do own a pet fish') +
    annotate("pointrange", x = 0.4, y = 0.615, ymin = 0.4, ymax =0.615,
             color = "black", size = 0.5) +
    annotate("text", x = 0.4, y = 0.34, size = 2.90, hjust = 0,
             label ="Example point:\nSome student who does not own a pet fish has a 40% chance of passing (x-axis value).\nHowever, if that same student did own a pet fish then their predicted probability of passing\nwould be approximately 61% (y-axis value). For this particular student, owning a pet fish\nincreased their chances of passing by 21% (distance from the dashed line to the black point).")



####### To show that showing many of these can get overwhelming on the eyes
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

####### Show that we can lighten the load by adding common axes
y.grob <- textGrob("Predicted probability of the student passing if they own the pet",
                   gp = gpar(col = "black", font.size = 9), rot = 90)
x.grob <- textGrob("Predicted probability of the student passing if they do not own the pet",
                   gp = gpar(col = "black", font.size = 11))

grid.arrange(
    arrangeGrob(
        plot_grid(
            (wrap_plots(pet.fish + theme(axis.title = element_blank()),
                        pet.dog + theme(axis.title = element_blank(),
                                        legend.title = element_blank()),
                        pet.cat + theme(axis.title = element_blank(),
                                        legend.title = element_blank()),
                        nrow = 1) +
                 plot_annotation(title = "Passing based on the pet a student owns")
             & theme(title = element_text(face = 'bold')))
        ),
        left = y.grob, bottom = x.grob
    )
)
