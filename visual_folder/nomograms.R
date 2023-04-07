## ---- create_logit_model_with_rms ----

# This the same model as pass.m, the only difference is that we are using rms's
# lrm function instead of glm.
library(rms)
library(tidyverse)
pass.m2 <- lrm(passed ~ mac + glasses + pet.type + favorite.color + cs.prior.gpa +
                   cs.height + tutoring,
               data = df %>%
                   mutate(favorite.color = case_when(favorite.color == "blue" ~ "b",
                                                     favorite.color == "green" ~ "g",
                                                     favorite.color == "orange" ~ "o",
                                                     favorite.color == "red" ~ "r"),
                          favorite.color = fct_infreq(favorite.color))
)


dd <- datadist(df %>%
                   mutate(favorite.color = case_when(favorite.color == "blue" ~ "b",
                                                     favorite.color == "green" ~ "g",
                                                     favorite.color == "orange" ~ "o",
                                                     favorite.color == "red" ~ "r"),
                          favorite.color = fct_infreq(favorite.color)))
options(datadist = 'dd')

## ---- create_nomogram_using_rms ----

nomogram <- nomogram(pass.m2, lp = F, fun = function(x) 1 / (1 + exp(-x)))
plot(nomogram)

## ---- create_dataframe_for_nomogram_values ----

predictor.names <- as.vector(names(nomogram))[1:7]
list.position <- as.vector(1:length(nomogram))[1:7]

fun <- function(p, n){

   return(data.frame(nomogram[[n]][1:3]) %>%
              rename("value" = p) %>%
              mutate(value = as.character(value),
                     name = p,
                     type = "coefficient") %>%
              dplyr::select(name,
                            type,
                            value,
                            points)
   )

}

extract.nomo.df<- map2_dfr(predictor.names, list.position, fun)

extract.nomo.df %>%
  mutate(name = gsub("cs.", "", name)) %>%
  left_join(df %>%
              dplyr::select(id, height, prior.gpa) %>%
              pivot_longer(cols = c("height", "prior.gpa")) %>%
              group_by(name) %>%
              summarise(mean = mean(value),
                        sd = sd(value)) %>%
              ungroup(),
            by = "name") %>%
  mutate(value = if_else(name %in% c("height", "prior.gpa"),
                         as.character(round((as.numeric(value) * sd) + mean)),
                         value)) %>%
  rbind(data.frame(nomogram[8][[1]]) %>%
            rename("points" = "x") %>%
            mutate(name = "total.points",
                   value = as.character(points),
                   points = seq(0, 100, length.out = 9),
                   type = name)
  ) %>%
  rbind(data.frame(nomogram[9][[1]][1:2]) %>%
            rename("points" = "x",
                   "value" = "x.real") %>%
            mutate(name = "probability",
                   value = as.character(value),
                   points = points*0.625,
                   type = name
            )
  ) %>%
  rbind(data.frame(points = seq(0, 100, by = 10))%>%
            mutate(name = "points",
                   value = as.character(seq(0, 100, by = 10)),
                   type = "points")
        ) %>%
  group_by(name) %>%
  mutate(max.point = max(points),
         min.point = min(points)) %>%
  ungroup() %>%
  mutate(type = factor(type, levels = c("probability",
                                        "total.points",
                                        "coefficient",
                                        "points"), ordered = T),
         pretty.parameter = case_when(name == "prior.gpa" ~ "Prior GPA",
                                      name == "height" ~ "Height",
                                      name == "pet.type" ~ "Pet type",
                                      name == "favorite.color" ~ "Favorite color",
                                      name == "tutoring" ~ "Tutoring",
                                      name == "glasses" ~ "Glasses",
                                      name == "mac" ~ "Mac",
                                      name == "total.points" ~ "Total points",
                                      name == "probability" ~ "Probability",
                                      name == "points" ~ "Points"))

## ---- create_nomogram ----

library(ggrepel)
text.size = 4
nomo.g <- nomogram.df %>%
    ggplot(aes(y= fct_reorder(pretty.parameter, as.integer(type)),
               label = value, group = type)) +
    geom_linerange(aes(xmin = min.point, xmax = max.point)) +
    geom_point(aes(x = points), shape = 3, size = 1.5) +
    geom_text_repel(aes(x = points, label = value),
                    size = text.size,
                    nudge_y = -0.15,
                    data = nomogram.df %>%
                        group_by(name) %>%
                        filter(length(name) <5) %>%
                        ungroup()) +
    geom_text(aes(x=points), nudge_y = 0.25, size = text.size,
              data = nomogram.df %>%
                  group_by(name) %>%
                  filter(length(name) >= 5) %>%
                  ungroup()) +
    labs(x = "",
         y = "") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

nomo.g

## ---- nomogram_identify_points ----

nomo.g +
    annotate("segment", x = 80, xend = 80, y = "Prior GPA", yend = "Points",
             color = good.color, size = 1,
             arrow = arrow(length = unit(0.1, "in"), type = "closed")) +
    annotate("segment",
             x = as.numeric(nomogram.df %>%
                                filter(value == "dog") %>%
                                dplyr::select(points)
                            ),
             xend = as.numeric(nomogram.df %>%
                                   filter(value == "dog") %>%
                                   dplyr::select(points)
                               ),
             y = "Pet type",
             yend = "Points",
             color = good.color,
             size = 1,
             arrow = arrow(length = unit(0.1, "in"), type = "closed"))

## ---- nomogram_identify_probability ----

nomo.g +
    annotate("segment", x = 50, xend = 50, y = "Total points", yend = "Probability",
             color = good.color, size = 1,
             arrow = arrow(length = unit(0.1, "in"), type = "closed"))

