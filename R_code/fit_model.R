library(tidyverse)

## ---- load-data ----

library(tidyverse)
df = read.csv("data/course_outcomes.csv", header = T, stringsAsFactors = F)

## ---- prepare-data-continuous ----

df = df %>%
  mutate(
    cs.prior.gpa = (prior.gpa - mean(prior.gpa)) / sd(prior.gpa),
    cs.height = (height - mean(height)) / sd(height)
  )

## ---- prepare-data-categorical ----

df = df %>%
  mutate(
    pet.type = fct_relevel(pet.type, "none", "dog", "cat", "fish"),
    favorite.color = fct_relevel(favorite.color, "blue", "red",
                                 "green", "orange")
  )

## ---- model ----

library(lme4)
pass.m = glm(passed ~ mac + glasses + pet.type + favorite.color + cs.prior.gpa +
               cs.height + tutoring,
             data = df, family = binomial(link = "logit"))
summary(pass.m)$coefficients

## ---- get-coefficients ----

coefs.df = summary(pass.m)$coefficients %>%
  data.frame() %>%
  rownames_to_column("parameter") %>%
  mutate(pretty.parameter = case_when(parameter == "(Intercept)" ~ "Intercept",
                                      grepl("TRUE$", parameter) ~
                                        str_to_title(gsub("TRUE", "",
                                                          parameter)),
                                      grepl("pet\\.type", parameter) ~
                                        paste("Pet:",
                                              str_to_title(gsub("pet\\.type",
                                                                "", parameter))),
                                      grepl("favorite\\.color", parameter) ~
                                        paste("Favorite color:",
                                              str_to_title(gsub("favorite\\.color",
                                                                "", parameter))),
                                      parameter == "cs.prior.gpa" ~ paste("Prior GPA\n(",
                                                                          round(sd(df$prior.gpa),
                                                                                1),
                                                                          "-pt increase)",
                                                                          sep = ""),
                                      parameter == "cs.height" ~ paste("Height\n(",
                                                                       round(sd(df$height),
                                                                             1),
                                                                       "-in increase)",
                                                                       sep = ""))) %>%
  dplyr::select(parameter, pretty.parameter, est = Estimate, se = Std..Error,
                z = z.value, p = Pr...z..)
