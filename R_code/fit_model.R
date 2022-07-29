library(tidyverse)
library(lme4)

# Load the data.
df = read.csv("data/course_outcomes.csv", header = T, stringsAsFactors = F) %>%
  mutate(pet.type = fct_relevel(pet.type, "none", "dog", "cat", "fish"),
         favorite.color = fct_relevel(favorite.color, "blue", "red", "green",
                                      "orange"))

# Fit the model.
pass.m = glm(passed ~ mac + glasses + pet.type + favorite.color + cs.prior.gpa +
               cs.height + tutoring,
             data = df, family = binomial(link = "logit"))
summary(pass.m)

# Create a dataframe of coefficients with pretty fields for graphing.
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
