library(tidyverse)
library(lme4)

# Load the data.
df = read.csv("data/course_outcomes.csv", header = T, stringsAsFactors = F) %>%
  mutate(pet.type = fct_relevel(pet.type, "none", "dog", "cat", "fish"),
         favorite.color = fct_relevel(favorite.color, "blue", "red", "green",
                                      "orange"))

# Fit the model.
pass.m = glm(passed ~ mac + glasses + pet.type + favorite.color + cs.prior.gpa +
               cs.height,
             data = df, family = binomial(link = "logit"))
summary(pass.m)
