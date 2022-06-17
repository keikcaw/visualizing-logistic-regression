library(tidyverse)
library(logitnorm)

n.students = 5000
parameters = list(intercept = 1.4, mac = 0, glasses = 0.3, pet.typedog = -0.1,
                  pet.typecat = 0.2, pet.typefish = 0.8,
                  favorite.colorred = 0, favorite.colorgreen = -0.3,
                  favorite.colororange = -0.1, cs.prior.gpa = 1,
                  cs.height = 0.2, tutoring = 0.1)

df = data.frame(id = 1:n.students) %>%
  mutate(mac = runif(n()) < 0.3,
         glasses = runif(n()) < 0.4,
         pet.rand = runif(n()),
         pet.type = case_when(pet.rand < 0.4 ~ "none",
                              pet.rand < 0.8 ~ "dog",
                              pet.rand < 0.95 ~ "cat",
                              T ~ "fish"),
         pet.type = fct_relevel(pet.type, "none", "dog", "cat", "fish"),
         color.rand = runif(n()),
         favorite.color = case_when(color.rand < 0.4 ~ "blue",
                                    color.rand < 0.7 ~ "red",
                                    color.rand < 0.9 ~ "green",
                                    T ~ "orange"),
         favorite.color = fct_relevel(favorite.color, "blue", "red", "green",
                                      "orange"),
         prior.gpa = round(rbeta(n(), 5, 1) * 4, 2),
         cs.prior.gpa = (prior.gpa - mean(prior.gpa)) / sd(prior.gpa),
         height = round(rnorm(n(), 66, 3)),
         cs.height = (height - mean(height)) / sd(height),
         tutoring = runif(n()) < 0.5) %>%
  dplyr::select(id, mac, glasses, pet.type, favorite.color, prior.gpa,
                cs.prior.gpa, height, cs.height, tutoring)
df$passed = invlogit((model.matrix(~ mac + glasses + pet.type + favorite.color +
                                     cs.prior.gpa + cs.height + tutoring,
                                   data = df) %*% unlist(parameters))[,1])
df$passed = df$passed > runif(nrow(df))

write.csv(df, "data/course_outcomes.csv", row.names = F)
