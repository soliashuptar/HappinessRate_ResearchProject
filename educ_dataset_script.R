library(readr)
education <- read_csv("culture/EDULIT_DS_18042020113952803.csv")
education
View(education)
df <- education %>% filter(education[2] == "Enrolment in primary education, both sexes (number)")
write.csv(df, 'enrolment_primary_educ.csv')

df <- education %>% filter(education[2] == "Enrolment in secondary education, both sexes (number)")
write.csv(df, 'enrolment_secondary_educ.csv')

df <- education %>% filter(education[2] == "Enrolment in tertiary education, all programmes, both sexes (number)")
write.csv(df, 'enrolment_tertiary_educ.csv')
