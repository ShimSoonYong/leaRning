library(tidyverse)
library(readr)
library(skimr)
library(conflicted)
conflict_prefer(select, tidyverse::select)

Bank <- read_csv("data/Bank_Personal_Loan_Modelling.csv")

Bank %>% glimpse()
Bank %>% select(-ID) -> Bank
Bank %>% mutate(
  across(c(
    Education,
    `Personal Loan`,
    `Securities Account`,
    `CD Account`,
    Online,
    CreditCard),
    as.factor
  )
) -> Bank.prep


Bank.prep %>% skim()
princomp(Bank) -> pca.fit
summary(pca.fit)

library(factoextra)
fviz_pca_biplot(pca.fit)

library(MASS)
lda(`Personal Loan` ~ . , data = Bank.prep) -> lda.fit
lda.fit
plot(lda.fit)
