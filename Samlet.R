library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

df <- read.csv("Arbejde.csv", header = TRUE, sep = ",")

# df <- df %>% filter(Completion.time >= "1/1/24 12:00:00" && Completion.time <= "1/7/24 12:00:00")

df <- df %>%
  mutate(Completion.time = mdy_hms(Completion.time)) %>%
  filter(Completion.time >= mdy_hms("9/1/24 00:00:00") &
           Completion.time <= mdy_hms("12/31/24 00:00:00"))

#### Alle typer ----

temp <- df %>%
  select(Type) %>%
  count(Type) %>%
  arrange(desc(n)) %>%
  mutate(Type = factor(Type, levels = Type))
  
ggplot(temp, aes(y = Type, x = n)) +
  geom_bar(stat = "Identity")

#### Alle studerende ----

studerende <- df %>% 
  count(Studieretning) %>% 
  arrange(desc(n)) %>% 
  mutate(Studieretning = factor(Studieretning, levels = Studieretning))
  


ggplot(studerende, aes(y = Studieretning, x = n)) +
  geom_bar(stat = "Identity") +
  ggtitle("Samlet antal vejledninger for studieretninger")

#### Vejledninger for Bachelor studerende ----

temp <- df %>%
  filter(Type == "Bachelor") %>% 
  count(Studieretning) %>% 
  arrange(desc(n)) %>%
  mutate(Studieretning = factor(Studieretning, levels = Studieretning))

ggplot(temp, aes(y = Studieretning, x = n)) +
  geom_bar(stat = "Identity") +
  ggtitle("Antal vejledninger for bachelor pr studieretning")

#### Vejledninger for kandidat studerende ----

temp <- df %>% filter(Type == "Kandidat") %>%
  count(Studieretning) %>%
  arrange(desc(n)) %>%
  mutate(Studieretning = factor(Studieretning, levels = Studieretning))

ggplot(temp, aes(y = Studieretning, x = n)) +
  geom_bar(stat = "Identity") +
  ggtitle("Kandidatstuderende")

#### Type af vejledning ----

temp <- df %>%
  filter(Emne !="") %>%
  select(Emne)


temp <- temp %>% separate_longer_delim(cols = Emne, delim = ";") %>%
  filter(Emne != "")

theme_counts <- temp %>%
  count(Emne) %>%
  filter(n > 2) %>%
  pull(Emne)

temp <- temp %>%
  filter(Emne %in% theme_counts) %>%
  count(Emne)

vejledning_sorted <- temp %>%
  arrange(desc(n)) %>%
  mutate(Emne = factor(Emne, levels = Emne))

ggplot(vejledning_sorted, aes(x = n, y = Emne)) +
  geom_bar(stat = "Identity")

# Emner der er interessante - Dispensation, kursustilmelding, droppe ud, mistrivelse

# dispensation

test <- function(df, tema) {
  temp <- df %>%
    select(Studieretning, Emne) %>%
    filter(Emne != "") %>%
    separate_longer_delim(cols = Emne, delim = ";") %>%
    filter(Emne == tema) %>%
    count(Studieretning) %>%
    arrange(desc(n)) %>%
    mutate(Studieretning = factor(Studieretning, levels = Studieretning))
  
  plot <- ggplot(temp, aes(x = n, y = Studieretning)) +
    geom_bar(stat = "identity") +
    labs(title = sprintf("Samlet studievejledning for: %s pr studieretning", tema), x = "Antal")
  
  return(plot)
}


emner <- c("Dispensation (Husk note)", 
  "Kursustilmelding", 
  "Mistrivsel (Husk note)", 
  "Tanker om at droppe ud (Husk note)",
  "Ikke-speciale projekter",
  "Generelt studietjek",
  "Valg undervejs")


for (i in emner) {
  print(test(df, i))
}


