library("tidyverse")
library("readxl")
library("lubridate")

df_raw <- read_xlsx("Datasæt Observation - Hvidovre Hospita.xlsx")
datoer_raw <- read_xlsx("Datasæt Observation - Hvidovre Hospita.xlsx", sheet = "Datooversigt for interventioner")

datoer <- datoer_raw %>% 
  mutate(date = make_date(year(Dato), month(Dato), day(Dato))) %>% 
  transmute(date, tr = Type)

df_pre <- df_raw %>% 
  pivot_longer(Ja:Nej) %>% 
  mutate(ids = map(value, seq_len)) %>% 
  unnest(cols = "ids") %>% 
  transmute(hc = ifelse(name == "Nej", 0, 1),
            male = ifelse(Køn == "M", 1, 0),
            time = Tidspunkt,
            date = make_date(year(Dato), month(Dato), day(Dato))) %>% 
  left_join(datoer, by = "date") %>% 
  mutate(tr = case_when(
    tr == "Kontrol" ~ "01. Baseline",
    tr == "Int. 1" ~ "02. Placement 1",
    tr == "Int. 2" ~ "04. Placement 1 + salience",
    tr == "Int. 3" ~ "03. Salience",
    tr == "Int. 4" ~ "05. Campaign",
    tr == "Int. 5" ~ "06. Placement 1 + salience + campaign",
    tr == "Int. 6" ~ "07. Placement 2",
    tr == "Int. 7" ~ "08. Placement 2 + salience",
    tr == "Int. 8" ~ "09. Placement 2 + salience + assertion",
    tr == "Int. 9" ~ "10. Placement 2 + salience + assertion + campaign"
 )) %>% 
  mutate(tr = fct_relevel(tr, c("Baseline", "Salience", "Campaign")),
         hc_pct = hc * 100)

df_covid_raw <- read_xlsx("Datasæt Observation - Hvidovre Hospital - 2021 .xlsx")

df_covid <- df_covid_raw %>% 
  pivot_longer(Ja:`Nej personale`) %>% 
  mutate(ids = map(value, seq_len)) %>% 
  unnest(cols = "ids") %>% 
  filter(!name %in% c("Ja personale", "Nej personale")) %>% 
  transmute(hc = ifelse(name == "Nej", 0, 1),
            male = ifelse(Køn == "M", 1, 0),
            time = Tidspunkt,
            date = make_date(year(Dato), month(Dato), day(Dato))) %>% 
  mutate(tr = "11. Treatment 10 in COVID context",
         hc_pct = hc * 100)

NROW(unique(df_pre$date))
max(df_pre$date)
min(df_pre$date)

NROW(unique(df_pre$date)) + NROW(unique(df_covid$date)) # 50 days
NROW(df_pre) + NROW(df_covid)

df_pre
df_covid

df <- bind_rows(df_pre, df_covid)

unique(df_covid$date)

df %>% 
  write_csv("hhc.csv")

