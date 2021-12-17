library("tidyverse")
library("stargazer")

df <- read_csv("hhc.csv")

df %>% 
  group_by(date) %>% 
  summarise(n = n(), 
            treatment = paste0(unique(tr))) %>% 
  print(n = NROW(.))

df %>% 
  group_by(tr) %>% 
  summarise(hc = mean(hc)) %>% 
  ggplot(aes(hc, tr, fill = tr)) +
  geom_col() +
  theme_bw() +
  labs(y = NULL,
       x = "Hand hygiene compliance (%)") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.55)) +
  geom_text(aes(label = round(hc, 4) * 100), hjust= -0.5, colour = "black") +
  scale_fill_manual(values = c("gray80", "gray70", "gray60", rep("#3D9970", 3), rep("#001f3f", 4), "#0074D9")) +
  theme(legend.position = "none") 

ggsave("hc_effects.png", width = 8, height = 4)

df %>% 
  group_by(tr, male) %>% 
  summarise(hc = mean(hc)) %>% 
  mutate(male = ifelse(male == 1, "Men", "Women")) %>% 
  ggplot(aes(hc, tr, group = male, fill = male)) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(x = "Hand hygiene compliance (%)",
       y = NULL) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.55)) +
  geom_text(aes(label = round(hc, 4) * 100), hjust= -0.5, position = position_dodge(width = 1), colour = "black") +
  ggthemes::scale_fill_gdocs() +
  labs(fill = NULL) +
  theme(legend.justification=c(-6.5,6.2), legend.position=c(0.00,0.98))

ggsave("hc_effects_gender.png", width = 10, height = 5)

reg1 <- lm(hc_pct ~ tr + male + date + time, data = df)

stargazer(reg1, type = "text",
          keep = c("tr", "male"),
          covariate.labels = c(
            "02: Placement 1",
            "03: Salience",
            "04: Placement 1 + salience",
            "05: Campaign",
            "06: Placement 1 + salience + campaign",
            "07: Placement 2",
            "08: Placement 2 + salience",
            "09: Placement 2 + salience + assertion",
            "10: Placement 2 + salience + assertion + campaign",
            "11: Treatment 10 in COVID context",
            "Male"),
          out = "regression.htm")

reg2 <- lm(hc_pct ~ tr*male + date + time, data = df)  

stargazer(reg2, type = "text",
          keep = c(":male"),
          covariate.labels = c(
            "02: Placement 1",
            "03: Salience",
            "04: Placement 1 + salience",
            "05: Campaign",
            "06: Placement 1 + salience + campaign",
            "07: Placement 2",
            "08: Placement 2 + salience",
            "09: Placement 2 + salience + assertion",
            "10: Placement 2 + salience + assertion + campaign",
            "11: Treatment 10 in COVID context"),
          out = "regression_interaction.htm")

# Time of day
lm(hc_pct ~ tr + male + date + time, data = df) %>% 
  summary()
