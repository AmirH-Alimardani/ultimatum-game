library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

df = read_excel("ultimatum.game.xlsx") %>%
  clean_names() 


ggplot(df, aes(x = offer)) +
  geom_density(
    fill = "lightblue",
    alpha = 0.6,
    adjust = 3.5
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  ) +
  labs(
    title = "توزیع کل",
    x = "Offered Share (0–100)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)
  )


#  فنی
df_offer_fanni <- df %>%
  filter(role == "پیشنهاد دهنده") %>%
  filter(faculty == "فنی و علوم") %>%
  mutate(offer = as.numeric(offer))

# توزیع
ggplot(df_offer_fanni, aes(x = offer)) +
  geom_density(
    fill = "lightblue",
    alpha = 0.6,
    adjust = 3
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  ) +
  labs(
    title = "توزیع در دانشکده فنی و علوم",
    x = "Offered Share (0–100)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)
  )




#  هنر
df_offer_honar <- df %>%
  filter(role == "پیشنهاد دهنده") %>%
  filter(faculty == "هنرهای زیبا") %>%
  mutate(offer = as.numeric(offer))

# توزیع
ggplot(df_offer_honar, aes(x = offer)) +
  geom_density(
    fill = "lightblue",
    alpha = 0.6,
    adjust = 3      # صاف‌تر شدن منحنی برای نمونه کوچک
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  ) +
  labs(
    title = "توزیع در دانشکده هنرهای زیبا",
    x = "Offered Share (0–100)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)
  )





# علوم و فنی
df_offer_olom <- df %>%
  filter(role == "پیشنهاد دهنده") %>%
  filter(faculty == "علوم اجتماعی") %>%
  mutate(offer = as.numeric(offer))

# توزیع
ggplot(df_offer_olom, aes(x = offer)) +
  geom_density(
    fill = "lightblue",
    alpha = 0.6,
    adjust = 3      
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  ) +
  labs(
    title = "توزیع در دانشکده علوم اجتماعی",
    x = "Offered Share (0–100)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)
  )

#________________________________________________________________________________
df_mean_offer <- df %>%
  filter(role == "پیشنهاد دهنده") %>%
  mutate(offer = as.numeric(offer)) %>%
  group_by(faculty) %>%
  summarise(
    mean_offer = mean(offer, na.rm = TRUE),
    n = n()
  )

ggplot(df_mean_offer, aes(x = faculty, y = mean_offer, fill = faculty)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    breaks = seq(20, 70, by = 5)
  ) +
  coord_cartesian(ylim = c(20, 70)) + 
  labs(
    title = "میانگین مبلغ پیشنهادی به تفکیک دانشکده",
    x = "دانشکده",
    y = "میانگین پیشنهاد (۰ تا ۱۰۰)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none" 
  )

#_______________________________________________________________________________
df_sex_offer <- df %>%
  filter(role == "پیشنهاد دهنده") %>%
  mutate(
    offer = as.numeric(offer),
    sex_group = if_else(sex == "آقا", "آقا", "خانم")
  ) %>%
  group_by(sex_group) %>%
  summarise(
    mean_offer = mean(offer, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(df_sex_offer, aes(x = sex_group, y = mean_offer, fill = sex_group)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    breaks = seq(30, 60, by = 5)
  ) +
  coord_cartesian(ylim = c(30, 60)) +
  labs(
    title = "میانگین مبلغ پیشنهادی بر جنسیت",
    x = "",
    y = "میانگین پیشنهاد (۰ تا ۱۰۰)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    legend.position = "none"
  )




df_sex_offer_raw <- df %>%
  filter(role == "پیشنهاد دهنده") %>%
  mutate(
    offer = as.numeric(offer),
    sex_group = if_else(sex == "آقا", "آقا", "خانم")
  )

# t-test میانگین پیشنهاد بر اساس جنسیت
t_test_sex <- t.test(
  offer ~ sex_group,
  data = df_sex_offer_raw
)

t_test_sex



#_______________________________________________________________________________
df_relig_offer <- df %>%
  filter(role == "پیشنهاد دهنده") %>%
  mutate(
    offer = as.numeric(offer),
    relig_group = if_else(religiosity > 2, "دینداری میانه به بالا", "دینداری میانه به پایین")
  ) %>%
  group_by(relig_group) %>%
  summarise(
    mean_offer = mean(offer, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(df_relig_offer, aes(x = relig_group, y = mean_offer, fill = relig_group)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    breaks = seq(30, 60, by = 5)
  ) +
  coord_cartesian(ylim = c(30, 60)) +
  labs(
    title = "میانگین مبلغ پیشنهادی بر اساس دینداری",
    x = "",
    y = "میانگین پیشنهاد (۰ تا ۱۰۰)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    legend.position = "none"
  )


df_relig_offer1 <- df %>%
  filter(role == "پیشنهاد دهنده") %>%
  mutate(offer = as.numeric(offer)) %>%
  filter(religiosity < 2 | religiosity > 3) %>%   # حذف 2 و 3
  mutate(
    relig_group = if_else(
      religiosity > 3,
      "دینداری بالا",
      "دینداری پایین"
    )
  ) %>%
  group_by(relig_group) %>%
  summarise(
    mean_offer = mean(offer, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


ggplot(df_relig_offer1, aes(x = relig_group, y = mean_offer, fill = relig_group)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    breaks = seq(30, 60, by = 5)
  ) +
  coord_cartesian(ylim = c(30, 60)) +
  labs(
    title = "میانگین مبلغ پیشنهادی بر اساس دینداری (حذف گروه‌های میانی)",
    x = "",
    y = "میانگین پیشنهاد (۰ تا ۱۰۰)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    legend.position = "none"
  )


#_______________________________________________________________________________

df_risk_offer <- df %>%
  filter(role == "پیشنهاد دهنده") %>%
  mutate(
    offer = as.numeric(offer),
    risk_group = if_else(risk_taking > 2, "ریسک پذیری بالا", "ریسک پذیری کم")
  ) %>%
  group_by(risk_group) %>%
  summarise(
    mean_offer = mean(offer, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(df_risk_offer, aes(x = risk_group, y = mean_offer, fill = risk_group)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    breaks = seq(30, 60, by = 5)
  ) +
  coord_cartesian(ylim = c(30, 60)) +
  labs(
    title = "میانگین مبلغ پیشنهادی بر اساس ریسک پذیری",
    x = "",
    y = "میانگین پیشنهاد (۰ تا ۱۰۰)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    legend.position = "none"
  )





df_job_offer <- df %>%
  filter(role == "پیشنهاد دهنده") %>%
  mutate(
    offer = as.numeric(offer),
    job_group = if_else(employment_status == "شاغل", "شاغل", "سایر")
  ) %>%
  group_by(job_group) %>%
  summarise(
    mean_offer = mean(offer, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(df_job_offer, aes(x = job_group, y = mean_offer, fill = job_group)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    breaks = seq(30, 60, by = 5)
  ) +
  coord_cartesian(ylim = c(30, 60)) +
  labs(
    title = "میانگین مبلغ پیشنهادی بر اساس وضعیت اشتغال",
    x = "",
    y = "میانگین پیشنهاد (۰ تا ۱۰۰)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    legend.position = "none"
  )



#_______________________________________________________________________________
df_responder <- df %>%
  filter(role == "پذیرنده")

df_responder <- df %>%
  filter(role == "پذیرنده")

df_responder %>% 
  summarise(
    rejection_rate = mean(accept_or_reject == "رد", na.rm = TRUE),
    n = n()
  )

df_responder <- df %>%
  filter(role == "پذیرنده")

df_responder %>% 
  filter(offer <= 50) %>%
  summarise(
    rejection_rate = mean(accept_or_reject == "رد", na.rm = TRUE),
    n = n()
  )

df_responder %>% 
  filter(offer < 50) %>%
  summarise(
    rejection_rate = mean(accept_or_reject == "رد", na.rm = TRUE),
    n = n()
  )


df_responder <- df %>%
  filter(role == "پذیرنده") %>%
  mutate(
    offer = as.numeric(offer),
    offer_group = case_when(
      offer <= 30 ~ "≤ 30",
      offer <= 40 ~ "30–40",
      offer <= 50 ~ "40–50",
      TRUE        ~ "> 50"
    )
  )

df_reject_by_offer <- df_responder %>%
  group_by(offer_group) %>%
  summarise(
    rejection_rate = mean(accept_or_reject == "رد", na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    offer_group = factor(
      offer_group,
      levels = c("≤ 30", "30–40", "40–50", "> 50")
    )
  )



ggplot(df_reject_by_offer,
       aes(x = offer_group, y = rejection_rate, fill = rejection_rate)) +
  geom_col(width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_gradient(
    low = "#fde0dd",   # صورتی خیلی روشن
    high = "red"   # قرمز تیره
  ) +
  labs(
    title = "نرخ رد پیشنهاد به تفکیک سطح پیشنهاد",
    x = "سطح پیشنهاد",
    y = "نرخ رد شدن",
    fill = "نرخ رد"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "right"
  )

#_______________________________________________________________________________
df_sex_reject <- df %>%
  filter(role == "پذیرنده") %>%
  mutate(
    sex_group = if_else(sex == "آقا", "آقا", "خانم"),
    rejected = accept_or_reject == "رد"
  ) %>%
  group_by(sex_group) %>%
  summarise(
    rejection_rate = mean(rejected, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(df_sex_reject, aes(x = sex_group, y = rejection_rate, fill = sex_group)) +
  geom_col(width = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 0.30, by = 0.05)
  ) +
  coord_cartesian(ylim = c(0, 0.30)) +
  labs(
    title = "نرخ رد پیشنهاد به تفکیک جنسیت",
    x = "جنسیت",
    y = "نرخ رد شدن"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    legend.position = "none"
  )


