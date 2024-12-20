# 1.
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

library(dplyr)
library(tidyr)

data(mtcars)
df <- as.data.frame(mtcars)

# 2.
df <- df %>%
  select(mpg, cyl, hp, gear) %>% 
  filter(cyl > 4)
print(df)

# 3.
df <- df %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)
print(df)

# 4.
df <- df %>%
  mutate(eficiencia = consumo / potencia)

df_aggregated <- df %>%
  group_by(cyl) %>%
  summarise(consumo_medio = mean(consumo, na.rm = TRUE), 
            potencia_maxima = max(potencia, na.rm = TRUE))
print(df)
print(df_aggregated)

# 5.
df_gear <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

df <- df %>%
  left_join(df_gear, by = "gear")
print(df)

# 6.
df_long <- df %>%
  pivot_longer(cols = c(consumo, potencia, eficiencia),
               names_to = "medida",
               values_to = "valor")
print(df_long)

duplicados <- df_long %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
print(duplicados)

df_wide <- df_long %>%
  pivot_wider(names_from = medida, 
              values_from = valor, 
              values_fn = mean)
print(df_wide)