dat <- read.csv ("cleaned_wolf_parturition_May15_final_2026.csv", header=TRUE)

# CV per individual (need an individual ID column)
cv <- function(x) (sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE)) * 100

cv_individual <- dat %>%
  group_by(WolfID, Region2) %>%
  summarise(
    cv_ind = cv(DOY),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n > 1)

cv_population <- dat %>%
  group_by(Region2) %>%
  summarise(
    cv_pop = cv(DOY),
    .groups = "drop"
  )

cv_combined$Region2 <- gsub("Middle East", "Southwest Asia", cv_combined$Region2)
cv_population$Region2 <- gsub("Middle East", "Southwest Asia", cv_population$Region2)


cv_combined <- cv_individual %>%
  left_join(cv_population, by = "Region2")

ggplot() +
  geom_jitter(data = cv_combined,
              aes(x = Region2, y = cv_ind, colour = Region2),
              size = 3, alpha = 0.7, width = 0.15) +
  
  geom_segment(data = cv_population,
               aes(x = as.numeric(factor(Region2)) - 0.4,
                   xend = as.numeric(factor(Region2)) + 0.4,
                   y = cv_pop, yend = cv_pop),
               colour = "black", linewidth = 1.2) +
  
  geom_text(data = cv_population,
            aes(x = Region2, y = cv_pop, label = paste0("pop CV: ", round(cv_pop, 1))),
            vjust = -0.8, size = 3.2, colour = "black") +
  
  labs(
    x = NULL,
    y = "Coefficient of Variation (DOY)",
    title = "Within-individual vs. population CV by region"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 13),
    axis.title = element_text(size = 15),
    legend.position = "none"
  )
