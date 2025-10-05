# =============================
# =============================
nesting_data <- data.frame(
  area = c("Indian Ocean", "Pacific Ocean"),
  nests = c(356, 635)
)

# 
coordinates <- data.frame(
  area = c("Indian Ocean", "Pacific Ocean", "Hainan"),
  lat = c(26.06, -23.28, 19.3),
  lon = c(50.65, 118.57, 110.1)
)

# =============================
# =============================
library(geosphere)
library(dplyr)
library(tidyr)
library(ggplot2)

# =============================
# =============================
hainan_coord <- coordinates[coordinates$area == "Hainan", c("lon", "lat")]


nesting_data$lon <- coordinates$lon[match(nesting_data$area, coordinates$area)]
nesting_data$lat <- coordinates$lat[match(nesting_data$area, coordinates$area)]
nesting_data$distance_to_hainan <- distm(
  nesting_data[, c("lon", "lat")],
  hainan_coord,
  fun = distHaversine
) / 1000  # 转换为公里

# 
nesting_data$equal_contribution_prior <- 1/nrow(nesting_data)
nesting_data$population_abundance_prior <- with(nesting_data, nests / sum(nests))

lambda <- 5000
nesting_data$distance_prior <- exp(-nesting_data$distance_to_hainan / lambda)
nesting_data$distance_prior <- nesting_data$distance_prior / sum(nesting_data$distance_prior)

nesting_data$combined_prior <- with(nesting_data,
                                    equal_contribution_prior * population_abundance_prior * distance_prior
)
nesting_data$combined_prior <- nesting_data$combined_prior / sum(nesting_data$combined_prior)

# =============================
#
# =============================
calculate_bootstrap_ci <- function(data, n_boot = 1000) {
  results <- replicate(n_boot, {
    boot_sample <- data[sample(nrow(data), replace = TRUE), ]
    boot_sample$combined_prior <- with(boot_sample,
                                       equal_contribution_prior * (nests / sum(nests)) * distance_prior
    )
    boot_sample$combined_prior <- boot_sample$combined_prior / sum(boot_sample$combined_prior)
    boot_sample$combined_prior
  })
  ci <- apply(results, 1, quantile, probs = c(0.025, 0.975))
  return(data.frame(
    area = data$area,
    lower = ci[1, ],
    upper = ci[2, ]
  ))
}

ci_data <- calculate_bootstrap_ci(nesting_data)

# =============================
# 
# =============================
plot_data <- nesting_data %>%
  select(area, combined_prior, distance_to_hainan) %>%
  left_join(ci_data, by = "area") %>%
  mutate(area = factor(area, levels = area[order(distance_to_hainan)]))

# =============================
# =============================
p <- ggplot(plot_data, aes(x = area, y = combined_prior)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2,
    color = "black",
    size = 1    
  ) +
  geom_point(
    size = 4,
    shape = 21,
    fill = "white",
    color = "black",
    stroke = 1.2     ) +
  labs(
    x = "",
    y = "Estimated Source Contribution",
    title = ""
  ) +
  theme_classic(base_size = 9, base_family = "sans") +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 9),
    axis.ticks.length = unit(0.15, "cm"),
    axis.line = element_line(size = 1.2, color = "black"),
    axis.ticks = element_line(size = 1, color = "black")
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  coord_cartesian(ylim = c(0, NA))

# =============================
# 
# =============================
ggsave("E:/daimao/combined_prior_indian_pacific.pdf",
       p, width = 7, height = 4.8, units = "in",
       device = cairo_pdf)

