# make calculations for numbers needed in the paper
a <- data_extra %>%
  group_by(LTcateg, biome) %>%
  summarize(
    minR = min(Richness_2020/areakm2, na.rm = T),
            meanR = mean(Richness_2020/areakm2, na.rm = T),
            maxR = max(Richness_2020/areakm2, na.rm = T),
            minE = min(Endemism_2020/areakm2, na.rm = T),
            meanE = mean(Endemism_2020/areakm2, na.rm = T),
            maxE = max(Endemism_2020/areakm2, na.rm = T),
            minFo = min(p_for23, na.rm = T),
            meanFo = mean(p_for23, na.rm = T),
            maxFo = max(p_for23, na.rm = T))
a

b <- data_extra %>%
  group_by(LTcateg) %>%
  summarize(minR = min(Richness_2020/areakm2, na.rm = T),
            meanR = mean(Richness_2020/areakm2, na.rm = T),
            maxR = max(Richness_2020/areakm2, na.rm = T),
            minE = min(Endemism_2020/areakm2, na.rm = T),
            meanE = mean(Endemism_2020/areakm2, na.rm = T),
            maxE = max(Endemism_2020/areakm2, na.rm = T))
a