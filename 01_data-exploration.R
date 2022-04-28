library(tidyverse)

# Load data ---------------------------------------------------------------

raw <- read.csv("LUNA_COEXIST_ALL_CLEAN_FINAL.csv")


# Data wrangling ----------------------------------------------------------

# Data marked for use
dat.use <- raw %>% 
  filter(USE == "Y")

cols.num <- c("FOCAL_HT_mm", "FOCAL_Biomass_g", "NEIGH_CT", "AVG_NEIGH_HT_mm",
              "TALLEST_IN_30cm_mm", "Per_Litt", "Per_Bare", "Per_Plant",
              "Neigh_per_BD", "Neigh_per_FL", "Neigh_per_FR")
dat.use[cols.num] <- sapply(dat.use[cols.num], as.numeric)
dat.use$TIME.POINT <- as.character(dat.use$TIME.POINT)

# Divide into treatment and control
use.trt <- dat.use %>% 
  filter(TRT == "LG")

use.cntrl <- dat.use %>% 
  filter(TRT == "CTRL")

# Average values across time points
avg.time <- dat.use %>% 
  group_by(FOCAL, FOCAL_ID, TRT, NEIGH) %>% 
  summarise(focal.ht = mean(FOCAL_HT_mm, na.rm = TRUE),
            focal.biom = mean(FOCAL_Biomass_g, na.rm = TRUE),
            per.litt = mean(Per_Litt, na.rm = TRUE),
            per.bare = mean(Per_Bare, na.rm = TRUE),
            per.plant = mean(Per_Plant, na.rm = TRUE),
            neigh.ct = mean(NEIGH_CT, na.rm = TRUE),
            neigh.bd = mean(Neigh_per_BD, na.rm = TRUE),
            neigh.fl = mean(Neigh_per_FL, na.rm = TRUE), 
            neigh.fr = mean(Neigh_per_FR, na.rm = TRUE),
            .groups = "keep")

avg.trt <- avg.time %>% 
  filter(TRT == "LG")

avg.cntrl <- avg.time %>% 
  filter(TRT == "CTRL")



# Visualization by treatment over time ------------------------------------

# Function
viz <- function(cntrl.y, trt.y, y, h) {
  print(summary(aov(cntrl.y ~ use.cntrl$TIME.POINT)))
  print(TukeyHSD(aov(cntrl.y ~ use.cntrl$TIME.POINT)))
  
  print(summary(aov(trt.y ~ use.trt$TIME.POINT)))
  print(TukeyHSD(aov(trt.y ~ use.trt$TIME.POINT)))
  
  bxplot <- ggplot(dat.use, aes(x = TIME.POINT, y = y)) +
    geom_boxplot() +
    geom_jitter() +
    facet_wrap(~TRT) +
    theme_bw() +
    labs(y = h)
  bxplot

}

viz(use.cntrl$FOCAL_HT_mm, use.trt$FOCAL_HT_mm, dat.use$FOCAL_HT_mm, "Focal height")
viz(use.cntrl$NEIGH_CT, use.trt$NEIGH_CT, dat.use$NEIGH_CT, "Neighbor count")
viz(use.cntrl$Per_Litt, use.trt$Per_Litt, dat.use$Per_Litt, "Percent litter")

viz(use.cntrl$Per_Bare, use.trt$Per_Bare, dat.use$Per_Bare, "Percent bare") # outlier?
filter(dat.use, Per_Bare > 750) # 890?
bare.out <- filter(dat.use, Per_Bare < 750)
bare.out.c <- filter(bare.out, TRT == "CTRL")
bare.out.t <- filter(bare.out, TRT == "LG")
summary(aov(bare.out.c$Per_Bare ~ bare.out.c$TIME.POINT))
TukeyHSD(aov(bare.out.c$Per_Bare ~ bare.out.c$TIME.POINT))
summary(aov(bare.out.t$Per_Bare ~ bare.out.t$TIME.POINT))

ggplot(bare.out, aes(x = TIME.POINT, y = Per_Bare)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(~TRT) +
  theme_bw() +
  labs(y = "Percent bare")

viz(use.cntrl$Per_Plant, use.trt$Per_Plant, dat.use$Per_Plant, "Percent plant")



# T-test comparison, time points averaged ---------------------------------

t.test(avg.cntrl$focal.ht, avg.trt$focal.ht)
t.test(avg.cntrl$focal.biom, avg.trt$focal.biom)
t.test(avg.cntrl$neigh.ct, avg.trt$neigh.ct)
t.test(avg.cntrl$per.litt, avg.trt$per.litt) # NS
t.test(avg.cntrl$per.bare, avg.trt$per.bare) # NS
t.test(avg.cntrl$per.plant, avg.trt$per.plant)
