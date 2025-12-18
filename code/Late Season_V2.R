##### CHAPTER 2 LATE SEASON MODEL V2 #####
# this script will be the next step in the early season model
# I will mainly focus on changing the mixing models by looking at their plots to see if the polygons make sense
# then I can see if I need to alter TDFs 
# I also want to put in the raw data for the consumers instead of averages (as it is in V1) to get better estimates with variability
# when I do this, it will no longer do the weighted means so I may need to figure out if that sense or not

##### SECTION 1: LOAD PACKAGES AND IMPORT DATA #####
# set seed for reproducibility
set.seed(2283)

# load packages
library(tidyverse)
library(Rpath)
library(IsoPath)
library(stats)
library(factoextra)

# load data
iso_data_all = read.csv("data/iso_data.csv") |>
  select(-X)
iso_data_single = read.csv("data/single_iso_data.csv") |>
  select(-X)
biomass_data = read.csv("data/siene_data_for_biomass_est.csv")
biomass_data$Weight = as.numeric(biomass_data$Weight)
biomass_data$Weight[biomass_data$CommonName == "Skilletfish"] = 1.0 # biomass was blank for skilletfish

# load in parameter estimates
fish_parameters = read.csv("data/fish_parameters.csv") 

##### SECTION 2: SUBSET DATA AND COMBINE ALL ISOTOPE DATA #####
# filter for only prior to week 8
iso_data = iso_data_all %>%
  filter(Week >= 0) #adjust based on seasonailty interests wk0 includes all data

test2 = iso_data %>%
  filter(CommonName == "Black Sea Bass")

# fix spp names
iso_data$Species = replace(iso_data$Species, iso_data$Species == "Anchoa spp.", "Anchoa mitchilli")
iso_data$Species = replace(iso_data$Species, iso_data$Species == "Menidia spp.", "Menidia menidia")

# estimate phytoplankton data
phyto = iso_data_single %>%
  filter(CommonName == "Eastern Oyster")

phyto = phyto %>%
  mutate(
    CommonName = "Phytoplankton",
    Species = "Phyto",
    d15N = d15N - 3.4,
    d13C = d13C - 0.4
  )

# estimate zooplankton data
zoop = iso_data %>%
  filter(CommonName == "Anchovy")

zoop = zoop %>%
  mutate(
    CommonName = "Zooplankton",
    Species = "Zoop",
    d15N = d15N - 3.4,
    d13C = d13C - 0.4
  )

# add in the data with only a single estimate (primary producers and detritus)
iso_data = rbind(iso_data, iso_data_single, phyto, zoop)
# remove duplicates (the oysters)
iso_data = unique(iso_data)

# subset biomass data for only week 2 and 5 seine
#biomass_data = biomass_data %>%
#  filter(Date %in% c("9/7/23", "10/4/23"))

# make dataframe with summary length and iso data
mean_length = aggregate(TL_cm ~ Species, data = iso_data, FUN = mean)
mean_length = mean_length %>%
  rename(TL_cm_mean = TL_cm)
# n_mean = aggregate(d15N ~ Species, data = iso_data, FUN = mean)
# c_mean = aggregate(d13C ~ Species, data = iso_data, FUN = mean)
# length = mean_length %>%
#   left_join(n_mean, by = "Species") %>%
#   left_join(c_mean, by = "Species")


# change species names with spp. for merge
mean_length$Species = replace(mean_length$Species, mean_length$Species == "Anchoa spp.", "Anchoa mitchilli")
mean_length$Species = replace(mean_length$Species, mean_length$Species == "Menidia spp.", "Menidia menidia")

## export mean_length ##
write.csv(mean_length, file = "data/late_mean_length.csv")

# merge with the fish parameters
fish_parameters_length = left_join(mean_length, fish_parameters, by = "Species")

##### SECITON 3: ESTIMATE Q/B AND P/B #####
# P/B with minumum entry being 1 cm (smallest fish we caught in seine)
fish_parameters_length$PB = ((fish_parameters_length$K * (fish_parameters_length$Loo - fish_parameters_length$TL_cm))/(fish_parameters_length$TL_cm - 1))
# these may be a little low but they roughly match the methods paper

# Q/B using our mortality estimates
fish_parameters_length$QB = 10^(5.847 + 0.28*log(fish_parameters_length$PB) - 0.152*(log(fish_parameters_length$Winfinity)) 
                                - 1.36*(1000/(23.3+273.15)) + 0.062*fish_parameters_length$AspectRatio)

# add in Q/B and P/B for inverts
# q/b
fish_parameters_length$QB[fish_parameters_length$Species == "Callinectes sapidus"] = 4.0 # estimate from methods paper
fish_parameters_length$QB[fish_parameters_length$Species == "Crassostrea virginica"] = 2.0 # estimate from methods paper
fish_parameters_length$QB[fish_parameters_length$Species == "Ilyanassa obsoleta"] = 10.0 # estimate from methods paper
fish_parameters_length$QB[fish_parameters_length$Species == "Pagurus sp."] = 10.0 # estimate from methods paper
fish_parameters_length$QB[fish_parameters_length$Species == "Palaemon vulgaris"] = 10.0 # estimate from methods paper
fish_parameters_length$QB[fish_parameters_length$Species == "Panopeus herbstii"] = 10.0 # estimate from methods paper
fish_parameters_length$QB[fish_parameters_length$Species == "Busycon carica"] = 18.0 # estimate from Lucey thesis

# p/b
fish_parameters_length$PB[fish_parameters_length$Species == "Callinectes sapidus"] = 1.21 # estimate from methods paper
fish_parameters_length$PB[fish_parameters_length$Species == "Crassostrea virginica"] = 0.63 # estimate from methods paper
fish_parameters_length$PB[fish_parameters_length$Species == "Ilyanassa obsoleta"] = 2.0 # estimate from methods paper
fish_parameters_length$PB[fish_parameters_length$Species == "Pagurus sp."] = 2.0 # estimate from methods paper
fish_parameters_length$PB[fish_parameters_length$Species == "Palaemon vulgaris"] = 2.0 # estimate from methods paper
fish_parameters_length$PB[fish_parameters_length$Species == "Panopeus herbstii"] = 2.0 # estimate from methods paper
fish_parameters_length$PB[fish_parameters_length$Species == "Busycon carica"] = 10.0 # estimate Lucey thesis

##### CLUSTER BASED ON INSTINCT #####
# define groups
pelagic_fish = c("Anchovy", "Silverside", "Skilletfish", "Pigfish", "Gray Snapper", "Bluefish")
predator_fish = c("American Eel", "Spot", "Black Drum")
benthic_predators = c("Northern Pipefish", "Tautog", "Striped Killifish", "Silver Perch", "Weakfish", "Smallmouth Flounder", "Mummichog", "Summer Flounder", "Cunner")
structure_fish = c("Atlantic Croaker", "Feather Blenny", "Blackcheek Tonguefish", "Naked Goby", "Oyster Toadfish",
                   "Black Sea Bass", "Smallmouth Flounder")
detritivores = c("White Mullet", "Sheepshead Minnow")
benthic_inverts = c("Hermit Crab", "Mud Crab", "Grass Shrimp")
mud_snail = c("Mud Snail")
oyster = c("Eastern Oyster")
benthic_alg = c("Ulva")
benthic_alg2 = c("Green Algae", "Red Algae")
detritus = c("Detritus")
phyto = c("Phytoplankton")
zoop = c("Zooplankton")
blue_crab = c("Blue Crab")
whelk = c("Knobbed Whelk")

# add in functional group column to iso data
iso_data = iso_data %>%
  mutate(FunctionalGroup = case_when(
    CommonName %in% pelagic_fish ~ "PelagicFish",
    CommonName %in% predator_fish ~ "PredatorFish",
    CommonName %in% structure_fish ~ "StructureFish",
    CommonName %in% benthic_predators ~ "BenthicPredators",
    CommonName %in% benthic_inverts ~ "BenthicInvertebrates",
    CommonName %in% mud_snail ~ "MudSnails",
    CommonName %in% oyster ~ "Oysters",
    CommonName %in% benthic_alg ~ "BenthicAlg",
    CommonName %in% benthic_alg2 ~ "BenthicAlg2",
    CommonName %in% detritus ~ "Detritus",
    CommonName %in% phyto ~ "Phytoplankton",
    CommonName %in% zoop ~ "Zooplankton",
    CommonName %in% blue_crab ~ "BlueCrabs",
    CommonName %in% whelk ~ "Whelk",
    CommonName %in% detritivores ~ "Detritivore",
    TRUE ~ "Other"
  ))

# add in functional group column to biomass data
biomass_data = biomass_data %>%
  mutate(FunctionalGroup = case_when(
    CommonName %in% pelagic_fish ~ "PelagicFish",
    CommonName %in% predator_fish ~ "PredatorFish",
    CommonName %in% structure_fish ~ "StructureFish",
    CommonName %in% benthic_predators ~ "BenthicPredators",
    CommonName %in% benthic_inverts ~ "BenthicInvertebrates",
    CommonName %in% mud_snail ~ "MudSnails",
    CommonName %in% blue_crab ~ "BlueCrabs",
    CommonName %in% whelk ~ "Whelk",
    CommonName %in% detritivores ~ "Detritivore",
    TRUE ~ "Other"
  )) %>% 
  filter(FunctionalGroup != "Other")

## calculate proportions of biomass for each group ##
# drop those without weights
biomass_data_tots = biomass_data %>%
  filter(!is.na(Weight))
# get proportions
biomass_weights = biomass_data_tots %>%
  group_by(CommonName, Species, FunctionalGroup) %>%
  summarise(
    spp_biomass = sum(Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(FunctionalGroup) %>%
  mutate(
    group_biomass = sum(spp_biomass),
    prop_in_group = spp_biomass / group_biomass
  ) %>%
  ungroup()

# merge proportions with the SIA data
group_data = left_join(fish_parameters_length, biomass_weights, by = "Species")

weighted.sd = function(x, w, na.rm = FALSE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }
  wm <- weighted.mean(x, w)
  sqrt(sum(w * (x - wm)^2) / sum(w))
}

# calculate SIA sample sizes for weighting
iso_data = iso_data %>%
  group_by(Species) %>%
  mutate(
    samples = n()
  )

# take out species column from biomass_weights
biomass_weights_nospp = biomass_weights %>%
  select(-c(Species, FunctionalGroup))

# join isotope data with weights
iso_data = left_join(iso_data, biomass_weights_nospp, by = "CommonName")
iso_data = left_join(iso_data, fish_parameters_length, by = "Species")

# save iso_data for making plots in other scripts
write.csv(iso_data, file = "data/late_iso_all.csv")

# calculate weight for each individual SIA sample
iso_data$ind_prop = iso_data$prop_in_group/iso_data$samples

# filter for only species with PB estimates
iso_data_measured = iso_data %>%
  filter(!is.na(PB))
# make sure columns are numeric
iso_data_measured$d15N = as.numeric(iso_data_measured$d15N)
iso_data_measured$d13C = as.numeric(iso_data_measured$d13C)

# check to make sure Q/B is larger than P/B
iso_data_measured$QP = iso_data_measured$QB/iso_data_measured$PB

# the Q/B should be at least 3 times mortality (p/b)
for (i in 1:nrow(iso_data_measured)){
  if (iso_data_measured$QP[i] < 3){
    iso_data_measured$QB[i] = 3*iso_data_measured$PB[i]
  }
}
# check if it worked
iso_data_measured$QP = iso_data_measured$QB/iso_data_measured$PB

# calculate group weighted means and remove oysters since they do not need a weighted mean
group_means = iso_data_measured %>%
  group_by(FunctionalGroup) %>%
  summarise(
    d15N_w    = weighted.mean(d15N, ind_prop, na.rm = TRUE),
    d15N_w_sd = weighted.sd(d15N,   ind_prop, na.rm = TRUE),
    d13C_w    = weighted.mean(d13C, ind_prop, na.rm = TRUE),
    d13C_w_sd = weighted.sd(d13C,   ind_prop, na.rm = TRUE),
    PB_w      = weighted.mean(PB,   ind_prop, na.rm = TRUE),
    PB_w_sd   = weighted.sd(PB,     ind_prop, na.rm = TRUE),
    QB_w      = weighted.mean(QB,   ind_prop, na.rm = TRUE),
    QB_w_sd   = weighted.sd(QB,     ind_prop, na.rm = TRUE),
    biomass   = max(group_biomass) # they are all the same so max works
  ) %>% 
  filter(FunctionalGroup != "Oysters")

# calculate means of isotopes for the non-biomass functional groups (phyto, benthic algae, detritus, zoop, oysters)
group_means_nobio = iso_data %>%
  group_by(FunctionalGroup) %>%
  summarise(
    d15N_w    = mean(d15N, na.rm = TRUE),
    d15N_w_sd = sd(d15N, na.rm = TRUE),
    d13C_w    = mean(d13C, na.rm = TRUE),
    d13C_w_sd = sd(d13C, na.rm = TRUE),
    PB_w      = mean(PB, na.rm = TRUE),
    QB_w      = mean(QB, na.rm = TRUE),
    biomass   = max(group_biomass)
  )

# select for only the groups not in group_means (not weighted means in nobio)
diff_groups = setdiff(unique(group_means_nobio$FunctionalGroup), unique(group_means$FunctionalGroup))
group_means_nobio = group_means_nobio %>%
  filter(FunctionalGroup %in% diff_groups)

# dataframe with all functional groups
all_groups = bind_rows(group_means, group_means_nobio)
# make all NaN into NA
all_groups = all_groups %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

# save all_groups for making the plots in a seperate file
write.csv(all_groups, file = "data/late_iso_weighted.csv")

# bi-plot for all_groups
ggplot(iso_data, aes(x = d13C, y = d15N, color = FunctionalGroup, shape = FunctionalGroup)) +
  geom_point(alpha = 0.3) +
  geom_errorbar(data = all_groups, 
                aes(x = d13C_w, ymin = d15N_w - d15N_w_sd, ymax = d15N_w + d15N_w_sd, color = FunctionalGroup),
                width = 0.1, linewidth = 1, inherit.aes = FALSE) +
  geom_errorbarh(data = all_groups,
                 aes(y = d15N_w, xmin = d13C_w - d13C_w_sd, xmax = d13C_w + d13C_w_sd, color = FunctionalGroup),
                 height = 0.1, linewidth = 1, inherit.aes = FALSE) +
  geom_point(data = all_groups, aes(x = d13C_w, y = d15N_w, color = FunctionalGroup, shape = FunctionalGroup, fill = FunctionalGroup), 
             inherit.aes = FALSE, size = 3) +
  theme_minimal() +
  scale_shape_manual(values = 6:21) +
  xlim(c(-23, -8)) +
  ylim(c(3.5, 16.5)) +
  labs(x = 'd13C', y = 'd15N', title = 'Late Season Bi-Plot')

# fill in P/B and Q/B for these groups
# p/b
all_groups$PB_w[all_groups$FunctionalGroup == "Phytoplankton"] =  160   # estimate from methods paper
all_groups$PB_w[all_groups$FunctionalGroup == "BenthicAlg"]    =  80    # estimate from methods paper
all_groups$PB_w[all_groups$FunctionalGroup == "BenthicAlg2"]   =  80    # estimate from methods paper
all_groups$PB_w[all_groups$FunctionalGroup == "Zooplankton"]   =  25    # estimate from methods paper
all_groups$PB_w[all_groups$FunctionalGroup == "Oysters"]       =  0.63  # estimate from methods paper

# q/b
all_groups$QB_w[all_groups$FunctionalGroup == "Zooplankton"]   =  83.33 # estimate from methods paper
all_groups$QB_w[all_groups$FunctionalGroup == "Oysters"]       =  2     # estimate from methods paper

# biomass (total divided by the area seined)
all_groups$biomass = all_groups$biomass/(18.48*8)

# divide by catch efficiency for small fishes (average 0.605 from Parsley et al. 1989)
fish_groups = c("BenthicPredators", "Detritivore", "PelagicFish", "PredatorFish", "StructureFish")
all_groups = all_groups %>%
  mutate(biomass = if_else(FunctionalGroup %in% fish_groups,
                           biomass / 0.605,
                           biomass))

# biomass for oysters
all_groups$biomass[all_groups$FunctionalGroup == "Oysters"]    =  407.88 # estimate quadrat surveying

########## MODEL CONSTRUCTION ##########

##### DIET ESTIMATION #####
# list of functional groups that need diet estimation
predator_list = c("BenthicInvertebrates", "BenthicPredators", "BlueCrabs", "Detritivore", "MudSnails", "PelagicFish", "PredatorFish", 
                  "StructureFish", "Whelk", "Oysters")

# list of diets for each predator
prey_list = list(
  c("Phytoplankton", "BenthicAlg", "BenthicAlg2", "Zooplankton", "Detritus"), # diet for benthic invertebrates
  c("BenthicAlg", "BenthicAlg2", "Zooplankton", "BenthicInvertebrates", "MudSnails", "PelagicFish", "Detritus"), # diet for benthic predators
  c("BenthicAlg", "BenthicAlg2", "BenthicInvertebrates", "PelagicFish", "BenthicPredators", "StructureFish", "MudSnails", 
    "Detritus"), # blue crab diet
  c("Detritus", "Benthic Alg", "Benthic Alg2", "BenthicInvertebrates"), # diet for detritivores
  c("BenthicAlg", "BenthicAlg2", "Detritus"), # diet for mud snails
  c("MudSnails", "Phytoplankton", "Zooplankton", "Detritus"), # diet for pelagic fish
  c("BenthicInvertebrates", "BenthicPredators", "MudSnails", "PelagicFish", "StructureFish", "Oysters"), # diet for predator fish
  c("BenthicInvertebrates", "BenthicAlg", "BenthicAlg2", "MudSnails", "PelagicFish", "Zooplankton", "Detritus"), # diet for structure fish
  c("BenthicInvertebrates", "MudSnails", "Detritus", "Oysters"), # diet for whelk
  c("Phytoplankton", "Detritus") # diet for oysters
)

# loop for diet estimation in simmr
for (i in 1:length(predator_list)) {
  print("=====================================================================")
  print(predator_list[i])
  
  # subset measured isotope data for our predator
  predator_data = iso_data_measured %>%
    filter(FunctionalGroup == predator_list[[i]]) %>%
    rename(d13C_w = d13C,
           d15N_w = d15N) %>%
    select(FunctionalGroup, d13C_w, d15N_w)
  
  # take out the weighted average from the all_groups dataframe
  all_groups_nopred = all_groups %>%
    filter(FunctionalGroup != predator_list[[i]])
  
  # add in raw predator data to all_groups
  all_groups_simmr = bind_rows(all_groups_nopred, predator_data)
  
  # create simmr object
  simmr_object = format_simmr_data(data = all_groups_simmr, 
                                   consumer.names = predator_list[i],
                                   prey.names = prey_list[[i]],
                                   d13C.column = 'd13C_w', d13C.column.sd = 'd13C_w_sd', 
                                   d15N.column = 'd15N_w', d15N.column.sd = 'd15N_w_sd',
                                   species.column = "FunctionalGroup",
                                   disc.factors.c = 0.4, disc.factors.c.sd = 1.3,
                                   disc.factors.n = 3.4, disc.factors.n.sd = 1.0,
                                   data.type = "means")
  
  # run simmr model
  simmr.out = simmr_mcmc(simmr_object, mcmc_control = list(iter = 10000, burn = 1000, thin = 10, n.chain = 4))
  
  # check diagnostics
  summary(simmr.out, type = "diagnostics")
  
  # make plot
  plot(simmr_object, title = paste0(predator_list[i], " Bi-plot"))
  
  # if prey_list has Benthic Alg then combine sources here below
  if (grepl("BenthicAlg", prey_list[i])){
    # combine sources
    simmr.out.post = 
      combine_sources(simmr.out, 
                      to_combine = c("BenthicAlg", "BenthicAlg2"), 
                      new_source_name = "BenthicAlgae")
    # check diagnostics
    summary(simmr.out.post, type = "diagnostics")
    # save output
    assign(paste0("simmr_", predator_list[i]), simmr.out.post)
    # remove simmr.out.post
    rm(simmr.out.post)
  } else{
    # save simmr output with predator name
    assign(paste0("simmr_", predator_list[i]), simmr.out)
  }
  # delete predator_data, all_data_simmr, simmr_object, simmr.out
  rm(predator_data, all_groups_simmr, simmr_object, simmr.out, all_groups_nopred)
}

##### RPATH MODEL PARAMATERIZATION #####
# drop the BenthicAlg2 and rename BenthicAlg to BenthicAlgae
all_groups = all_groups %>%
  filter(FunctionalGroup != "BenthicAlg2") %>%
  mutate(FunctionalGroup = ifelse(FunctionalGroup == "BenthicAlg", "BenthicAlgae", FunctionalGroup))

all_groups = bind_rows(
  all_groups,
  tibble(FunctionalGroup = "Fishing")
)

# vector of group types for rpath parameters
types = c(0, # benthic inverts
          0, # benthic predators
          0, # blue crabs
          0, # detritivore
          0, # mud snails
          0, # pelagic fish
          0, # predator fish
          0, # structure fish
          0, # whelk
          1, # benthic algae
          2, # detritus
          0, # oysters
          1, # phytoplankton
          0, # zooplankton
          3) # fishing

# create parameter file
REco.params = create.rpath.params(group = all_groups$FunctionalGroup, type = types)

# assign biomass, pb, and qb parameters
REco.params$model[, Biomass := all_groups$biomass]
REco.params$model[, PB := all_groups$PB_w]
REco.params$model[, QB := all_groups$QB_w]

# EE for groups that need it
REco.params$model[Group == 'Phytoplankton', EE := 0.95]
REco.params$model[Group == 'Zooplankton', EE := 0.95]
REco.params$model[Group == 'BenthicAlgae', EE := 0.899]

#Biomass accumulation and unassimilated consumption
REco.params$model[, BioAcc  := c(rep(0, 14), rep(NA, 1))] # 0 for everything but fishing which is NA
REco.params$model[, Unassim := c(rep(0.2, 9), # benthic inverts - whelk
                                 0,   # benthic algae
                                 0.2, # detritus
                                 0.2, # oysters
                                 0,   # phytoplankton
                                 0.2, # zooplankton
                                 rep(NA, 1))] # fishing

#Detrital Fate
REco.params$model[, Detritus := c(rep(1, 10), # benthic inverts - benthic algae
                                  0,          # detritus
                                  rep(1, 3),  # oysters, phtyo, zoop
                                  0)]         # fishing

# fisheries landings and discards (assuming 0 here)
# create landings and discards object
land = c(rep(0, 14), rep(NA, 1))
disc = c(rep(0, 14), rep(NA, 1))

# assign them to the rpath model
REco.params$model[, Fishing := land]
REco.params$model[, Fishing.disc := disc]

##### PULL DIETS FROM SIMMR OUPUT AND PASS TO RPATH #####
# list of simmr objects in same order as predator_list
simmr_list = list(simmr_BenthicInvertebrates, simmr_BenthicPredators, simmr_BlueCrabs, simmr_Detritivore, simmr_MudSnails, 
                  simmr_PelagicFish,  simmr_PredatorFish, simmr_StructureFish, simmr_Whelk, simmr_Oysters)

# loop to pull out the means
for (i in 1:length(simmr_list)) {
  # extract means
  simmr.diet = simmr_diet_output_mean(simmr_list[[i]], all_groups$FunctionalGroup)
  # save simmr output with predator name
  assign(paste0("diet_", predator_list[i]), simmr.diet)
}

# standard deviation loop
for (i in 1:length(simmr_list)) {
  # extract means
  simmr.sd = simmr_diet_output_dist(simmr_list[[i]], all_groups$FunctionalGroup)
  # save simmr output with predator name
  assign(paste0("diet_", predator_list[i]), simmr.sd)
}

# pass diets to Rpath
REco.params$diet[, BenthicInvertebrates := c(diet_BenthicInvertebrates)]
REco.params$diet[, BenthicPredators := c(diet_BenthicPredators)]
REco.params$diet[, BlueCrabs := c(diet_BlueCrabs)]
REco.params$diet[, Detritivore := c(diet_Detritivore)]
REco.params$diet[, MudSnails := c(diet_MudSnails)]
REco.params$diet[, Oysters := c(diet_Oysters)]
REco.params$diet[, PelagicFish := c(diet_PelagicFish)]
REco.params$diet[, PredatorFish := c(diet_PredatorFish)]
REco.params$diet[, StructureFish := c(diet_StructureFish)]
REco.params$diet[, Whelk := c(diet_Whelk)]
REco.params$diet[, Zooplankton := c(rep(0, 12), 1, 0, 0)]

# print parameters
REco.params$model
REco.params$diet

write.rpath.params(REco.params, "late_season_V2", path = "~EcosimThis\Eco_Mass") #fix me
