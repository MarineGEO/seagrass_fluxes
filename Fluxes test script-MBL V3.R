
######## ANALYSIS OF FLUXES IN SEAGRASS FOOD WEBS######## 

# Author: Jon Lefcheck
# Contact: LefcheckJ@si.edu
# Last updated: 20 Feb 2019

#####---------------------------------------------------

# load required libraries
library(fluxweb)
library(nlme)
library(readxl)
library(piecewiseSEM)
library(tidyverse)

source("https://gist.githubusercontent.com/jslefche/c480eeb2ad67ca7c9a46/raw/177e47862e4053337cca66e94f5f0b91d5e7086f/edgarMethod.R")

# define years
years <- c(2007:2013)

#####---------------------------------------------------

# # load datasets
# bugs <- read_excel("C:/Users/jslef/Desktop/Epifauna Mobile V10.xlsx", sheet = 1)
# 
# preds <- read_excel("C:/Users/jslef/Desktop/Duffy Field Predator Abundance V5.xlsx", sheet = 3)
# 
# grass <- read_excel("C:/Users/jslef/Desktop/Grass biomass V3.xlsx", sheet = 3)
# 
# epi <- read_excel("C:/Users/jslef/Desktop/Duffy Field Chl a V2.xlsx", sheet = 3)
# 
# env1 <- read.csv("C:/Users/jslef/Desktop/CBVGIWQ03-08.csv")
# 
# env2 <- read.csv("C:/Users/jslef/Desktop/CBVGIWQ08-13.csv")
# 
# # write function to break apart date
# makeDate <- function(x) {
# 
#   x. <- as.character(x)
# 
#   new.x <- do.call(rbind.data.frame, strsplit(x., "\\-"))
# 
#   names(new.x) <- c("year", "month", "day")
# 
#   return(new.x)
# 
# }
# 
# bugs <- cbind.data.frame(bugs, makeDate(bugs$date))
# 
# preds <- cbind.data.frame(preds, makeDate(preds$date))
# 
# grass <- cbind.data.frame(grass, makeDate(grass$date))
# 
# epi <- cbind.data.frame(epi, makeDate(epi$date))
# 
# # subset data
# bugs <- bugs %>% filter(year %in% years)
# 
# preds <- preds %>% filter(year %in% years)
# 
# grass <- grass %>% filter(year %in% years)
# 
# epi <- epi %>% filter(year %in% years)
# 
# # remove some species
# bugs <- bugs %>% filter(!species.name.revised %in%
#                           c("Amphibalanus sp.", "Mysid sp.", "Urosalpnix cinerea", "Crangon septemspinosa",
#                             "Palaemonetes pugio", "Palaemonetes sp.", "Copepodia sp.", "Mercenaria mercenaria",
#                             "Ostracoda sp.", "Pagurus annulipes", "Neomysis mercedis", "Foraminifera sp.",
#                             "Cumacean sp.", "Gemma gemma", "Gargeria rapax", "Bivalvia sp.", "Unidentified isopod",
#                             "Assiminea succinea", "Syngnathus sp.", "Serpulidae sp.", "Gobiesox strumosus",
#                             "Nematoda sp.", "Danilia sp.", "Neverita duplicata", "Decapod sp.", "Littorina littorea",
#                             "Astyris lunata", "Unidentified amphipod", "Gobiidae sp.", "Geukensia demissa",
#                             "Macoma balthica", "Xanthidae sp.", "Diadumene leucolena", "Callinectes sapidus",
#                             "Petricolaria pholadiformis", "Hargeria rapax", "Nassarius vibex",
#                             "Scianeid sp.", "Unidentified hermit crab", "Tunicata sp.", "Planaria sp.",
#                             "Rhithropanopeus harrisii", "Eupleura caudata", "Stylochus ellipticus", "Paralichthys dentatus",
#                             "Glycera sp.", "Tanaid sp.", "Unidentified larval fish")) %>%
#   filter(group != "Other")
# 
# preds <- preds %>% filter(!species.name.revised %in%
#                             c("Anguilla rostrata", "Unidentified larval fish", "Callinectes sapidus (megalopae)",
#                               "Unidentified juvenile fish", "Rhithropanopeus harrisii", "Pleuronectiformes sp.",
#                               "Rhithropanopeus harrisii", "Tozeuma sp.", "Scianeid sp.",
#                               "Gasterosteiformes sp.", "Menidia menidia", "Pagurus annulipes",
#                               "Unidentified larval fish", "Hippolyte pleuracanthus", "Blenniidae sp.", NA,
#                               "Crangon septemspinosa", "Unidentified larval fish 2", "Unidentified scianeid",
#                               "Hippocampus erectus", "Hippolyte pleuracantha", "Unidentified shrimp", "Gasterosteidae sp.",
#                               "Tetradontiformes sp.", "Palaemonetes sp.", "Rhithropanopeus sp.", "Gobiosoma sp.",
#                               "Alpheus sp."))
# 
# grass <- grass %>% filter(!species.name.revised %in%
#                             c("Zostera marina rhizome", "Ruppia maritima rhizome", "Botryllus schlosseri",
#                               "Diatoms spp.", "Porifera sp.", "Hydroid spp.", "Unidentified egg mass"))
# 
# # subset env data
# env <- rbind(env1, env2)
# 
# env$DateTimeStamp <- as.character(env$DateTimeStamp)
# 
# env.date <- do.call(rbind.data.frame, strsplit(env$DateTimeStamp, "\\/| "))
# 
# names(env.date) <- c("month", "day", "year", "hour")
# 
# env <- cbind.data.frame(env, env.date[, 1:3])
# 
# env <- env %>% filter(year %in% years)
# 
# env <- env %>% group_by(year, month, day) %>% summarize(Temp = mean(Temp, na.rm = T), Sal = mean(Sal, na.rm = T),
#                                                  DO_mgl = mean(DO_mgl, na.rm = T), pH = mean(pH, na.rm = T),
#                                                  Turb = mean(Turb, na.rm = T))
# 
# # write data
# write.csv(bugs, "C:/Users/jslef/Desktop/bugs.csv")
# 
# write.csv(preds, "C:/Users/jslef/Desktop/preds.csv")
# 
# write.csv(grass, "C:/Users/jslef/Desktop/grass.csv")
# 
# write.csv(epi, "C:/Users/jslef/Desktop/epi.csv")
# 
# write.csv(env, "C:/Users/jslef/Desktop/env.csv")

#####---------------------------------------------------

# read in cleaned up data from 2012
bugs <- read.csv("C:/Users/jslef/Desktop/bugs.csv")

preds <- read.csv("C:/Users/jslef/Desktop/preds.csv")

grass <- read.csv("C:/Users/jslef/Desktop/grass.csv")

epi <- read.csv("C:/Users/jslef/Desktop/epi.csv")

env <- read.csv("C:/Users/jslef/Desktop/env.csv")

# compute bug biomass from size fractionated abundances
bugs <- filter(bugs, group %in% c("Crustacean", "Mollusc", "Polychaete", "Caprellid"))

bugs <- edgarMethod(bugs, group = "group")

bugs$total.biomass.g <- bugs$total.biomass / 1000

# scale abundances/biomasses to 1 m^2 bottom area
bugs[, grepl("abund|biomass", colnames(bugs))] <- bugs[, grepl("abund|biomass", colnames(bugs))] * (1 / (0.2 * 0.2))

# compute predator biomass from L-W regressions
predB <- function(x) {
  
  a <- c(`Syngnathus sp.` = 0.00038, `Callinectes sapidus` = 0.0643, 
         `Paralichthys dentatus` = 0.01, `Palaemonetes sp.` = 0.588,
         `Syngnathus fuscus` = 0.00038, `Syngnathus floridae` = 0.0004,
         `Crangon septemspinosa` = 0.5999, `Leiostomus xanthurus` = 0.00871,
         `Gobiosoma bosc` = 0.00617, `Gobiesox strumosus` = 0.0128,
         `Symphurus plagiusa` = 0.00794 , `Opsanus tau` = 0.00955,
         `Fundulus heteroclitus` = 0.01072, `Lucania parva` = 0.02188,
         `Trinectes maculatus` = 0.01259, `Pomatomus saltatrix` = 0.01,
         `Cynoscion nebulosus` = 0.0131)

  b <- c(`Syngnathus sp.` = 3.13, `Callinectes sapidus` = 2.74, 
         `Paralichthys dentatus` = 3.16, `Palaemonetes sp.` = 2.53,
         `Syngnathus fuscus` = 3.13, `Syngnathus floridae` = 3.12,
         `Crangon septemspinosa` = 2.41, `Leiostomus xanthurus` = 3.08,
         `Gobiosoma bosc` = 3.08, `Gobiesox strumosus` = 3.03600,
         `Symphurus plagiusa` = 3.02, `Opsanus tau` = 3.07,
         `Fundulus heteroclitus` = 3.17, `Lucania parva` = 3.22,
         `Trinectes maculatus` = 3.08, `Pomatomus saltatrix` = 2.98,
         `Cynoscion nebulosus` = 3)
  
  x$total.biomass.g <- a[x$species.name.revised] * x$size ^ b[x$species.name.revised]
  
  return(x)
  
}

preds <- predB(preds)

# remove species without biomass
preds <- preds %>% filter(!is.na(total.biomass.g))

# scale predator abundance/biomass to 1 m^2 bottom area
preds[, grepl("abundance|biomass", colnames(preds))] <- preds[, grepl("abundance|biomass", colnames(preds))] * (1 / (0.52 * 5))

# scale grass/macroalgal dry weight to 1 m^2 bottom area
grass$dm <- grass$dm * (1 / (pi * (0.15/2)^2))

# compute epiphytic chl-a in ug
epi$date <- as.character(epi$date)

colnames(epi) <- gsub("X", "", colnames(epi))

epi <- do.call(rbind, lapply(unique(epi$date), function(i) {
  
  x <- subset(epi, date == i)
  
  # subtract average of blanks
  if("Blank" %in% x$replicate) {
    
    x[x$replicate != "Blank", grepl("nm", colnames(x))] <- x[x$replicate != "Blank", grepl("nm", colnames(x))] - 
      colMeans(x[x$replicate == "Blank", grepl("nm", colnames(x))], na.rm = T)
    
    x <- subset(x, replicate != "Blank")
    
  }
  
  # set negative absorbances to 0
  x[, grepl("nm", colnames(x))][x[, grepl("nm", colnames(x))] < 0] <- 0
  
  # use trichromatic equations to convert absorbance into ug mL-1
  x$chla <- 11.85 * (x$`664nm` - x$`750nm`) - 1.54 * (x$`647nm` - x$`750nm`) - 0.08 * (x$`630nm` - x$`750nm`)
  
  x$chlb <- 21.03 * (x$`647nm` - x$`750nm`) - 5.43 * (x$`664nm` - x$`750nm`) - 2.66 * (x$`630nm` - x$`750nm`)
  
  x$chlc <- 24.52 * (x$`630nm` - x$`750nm`) - 1.67 * (x$`664nm` - x$`750nm`) - 7.60 * (x$`647nm` - x$`750nm`)
  
  x$chlp <- 7.60 * ((x$`480nm` - 3 * x$`750nm`) - 1.49 * (x$`510nm` - 2 * x$`750nm`))
  
  
  # multiply by volume of sample (20 mL) to yield ug biomass
  x[, grepl("chl", colnames(x))] <- x[, grepl("chl", colnames(x))] * 20
  
  # convert leaf area to dry weight
  leaf.dw <- rowMeans( -6.921e-5 + 2.27e-3 * x[, grepl("Area", colnames(x))], na.rm = T)
  
  chla.g.dw <- (x$chla * 1e-6) / leaf.dw 
  
  data.frame(
    x[, c("year", "month", "day", "location", "inshore.offshore", "replicate")],
    chla.g.dw
  )
  
} ) )

# scale epi to dry weight zostera
grass.z <- grass %>% filter(species.name.revised == "Zostera marina blades")

epi <- left_join(epi, grass.z)

epi$total.chla.g <- epi$chla.g.dw * epi$dm

epi$species.name.revised <- "epiphytic.algae"

# remove seagrasses from grass as nothing eats them
grass <- filter(grass, !species.name.revised %in% c("Zostera marina blades", "Ruppia maritima blades"))

#####---------------------------------------------------

# prepare food web matrix

# get vector of primary producer names
pp.names <- c(as.character(unique(grass$species.name.revised)), "epiphytic.algae")

# get vector of predator names
pred.names <- c("Nereis sp.", "Palaemonetes vulgaris", "Palaemonetes sp.", as.character(unique(preds$species.name.revised)))

# get vectors of species
species <- c(as.character(unique(bugs$species.name.revised)), as.character(unique(preds$species.name.revised)),
             as.character(unique(grass$species.name.revised)), as.character(unique(epi$species.name.revised)))

# get vector of herbivore names
herb.names <- species[!species %in% c(pp.names, pred.names)]

# create diet matrix
mat <- matrix(0, nrow = length(species), ncol = length(species))

dimnames(mat) <- list(species, species)

# herbivores eat algae
mat[rownames(mat) %in% pp.names, colnames(mat) %in% herb.names] <- 1

# preds eat herbivores
mat[rownames(mat) %in% herb.names, colnames(mat) %in% pred.names] <- 1

#####---------------------------------------------------

# get vector of average biomasses to scale losses (per g  biomass)
indiv.bugs <- cbind.data.frame(species.name.revised = bugs$species.name.revised, avg = rowMeans(bugs[, 24:32]/bugs[, 11:19], na.rm = T)/1000) # convert to g from mg

indiv.bugs <- indiv.bugs %>% group_by(species.name.revised) %>% summarize(avg = mean(avg, na.rm = T))

indiv.preds <- preds %>% group_by(species.name.revised) %>% summarize(avg = mean(total.biomass.g / abundance))

# NAs for primary producers
indiv.grass <- data.frame(avg = rep(0, length(unique(grass$species.name.revised)))) 

indiv.epi <- data.frame(avg = rep(0, length(unique(epi$species.name.revised))))

bodymasses <- c(indiv.bugs$avg, indiv.preds$avg, indiv.grass$avg, indiv.epi$avg)

names(bodymasses) <- c(as.character(indiv.bugs$species.name.revised), as.character(indiv.preds$species.name.revised), 
                       as.character(unique(grass$species.name.revised)), "epiphytic.algae")

# compute metabolic rates for inverts/ectotherms
constants <- data.frame(
  species,
  a = ifelse(species %in% c(herb.names, pred.names[c(1:4)]), 17.17, ifelse(species %in% pred.names[-c(1:3)], 18.18, 0)),
  b = ifelse(species %in% pp.names, 0, -0.29)
)



### also compute with temp!!!!

losses <- constants[match(names(bodymasses), constants$species), "a"] * bodymasses ^
  constants[match(names(bodymasses), constants$species), "b"]



# compute fluxes for each month
fluxes <- do.call(rbind, lapply(years, function(i) {
  
  do.call(rbind, lapply(1:12, function(j) {
    
    print(paste(i, j))
  
    bugs.sub <- bugs %>% filter(year == i & month == j)
    
    preds.sub <- preds %>% filter(year == i & month == j)
    
    grass.sub <- grass %>% filter(year == i & month == j)
    
    epi.sub <- epi %>% filter(year == i & month == j)
    
    if(nrow(bugs.sub) == 0 | nrow(preds.sub) == 0 | nrow(epi.sub) == 0 | all(is.na(epi.sub$total.chla.g)))
      
      data.frame() else {
    
    # a vector with the total biomass of each population (per m^2 of bottom area)
    total.bugs <- bugs.sub %>% group_by(species.name.revised) %>% summarize(sm = sum(total.biomass.g))
    
    total.preds <- preds.sub %>% group_by(species.name.revised) %>% summarize(sm = sum(total.biomass.g))
    
    total.grass <- grass.sub %>% group_by(species.name.revised) %>% summarize(sm = sum(dm))
    
    total.epi <- epi.sub %>% summarize(sm = sum(total.chla.g, na.rm = T)) %>% cbind.data.frame(species.name.revised = "epiphytic.algae", .)
    
    biomasses.df <- rbind(total.bugs, total.preds, total.grass, total.epi)
    
    biomasses.df <- filter(biomasses.df, !is.na(species.name.revised) | sm != 0)
    
    biomasses <- biomasses.df$sm
    
    names(biomasses) <- biomasses.df$species.name.revised
    
    biomasses[biomasses < 0] <- 0
    
    # vector of organism types = plant, animal, detritus
    org.types <- ifelse(names(biomasses) %in% pp.names, "plant", "animal")
    
    eff <- c(animal = 0.906, plant = 0.545, detritus = 0.158)
    
    efficiences <- eff[org.types]
    
    names(efficiences) <- names(biomasses)
    
    # subset interaction matrix
    mat.sub <- mat[names(biomasses), names(biomasses)]
    
    # subset bodymasses
    losses.sub <- losses[names(biomasses)]
    
    # compute fluxes
    mat.fluxes <- fluxing(mat.sub, biomasses, losses.sub, efficiences)
    
    # get fluxes to herbivores
    herbivory <- sum(rowSums(mat.fluxes[rownames(mat.fluxes) %in% pp.names, , drop = F]))
    
    carnivory <- sum(rowSums(mat.fluxes[rownames(mat.fluxes) %in% herb.names, , drop = F]))
    
    data.frame(
      year = i,
      month = j,
      flux = c("Herbivory", "Predation"),
      value = c(herbivory, carnivory) / nrow(unique(epi.sub[, c("inshore.offshore", "replicate")])) / 12
    )
    
    }
    
  } ) )
  
} ) )

# plot results
fluxes.summary <- fluxes %>% group_by(month, flux) %>% 
  
  summarize(mean.value = mean(value, na.rm = T), se.value = plotrix::std.error(value, na.rm = T))

fluxes.summary$flux <- factor(fluxes.summary$flux, levels = c("Herbivory", "Predation"))

ggplot(fluxes.summary, aes(x = as.factor(month), y = mean.value)) +
  geom_line(aes(group = 1)) +
  geom_errorbar(aes(ymax = mean.value + se.value, ymin = mean.value - se.value), width = 0) +
  geom_point() +
  facet_grid(~ flux) +
  labs(x = "Month", y = bquote(Fluxes~(Joules~30~d^-1~m^-2))) +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

qplot(x = subset(fluxes, flux == "Herbivory")$value, y = subset(fluxes, flux == "Predation")$value)

# plot against diversity
bugs.S <- bugs %>% group_by(year, month) %>% summarize(richness = length(unique(species.name.revised))) %>%
  
  mutate(flux = "Herbivory") 

fluxes <- preds %>% group_by(year, month) %>% summarize(richness = length(unique(species.name.revised))) %>%
  
  mutate(flux = "Predation") %>% rbind(., bugs.S) %>% left_join(fluxes, .)

ggplot() +
  geom_point(data = fluxes, aes(x = log10(richness), y = log10(value), group = flux, col = flux)) +
  stat_smooth(method = "lm", data = fluxes, aes(x = log10(richness), y = log10(value), group = flux, col = flux), 
              se = F) +
  scale_color_manual(values = c("red", "blue"), name = "") +
  labs(x = bquote(log[10]~(Species~Richness)), y = bquote(log[10]~(J~30~d^-1~m^-2))) +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.6, 0.2)
  )

# plot against temperature

env$month <- as.numeric(env$month)

fluxes <- env %>% filter(month %in% unique(fluxes$month)) %>% group_by(month) %>% 
  
  summarize(temp = mean(Temp, na.rm = T), sal = mean(Sal, na.rm = T), turb = mean(Turb, na.rm = T)) %>%
  
  left_join(fluxes, .)

ggplot() +
  geom_point(data = fluxes, aes(x = log10(temp), y = log10(value), group = flux, col = flux)) +
  stat_smooth(method = "lm", data = fluxes, aes(x = log10(temp), y = log10(value), group = flux, col = flux), 
              se = F) +
  scale_color_manual(values = c("red", "blue"), name = "") +
  labs(x = bquote(log[10]~(Temp~degree~C)), y = bquote(log[10]~(Joules~30~d^-1~m^-2))) +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.6, 0.2)
  )

# construct model
herbivory.mod <- lme(log10(value) ~ log10(richness) + log10(temp) + log10(sal), random = list(year = ~1, month = ~1), data = subset(fluxes, flux == "Herbivory"))

car::vif(herbivory.mod)

coefs(herbivory.mod)

rsquared(herbivory.mod)

predation.mod <- lme(log10(value) ~ log10(richness) + log10(temp) + log10(sal), random = list(year = ~1, month = ~1), data = subset(fluxes, flux == "Predation"))

car::vif(predation.mod)

coefs(predation.mod)

rsquared(predation.mod)
