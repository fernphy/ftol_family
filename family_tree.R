library(tidyverse)
library(ftolr)
library(ape)
library(MonoPhy)
library(assertr)

# Load ultrametric fern tree, drop outgroup
phy <- ft_tree(branch_len = "ultra", rooted = TRUE, drop_og = TRUE)

# Load fern taxonomy
taxonomy <- ftol_taxonomy %>%
  # Subset to only species in tree
  filter(species %in% phy$tip.label)

# Analyze monophyly of each family
family_mono_test <- AssessMonophyly(
  phy,
  as.data.frame(taxonomy[, c("species", "family")])
)

# Check that all families are monophyletic or monotypic
family_mono_summary <-
  family_mono_test$family$result %>%
  rownames_to_column("family") %>%
  as_tibble() %>%
  assert(in_set("Yes", "Monotypic"), Monophyly)

# Get one exemplar tip (species) per family
rep_tips <-
  taxonomy %>%
  group_by(family) %>%
  slice(1) %>%
  ungroup()

# Subset phylogeny to one tip per family
phy_family <- ape::keep.tip(phy, rep_tips$species)

# Relabel with family names
new_tips <-
tibble(species = phy_family$tip.label) %>%
  left_join(rep_tips, by = "species") %>%
  pull(family)

phy_family$tip.label <- new_tips

# Visualize tree
plot(ladderize(phy_family))

# Write out tree
ape::write.tree(
  phy_family, paste0("output/ftol_family_tree_v", ft_data_ver(), ".tre"))
