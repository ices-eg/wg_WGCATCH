# Combined H1/H5 selection-method proportions for all sampling levels through Sample (SA)
# Uses RDBES 2025 CSV files in the two hierarchy folders.

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

root <- "C:/Users/AS15/OneDrive - CEFAS/WGCATCH/WGCATCH 2026/Tor.a/RDBES_wgcatch"
out_dir <- file.path(root, "hierarchy_plots")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

read_rdbes <- function(folder, table) {
  f <- file.path(folder, paste0(table, ".csv"))
  if (!file.exists(f)) stop("Missing file: ", f)
  read.csv(f, check.names = FALSE, stringsAsFactors = FALSE,
           na.strings = c("", "NA"))
}

find_hierarchy_folder <- function(root, scheme_pattern) {
  candidates <- unique(c(
    root,
    dirname(list.files(root, pattern = "Design[.]csv$", recursive = TRUE,
                       full.names = TRUE))
  ))
  for (d in candidates) {
    f <- file.path(d, "Design.csv")
    if (!file.exists(f)) next
    de <- read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
    if ("DEsamplingScheme" %in% names(de) &&
        any(grepl(scheme_pattern, de$DEsamplingScheme, ignore.case = TRUE))) {
      return(d)
    }
  }
  stop("Could not find hierarchy: ", scheme_pattern)
}

# H1: Vessel -> Fishing trip -> Fishing operation -> Species selection -> Sample
# H5: Onshore event -> Landing event -> Species selection
spec <- tibble::tribble(
  ~hierarchy, ~level, ~sampling_unit,       ~table,               ~method_col,
  "H1",       "PSU",  "Vessel (H1)",            "VesselSelection",     "VSselectionMethod",
  "H1",       "SSU",  "Fishing trip (H1)",      "FishingTrip",         "FTselectionMethod",
  "H1",       "3SU",  "Fishing operation (H1)", "FishingOperation",    "FOselectionMethod",
  "H1",       "4SU",  "Species selection (H1)", "SpeciesSelection",    "SSselectionMethod",
  "H1",       "5SU",  "Sample (H1)",            "Sample",              "SAselectionMethod",
  "H5",       "PSU",  "Onshore event (H5)",     "OnshoreEvent",        "OSselectionMethod",
  "H5",       "SSU",  "Landing event (H5)",     "LandingEvent",        "LEselectionMethod",
  "H5",       "3SU",  "Species selection (H5)", "SpeciesSelection",    "SSselectionMethod",
  "H5",       "4SU",  "Sample (H5)",            "Sample",              "SAselectionMethod"
)

folders <- c(
  H1 = find_hierarchy_folder(root, "OffshoreCatchSamplingProgramme"),
  H5 = find_hierarchy_folder(root, "OnshoreCatchSamplingProgramme")
)

extract_methods <- function(hierarchy, level, sampling_unit, table, method_col) {
  x <- read_rdbes(folders[[hierarchy]], table)
  if (!method_col %in% names(x)) stop("Missing column ", method_col, " in ", table)
  tibble(
    hierarchy = hierarchy,
    level = level,
    sampling_unit = sampling_unit,
    selection_method = trimws(as.character(x[[method_col]]))
  ) %>%
    filter(!is.na(selection_method), selection_method != "")
}

selection <- bind_rows(Map(
  extract_methods,
  spec$hierarchy, spec$level, spec$sampling_unit, spec$table, spec$method_col
)) %>%
  count(hierarchy, level, sampling_unit, selection_method, name = "n") %>%
  group_by(hierarchy, level, sampling_unit) %>%
  mutate(
    total_with_method = sum(n),
    proportion = n / total_with_method
  ) %>%
  ungroup()

# Facet order and x-axis order within each facet.
selection <- selection %>%
  mutate(
    sampling_unit_label = paste0(sampling_unit, " (", hierarchy, ")"),
    
    level = factor(
      level,
      levels = c("PSU", "SSU", "3SU", "4SU", "5SU")
    ),
    
    sampling_unit_label = factor(
      sampling_unit_label,
      levels = c(
        "Vessel (H1)",
        "Onshore event (H5)",
        "Fishing trip (H1)",
        "Landing event (H5)",
        "Fishing operation (H1)",
        "Species selection (H5)",
        "Species selection (H1)",
        "Sample (H5)",
        "Sample (H1)"
      )
    )
  )

p <- ggplot(selection,
            aes(x = sampling_unit, y = proportion, fill = selection_method)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.25) +
  facet_wrap(~level, nrow = 2, scales = "free_x", drop = TRUE) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.03))) +
  labs(
    x = "Sampling unit",
    y = "Proportion of records",
    fill = "Selection method",
    title = "Selection methods by RDBES sampling level",
    #subtitle = "H1 offshore and H5 onshore hierarchies, through the Sample (SA) table"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom"
  )

ggsave(file.path(out_dir, "H1_H5_all_levels_to_SA_selection_methods.png"),
       p, width = 11, height = 8.2, dpi = 300)

write.csv(selection,
          file.path(out_dir, "H1_H5_all_levels_to_SA_selection_method_proportions.csv"),
          row.names = FALSE)

print(selection)
print(p)

##plot by hierarchy
ggplot(selection, aes(sampling_unit, proportion, fill = selection_method)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~hierarchy, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
