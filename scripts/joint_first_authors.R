# -------------------------------------
# Script: joint_first_authors.R
# Author: T. Eerola
# -------------------------------------

U <- unique(df$DOI)
print(length(U)) # 3383
length(unique(df$BIBTEXKEY)) # 3373!


sum(is.na(df$DOI))        # NAs don't count as unique but skew the count
sum(is.na(df$BIBTEXKEY))
sum(df$DOI == "", na.rm = TRUE)  # empty strings also common with DOIs

# 2. Find DOIs that appear with more than one BIBTEXKEY
df %>%
  distinct(BIBTEXKEY, DOI) %>%
  group_by(DOI) %>%
  filter(n() > 1) %>%
  arrange(DOI)

df %>%
  distinct(BIBTEXKEY, DOI) %>%
  group_by(BIBTEXKEY) %>%
  filter(n() > 1) %>%
  arrange(BIBTEXKEY)

write.csv(x = U, file = "data/unique_dois.csv", row.names = FALSE)

#### retrieve -----

library(openalexR)
library(dplyr)
library(stringr)

checkpoint_file <- "checkpoint_doi_results.rds"

if (file.exists(checkpoint_file)) {
  RESULTS <- readRDS(checkpoint_file)
  done <- !sapply(RESULTS, is.null)
  start_k <- if (all(done)) length(U) + 1L else which(!done)[1]
  cat(sprintf("Resuming from k = %d / %d\n", start_k, length(U)))
} else {
  RESULTS <- vector("list", length(U))
  names(RESULTS) <- U
  start_k <- 1L
  cat(sprintf("Starting fresh: %d DOIs to process\n", length(U)))
}

# ── Main loop ─────────────────────────────────────────────────────────────────
for (k in start_k:length(U)) {
  DOI <- U[k]
  cat(sprintf("[%d/%d] %s\n", k, length(U), DOI))

  m <- oa_fetch(
    entity  = "works",
    doi     = DOI,
    mailto  = "tuomas.eerola@durham.ac.uk",
    api_key = "yONbwjgCXk1eunyTjDJCcA",
    verbose = FALSE
  )

  RESULTS[[k]] <- m

  if (k %% 50 == 0 || k == length(U)) {
    saveRDS(RESULTS, checkpoint_file)
    cat(sprintf("  Checkpoint saved at k = %d\n", k))
  }
}
