rebalance_areas <- function(df, cutoff, oth_col = "OthAgr") {
  df <- as.data.frame(df)

  # Identify columns
  id_col <- 1
  area_cols <- setdiff(names(df)[-id_col], oth_col)

  # Store original column sums
  original_sums <- colSums(df[, area_cols, drop = FALSE], na.rm = TRUE)

  # Step 1: Move small areas (< cutoff) to OthAgr
  for (col in area_cols) {
    move_vals <- df[[col]] < cutoff
    df[[oth_col]][move_vals] <- df[[oth_col]][move_vals] + df[[col]][move_vals]
    df[[col]][move_vals] <- 0
  }

  # Step 2: Rebalance attempt
  new_sums <- colSums(df[, area_cols, drop = FALSE], na.rm = TRUE)
  scale_factors <- original_sums / new_sums
  scale_factors[!is.finite(scale_factors)] <- 1  # avoid Inf/NaN

  # Apply row-wise
  for (i in seq_len(nrow(df))) {
    row_vals <- as.numeric(df[i, area_cols, drop = FALSE])
    oth_val  <- df[i, oth_col]
    row_total <- sum(row_vals) + oth_val

    # Fully scaled row
    scaled_vals <- row_vals * scale_factors
    correction_full <- sum(scaled_vals) - sum(row_vals)

    if (oth_val >= correction_full) {
      # Case 1: OthAgr can absorb full correction
      df[i, area_cols] <- scaled_vals
      df[i, oth_col]   <- oth_val - correction_full
    } else {
      # Case 2: Only partial scaling possible
      frac <- oth_val / correction_full  # how much of scaling we can afford
      partial_vals <- row_vals + (scaled_vals - row_vals) * frac
      df[i, area_cols] <- partial_vals
      df[i, oth_col]   <- 0
    }

    # Safety: enforce row sum exactly
    adj <- row_total - (sum(df[i, area_cols]) + df[i, oth_col])
    if (abs(adj) > 1e-10) {
      df[i, oth_col] <- df[i, oth_col] + adj
    }
  }

  return(df)
}


# df <- data.frame(
#   ID = 1:3,
#   Wheat = c(5, 0.5, 10),
#   Maize = c(2, 0.2, 8),
#   OthAgr = c(0.1, 9.3, 2)
# )
#
# rebalance_areas(df, cutoff = 1)

