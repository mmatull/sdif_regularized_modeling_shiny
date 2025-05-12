# =====================================================================================================================================
# File: interval_utils.R
# =====================================================================================================================================
# Description:
# This script contains utility functions for working with numeric intervals:
# - parse_interval_boundaries: Extracts numeric boundaries from interval strings
# - sort_interval_strings: Sorts interval strings by their numeric boundaries
#
# Handles interval strings in formats like "(a,b]", "[c,d)", etc. and provides
# sorting capabilities based on the actual numeric values.
# 
# Author: mmatull
# Date: 2025-05-10
# =====================================================================================================================================


# =====================================================================================================================================
# Interval String Processing
# =====================================================================================================================================
#'
#' Parse and Sort Interval Strings by Numeric Boundaries
#'
#' Processes character representations of intervals (e.g., "(1,3]", "[2,4)") by:
#' 1. Extracting the numeric boundaries
#' 2. Sorting first by lower bound, then by upper bound
#'
#' @param interval_strings A character vector of interval strings in standard
#'        mathematical notation (e.g., "(a,b]", "[c,d)", "(e,f)", "[g,h]")
#'
#' @return A character vector of interval strings sorted by their numeric boundaries
#'
#' @examples
#' intervals <- c("(5,10]", "[1,3]", "(20,25]", "[15,18]", "(8,12]", "[2,4)")
#' sorted <- sort_interval_strings(intervals)
#' print(sorted)
#'
#' @export
sort_interval_strings <- function(interval_strings) {
  # Validate input
  if (!is.character(interval_strings)) {
    stop("Input must be a character vector of interval strings")
  }
  # Define missing values
  missing_start <- c("-1")
  missing_end <- c("9999", "Missing")
  
  # Split into missing and interval parts
  is_missing_start <- interval_strings %in% missing_start
  is_missing_end <- interval_strings %in% missing_end
  
  missing_part_start <- interval_strings[is_missing_start]
  missing_part_end <- interval_strings[is_missing_end]
  interval_part <- interval_strings[!is_missing_start & !is_missing_end]
  
  # Internal parser for interval boundaries
  extract_boundaries <- function(s) {
    # Remove all brackets/parentheses and whitespace
    clean <- gsub("[][()]", "",  s)
    clean <- gsub(" ", "", clean)
    # Split and convert to numeric
    parts <- as.numeric(strsplit(clean, ",")[[1]])
    if (length(parts) != 2) {
      stop(paste("Invalid interval format:", s))
    }
    parts
  }
  
  # Parse and sort intervals
  boundaries <- sapply(interval_part, extract_boundaries)
  sorted_order <- order(boundaries[1, ], boundaries[2, ])
  sorted_intervals <- interval_part[sorted_order]
  
  # Combine everything
  c(missing_part_start, sorted_intervals, missing_part_end)
}
