# -------------------------------
# LOAD LIBRARIES
# -------------------------------
library(tidyverse)
library(lubridate)
library(stringr)

# -------------------------------
# LOAD AND PREP DATA
# -------------------------------
df <- read_csv("~/Documents/LASMaL/ML Models/All_Curated_Demog_Activity_Sleep5_min_N23_with_Activity_Pred_RF.csv")

df <- df %>%
  mutate(
    Time = suppressWarnings(ymd_hms(Time, quiet = TRUE)),
    Day_clean = str_extract(Day_label, "^[A-Za-z]+")
  ) %>%
  filter(!is.na(Day_label) & Day_label != "", !is.na(Time)) %>%
  mutate(
    Day_clean = ifelse(Day_clean %in% c("PR", "MD", "RD", "PS"), Day_clean, NA),
    Day = factor(Day_clean, levels = c("PR", "MD", "RD", "PS")),
    Activity = factor(Activity_Pred_RF, levels = c("Sedentary", "NonSedentary", "Driving")),
    ID = factor(ID)
  ) %>%
  filter(!is.na(HR_Normalized), !is.na(Speed), !is.na(Cadence), !is.na(Day))

# -------------------------------
# ACTIVITY COLORS
# -------------------------------
activity_colors <- c(
  "Sedentary" = "gray60",
  "Driving" = "deeppink3",
  "NonSedentary" = "darkgreen"
)


line_colors <- c("HR_Normalized" = "red", "Speed" = "blue", "Cadence" = "black")

# -------------------------------
# START PDF EXPORT
# -------------------------------
pdf("~/Documents/LASMaL/ML Models/Activity_Overview_Sedentary_vs_NonSedentary.pdf", width = 13, height = 8.5)

ids <- unique(df$ID)

for (pid in ids) {
  df_part <- df %>% filter(ID == pid)
  if (nrow(df_part) == 0) next
  
  min_max <- list(
    HR_Normalized = range(df_part$HR_Normalized, na.rm = TRUE),
    Speed = range(df_part$Speed, na.rm = TRUE),
    Cadence = range(df_part$Cadence, na.rm = TRUE)
  )
  
  layout_mat <- cbind(matrix(1:12, nrow = 4, byrow = TRUE), 13)
  layout(layout_mat, widths = c(1.3, 1.1, 1.1, 0.8), heights = rep(1, 4))
  par(mar = c(4, 6, 2.7, 3), oma = c(5, 4, 4, 4))
  
  day_names <- levels(df$Day)
  signals <- c("HR_Normalized", "Speed", "Cadence")
  titles <- c("HR_Normalized [BPM]", "Speed [km/h]", "Cadence [CPM]")
  
  for (rowi in seq_along(day_names)) {
    day <- day_names[rowi]
    df_day <- df_part %>% filter(Day == day)
    if (nrow(df_day) == 0 || all(is.na(df_day$Time))) {
      for (i in 1:3) plot.new()
      next
    }
    
    df_day <- df_day %>%
      arrange(Time) %>%
      mutate(TimeHour = as.numeric(difftime(Time, as.Date(Time), units = "hours")))
    
    rle_act <- rle(as.character(df_day$Activity))
    lengths <- rle_act$lengths
    values <- rle_act$values
    starts <- c(1, cumsum(lengths)[-length(lengths)] + 1)
    ends <- cumsum(lengths)
    
    shade_blocks <- function(ymin, ymax) {
      for (i in seq_along(starts)) {
        val <- values[i]
        if (is.na(val)) next
        col_val <- activity_colors[[val]]
        if (is.na(col_val)) next
        xleft <- df_day$TimeHour[starts[i]]
        xright <- if (i < length(ends)) df_day$TimeHour[starts[i + 1]] else df_day$TimeHour[ends[i]]
        if (is.na(xright) || xright < xleft) xright <- 24
        rect(xleft, ymin, xright, ymax,
             col = adjustcolor(col_val, alpha.f = 0.65),
             border = NA, xpd = FALSE)
      }
    }
    
    for (i in seq_along(signals)) {
      sig <- signals[i]
      y <- df_day[[sig]]
      x <- df_day$TimeHour
      
      if (i == 1) {
        par(mar = c(4, 6.7, ifelse(rowi == 1, 2.7, 1.7), 3))
      } else {
        par(mar = c(4, 2.7, ifelse(rowi == 1, 2.7, 1.7), 3))
      }
      
      if (all(is.na(y)) || all(is.na(x))) {
        plot.new()
      } else {
        main_title <- if (rowi == 1) titles[i] else ""
        plot(x, y, type = "n", lwd = 2,
             ylab = "", xlab = "Time [h]",
             main = main_title,
             cex.main = 1.2,
             xlim = c(0, 24),
             ylim = min_max[[sig]],
             xaxt = "n", yaxt = "n")
        shade_blocks(min_max[[sig]][1], min_max[[sig]][2])
        lines(x, y, col = line_colors[sig], lwd = 2)
        axis(1, at = seq(0, 24, by = 2),
             labels = c("00", sprintf("%02d", seq(2, 22, by = 2)), "00"),
             font.axis = 2)
        axis(2, las = 2, cex.axis = 1, font.axis = 2)
        if (i == 1) {
          mtext(day, side = 2, line = 6, at = mean(min_max[[sig]]), cex = 1.1, font = 2)
        }
      }
    }
  }
  
  # ---- LEGEND PANEL ----
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", legend = names(activity_colors),
         fill = adjustcolor(activity_colors, alpha.f = 0.7),
         cex = 1.2, bty = "n", title = "Activity Key")
  
  # ---- FOOTER SUMMARY ----
  df_summary <- df_part %>%
    group_by(Day) %>%
    summarise(
      MeanHR = round(mean(HR_Normalized, na.rm = TRUE), 1),
      MaxSpeed = round(max(Speed, na.rm = TRUE), 2),
      MeanCadence = round(mean(Cadence, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(match(Day, c("PR", "MD", "RD", "PS")))
  
  summary_text <- paste(
    paste0(df_summary$Day, ": Mean Normalized HR = ", df_summary$MeanHR,
           ", Mean Cadence = ", df_summary$MeanCadence,
           ", Max Speed = ", df_summary$MaxSpeed),
    collapse = "  ||  "
  )
  
  mtext(summary_text, side = 1, outer = TRUE, line = 2.5, cex = 0.52, font = 2)
  mtext(paste("Participant:", pid), outer = TRUE, cex = 1.2, font = 2, line = 1)
}

dev.off()
