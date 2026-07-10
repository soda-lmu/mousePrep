## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5,
  warning = FALSE,
  message = FALSE
)


# For development
devtools::load_all("C:/Users/theer/OneDrive/Documents/mousepackage")

library(dplyr) 

## ----install, eval = FALSE----------------------------------------------------
# install.packages("devtools")
# devtools::install_github("soda-lmu/mousePrep")

## ----load-package, eval = FALSE-----------------------------------------------
# library(mousePrep)
# library(dplyr)

## ----workflow-image,out.width="100%",align = "center", echo=FALSE, fig.cap="mousePrep processing workflow"----
knitr::include_graphics("figures/MousePrep_white.png")

## ----read-data, eval = FALSE--------------------------------------------------
# raw_data <- read.csv("data/raw_mouse_data.csv")

## ----toy-data, echo=TRUE------------------------------------------------------
mouse_raw <- tibble::tibble(
  workerId = rep(c("p1", "p2"), each = 5),
  screenId = rep("q1", 10),
  timestamp = c(0, 100, 250, 400, 700, 0, 120, 260, 410, 800),
  xpos = c(10, 20, 35, 50, 80, 12, 24, 40, 55, 90),
  ypos = c(15, 25, 45, 60, 90, 18, 32, 50, 70, 95),
  eventType = rep("mousemove", 10),
  innerWidth = rep(1200, 10),
  innerHeight = rep(800, 10),
  scrollWidth = rep(1200, 10),
  scrollHeight = rep(1000, 10)
)

## ----standardize-colnames, echo=TRUE------------------------------------------
mouse_std <- standardize_colnames(mouse_raw, verbose = TRUE)
  

## ----convert-numeric, eval = FALSE--------------------------------------------
# mouse_std <- convert_numeric(mouse_std)

## ----rm-cases, echo=TRUE------------------------------------------------------
mouse_clean <- mouse_std %>%
  rm_cases(column_rm = "userAgent_is_touch_capable",
           factor_rm = TRUE,
           criteria = 1) %>%
  rm_cases(column_rm = "type",
           factor_rm = "resize",
           criteria = 1) 

## ----rm-touch-devices, eval = FALSE-------------------------------------------
# click_df <- sl_cases(stand_df4, column_sl = "type", factor_sl = "click")

## ----screen-dims, eval = FALSE------------------------------------------------
# screen_dims_data <- calculate_screen_dims(mouse_flag)

## ----time-vars, eval = FALSE--------------------------------------------------
# time_df <- calculate_time_vars(mouse_std)

## ----rm-cases-time, eval = FALSE----------------------------------------------
# mouse_clean_time <- rm_cases_time(mouse_clean, max_time = 7)

## ----mouse-class, eval = FALSE------------------------------------------------
# mouse_remove <- mouse_class_col(stand_df6, max_cutoff = 50)
# mouse_flag <- mouse_class(stand_df6, max_cutoff = 50)

## ----flag-outliers-data, eval = FALSE-----------------------------------------
# mouse_flagged <- flag_outliers(time_df,
#     flag_outliers = "data",
#     filter_var = "move_time"
#   )

## ----flag-outliers-threshold, eval = FALSE------------------------------------
# mouse_flagged <- flag_outliers(time_df,
#     flag_outliers = "threshold",
#     filter_var = "move_time")

## ----multiple-traj, eval = FALSE----------------------------------------------
# mouse_single_visit <- multiple_traj(mouse_clean)

## ----include-clicks, eval = FALSE---------------------------------------------
# mouse_with_clicks <- include_clicks(data_complete = df_merged, data_clicks = click_df)

## ----mousetrap-processing, eval = FALSE---------------------------------------
# mt_processed <- mp_processing_mt(mouse_clean,
#     nsteps = 101, hover_threshold = 500)

## ----export-long, eval = FALSE------------------------------------------------
# mouse_long <- mp_export_data(data = mt_data,direction = "long",long_join_data = participant_info,id = "mt_id")

## ----save-output, eval = FALSE------------------------------------------------
# write.csv(mouse_long, "data/processed/mouse_long.csv", row.names = FALSE)

