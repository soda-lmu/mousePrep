# this file entails an example usage case of the whole package

# Comment: Here I used a simple R file, but with the example data we can use a RMarkdown instead

#load in the needed packages 
library(data.table)
library(devtools)
library(dplyr) 

devtools::install_github("soda-lmu/mousePrep")
library(mousePrep)

#load the data 
df <- fread("../complete_MCI_data.csv") # here we can add the example data from Felix 

#sample the data
df1 <- df[1:1000000, ]

#rename columns to facilitate mousePrep usage 
stand_df <- standardize_colnames(df1)

#convert every possible numeric column into numeric
stand_df <- convert_numeric(stand_df, exclude = "mt_id")

#remove specific participant based on their id
part_vector <- unique(stand_df$mt_id[1:100]) #select random cases here
stand_df2 <- rm_part(stand_df, rm_vector = part_vector)

# remove cases that are not needed
stand_df3 <- stand_df2 %>%
  rm_cases(column_rm = "userAgent_is_touch_capable",
           factor_rm = TRUE,
           criteria = 1) %>%
  rm_cases(column_rm = "type",
           factor_rm = "resize",
           criteria = 1) 

#remove cases where participants needed more than 7 minutes for a question
stand_df4 <- rm_cases_time(stand_df3, max_time = 7)

# create click and time df for include_clicks()
click_df <- sl_cases(stand_df4, column_sl = "type", factor_sl = "click")
time_df <- calculate_time_vars(stand_df4)

#inspect outliers based on move_time from the calculate_time_vars function on mt_id level 
time_data <- flag_outliers(time_df, flag_outliers = "threshold", outlier_threshold = 120000)

# select all the mousemove events and save them as a df
stand_df5 <- sl_cases(stand_df4, column_sl = "type", factor_sl = "mousemove")

#remove participants with multiple visits
stand_df6 <- multiple_traj(stand_df5, part_id = "mouseid", quest_id = "screen", criteria = "all")
stand_df6 <- stand_df6[stand_df6$multiple_workers == FALSE,]

#inspect cases with less than 50 recorded mouse movement data points per screen (flagged)
touch_df <- mouse_class_col(stand_df6, max_cutoff = 50)

#keep cases with more than 50 recorded mouse movement data points per screen
mouse_df <- mouse_class(stand_df6, max_cutoff = 50)

#calculate screen dimensions
screen_dims_data <- calculate_screen_dims(mouse_df)

#include clicks in an extra column 
click_df <- data.frame(mt_id = click_df$mt_id, timestamps = click_df$timestamps)
df_merged <- left_join(mouse_df, time_df, by = "mt_id")

df_with_clicks <- include_clicks(data_complete = df_merged, data_clicks = click_df)
df_with_clicks <- df_with_clicks %>% select(-clicks_char)

#export data
long_data <- mp_export_data(data = mouse_df, direction = "long")
processed_data <- mt_process_data(data = mouse_df,nsteps = 101,hover_threshold = 500)


# Function reference by task

## Data standardization

| Function | Purpose |
  |---|---|
  | `standardize_cols()` | Standardize names, types, and units |
  | `convert_numeric()` | Convert quantitative columns to numeric type |
  
  ## Case and event filtering
  
  | Function | Purpose |
  |---|---|
  | `rm_cases()` | Drop invalid participants, screens, or empty trajectories |
  | `rm_cases_time()` | Drop cases or participants based on time spent |
  | `rm_touch_devices()` | Remove touch-device observations |
  | `mouse_class()` | Drop cases based on number of movement points |
  | `mouse_class_col()` | Flag cases based on number of movement points |
  | `rm_non_mouse_events()` | Remove non-mouse movement events |
  | `rm_resize_events()` | Detect and remove resize events |
  
  ## Feature engineering
  
  | Function | Purpose |
  |---|---|
  | `screen_dims_calculate()` | Compute screen width and height |
  | `screen_dims_calculate_uas()` | UAS wrapper for screen dimensions |
  | `time_vars_calculate()` | Compute initiation time, response time, and move time |
  
  ## Outlier detection
  
  | Function | Purpose |
  |---|---|
  | `flag_outliers()` | Flag outliers using data-driven or threshold rules |
  | `rm_outliers()` | Remove observations flagged as outliers |
  
  ## Repeated visits and clicks
  
  | Function | Purpose |
  |---|---|
  | `multiple_traj()` | Flag or select first, last, or longest trajectory |
  | `include_clicks()` | Merge click timing and location with trajectories |
  
  ## Mousetrap processing and export
  
  | Function | Purpose |
  |---|---|
  | `mp_processing_mt()` | Process trajectories using `mousetrap` functions |
  | `mp_export_dataset()` | Export cleaned data in long or wide format |
  
  # Practical recommendations
  
  ## Inspect data after each major step
  
  Mouse-tracking preprocessing can remove many observations. It is good practice to check row counts and participant counts after each major step.

```{r audit-counts, eval = FALSE}
nrow(mouse_raw)
nrow(mouse_std)
nrow(mouse_clean)
nrow(mouse_features)
nrow(mouse_no_outliers)
```

```{r participant-counts, eval = FALSE}
mouse_no_outliers |>
  summarise(
    n_rows = n(),
    n_participants = n_distinct(mt_id)
  )
```

## Keep flagged datasets before deleting rows

Before removing outliers or invalid trajectories, save a flagged version.

```{r save-flagged, eval = FALSE}
write.csv(mouse_flagged, "data/processed/mouse_flagged_before_removal.csv", row.names = FALSE)
```

This makes the preprocessing workflow easier to audit and reproduce.

## Choose repeated-trajectory rules deliberately

The right rule depends on the research question.

- Use the first trajectory when initial exposure is most important.
- Use the last trajectory when submitted response behavior matters most.
- Use the longest trajectory when you want the most complete movement record.

## Use wide data for modeling and long data for trajectory analysis

Long data is often better for plotting and trajectory-level processing. Wide data is often easier to use for regression, classification, or participant-level analysis.

# Troubleshooting

## `mousetrap` interpolation error

If you see an error such as:
  
  ```text
need at least two non-NA values to interpolate
```

this usually means at least one trajectory has fewer than two valid timestamp-coordinate pairs. Before running `mp_processing_mt()`, check that each `mt_id` has enough valid observations.

```{r troubleshoot-interpolation, eval = FALSE}
mouse_final |>
  filter(!is.na(xpos), !is.na(ypos), !is.na(timestamps)) |>
  count(mt_id) |>
  filter(n < 2)
```

You can remove these trajectories before processing.

```{r remove-short-trajectories, eval = FALSE}
valid_ids <- mouse_final |>
  filter(!is.na(xpos), !is.na(ypos), !is.na(timestamps)) |>
  count(mt_id) |>
  filter(n >= 2) |>
  pull(mt_id)

mouse_final <- mouse_final |>
  filter(mt_id %in% valid_ids)
```

## Duplicate rows per trajectory

If you see a warning that more than one unique row remains per `mt_id` after removing trajectory data, check whether participant-level or screen-level columns vary within the same trajectory.

```{r troubleshoot-duplicates, eval = FALSE}
mouse_final |>
  group_by(mt_id) |>
  summarise(
    n_screen_id = n_distinct(screen_id),
    n_participant_id = n_distinct(participant_id),
    .groups = "drop"
  ) |>
  filter(n_screen_id > 1 | n_participant_id > 1)
```

# Suggested project folder structure

A simple project structure can make preprocessing reproducible.

```text
mouse-project/
  ├── data/
  │   ├── raw/
  │   └── processed/
  ├── scripts/
  │   └── 01_preprocess_mouse_data.R
├── outputs/
  │   ├── figures/
  │   └── tables/
  └── mouse-project.Rproj
```
