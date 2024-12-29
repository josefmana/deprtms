#
# This is a script used to describe the data
#

#
# PRINT ROUNDED NUMBER ----
rprint <- function(.x, .decimals = 2) sprintf( paste0("%.",.decimals,"f"), round(.x, .decimals) )

#
# GIVE CENTRAL TENDENCY ± VARIABILITY ----
cenvar <- function(.y, .dec = 2, cen = "mean", var = "sd", sep = " ± ") sapply(
  
  c(cen, var),
  function(fun) do.call(

    fun,
    list(.y, na.rm = T)

  ) %>% rprint(.decimals = .dec)
  
) %>% paste(collapse = sep)

#
# PREPARE A DESCRIPTIVE TABLE ----
description_table <- function(.data, include = 1, decs = 2) .data %>%
  
  filter(completed %in% include) %>%
  group_by(stim_type, scale, subscale, occasion) %>%
  summarise( N = sum( !is.na(score) ), Score = cenvar(score, .dec = decs) ) %>%
  ungroup() %>%
  pivot_wider(values_from = c("N","Score"), names_from = c("stim_type","occasion") ) %>%
  
  relocate(Score_HF_T0, .after = N_HF_T0) %>%
  relocate(Score_HF_T1, .after = N_HF_T1) %>%
  relocate(Score_HF_T2, .after = N_HF_T2) %>%
  relocate(Score_TBS_T0, .after = N_TBS_T0) %>%
  relocate(Score_TBS_T1, .after = N_TBS_T1)


# NEXT MAKE THE ORDER OF 'scale' & 'subscale' SENSIBLE
# (VIA ORDERING THE VARIABLES AS FACTORS),
# AND FORMAT THE TABLE VIA gt()
#