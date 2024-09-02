
n_iter <- 10000

AD_trans_E <- AD_trans2 |>
  filter(redd_activity %in% c("deforestation", "forest degradation")) |>
  mutate(trans_id = paste0(lu_id_initial, "-", lu_id_final))

check <- nrow(AD_trans_E) == length(unique(AD_trans_E$trans_id))
message("unique number of transition: ", check)

n_trans <- nrow(AD_trans_E)

x = 2
U_all <- map(1:n_trans, function(x){
  
  redd_activity <- AD_trans_E$redd_activity[x]
  trans_id <- AD_trans_E$trans_id[x]
  
  print(trans_id)
  
  LU_init <- AD_trans_E$lu_id_initial[x] 
  LU_final <- AD_trans_E$lu_id_final[x]
  
  AD_mean <- AD_trans_E$trans_area[x]
  AD_se  <- AD_trans_E$trans_se[x]
  
  AGB_i_mean <- cstock |> filter(id == LU_init) |> pull(value)
  AGB_i_se   <- cstock |> filter(id == LU_init) |> pull(se)
  
  AGB_f_mean <- cstock |> filter(id == LU_final) |> pull(value)
  AGB_f_se   <- cstock |> filter(id == LU_final) |> pull(se)
  
  RS_mean <- cstock |> filter(id == "RS") |> pull(value)
  RS_se <- cstock |> filter(id == "RS") |> pull(se)
  
  CF_mean <- cstock |> filter(id == "CF") |> pull(value)
  CF_se <- cstock |> filter(id == "CF") |> pull(se)
  
  
  ## Sim
  set.seed(93)
  AD    <- rnorm(n = n_iter, mean = AD_mean   , sd = AD_se)
  AGB_i <- rnorm(n = n_iter, mean = AGB_i_mean, sd = AGB_i_se)
  AGB_f <- rnorm(n = n_iter, mean = AGB_f_mean, sd = AGB_f_se)
  RS    <- rnorm(n = n_iter, mean = RS_mean   , sd = RS_se)
  CF    <- rnorm(n = n_iter, mean = RS_mean   , sd = CF_se)
  
  sim <- cbind(AD, AGB_i, AGB_f, RS, CF) |> 
    as_tibble() |>
    mutate(
      E = AD * (AGB_i - AGB_f) * (1 + RS) * CF * 44/12,
      trans_id = trans_id,
      redd_activity = redd_activity,
      sim_no = 1:n_iter
    )
  
  # hist(sim$E)
  # E_median <- median(sim$E)
  # E_ci_lower <- median(sim$E) - quantile(sim$E, 0.1)
  # E_ci_upper <- quantile(sim$E, 0.9) - median(sim$E)
  # E_ci <- (E_ci_upper + E_ci_lower) / 2
  # E_ciperc <- round(E_ci / E_median * 100, 0)
  # paste0(round(E_median, 0), " +/- ", E_ciperc, "%")

  }) |> list_rbind()


U_redd <- U_all |>
  group_by(redd_activity, sim_no) |>
  summarise(E_redd = sum(E), .groups = "drop") |>
  mutate(E_redd_mean = E_redd / RP)
  
## Isolate Emissions from deforestation ####
U_DF <- U_redd |> filter(redd_activity == "deforestation")

hist(U_DF$E_redd_mean)
E_median <- median(U_DF$E_redd_mean)
E_ci_lower <- median(U_DF$E_redd_mean) - quantile(U_DF$E_redd_mean, 0.05)
E_ci_upper <- quantile(U_DF$E_redd_mean, 0.95) - median(U_DF$E_redd_mean)
E_ci <- (E_ci_upper + E_ci_lower) / 2
E_ciperc <- round(E_ci / E_median * 100, 0)
paste0(round(E_median, 0), " +/- ", E_ciperc, "%")

## Isolate emissions from degradation
U_DG <- U_redd |> filter(redd_activity == "forest degradation")

hist(U_DG$E_redd_mean)
E_median <- median(U_DG$E_redd_mean)
E_ci_lower <- median(U_DG$E_redd_mean) - quantile(U_DG$E_redd_mean, 0.05)
E_ci_upper <- quantile(U_DG$E_redd_mean, 0.95) - median(U_DG$E_redd_mean)
E_ci <- (E_ci_upper + E_ci_lower) / 2
E_ciperc <- round(E_ci / E_median * 100, 0)
paste0(round(E_median, 0), " +/- ", E_ciperc, "%")


## FREL with Uncertainty ####

U_FREL <- U_redd |>
  group_by(sim_no) |>
  summarise(E_FREL = sum(E_redd_mean))

hist(U_FREL$E_FREL)
E_median <- median(U_FREL$E_FREL)
E_ci_lower <- median(U_FREL$E_FREL) - quantile(U_FREL$E_FREL, 0.05)
E_ci_upper <- quantile(U_FREL$E_FREL, 0.95) - median(U_FREL$E_FREL)
E_ci <- (E_ci_upper + E_ci_lower) / 2
E_ciperc <- round(E_ci / E_median * 100, 0)
paste0(round(E_median, 0), " +/- ", E_ciperc, "%")


