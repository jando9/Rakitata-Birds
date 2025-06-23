check_singularity <- function(model){
  tt <- getME(model, "theta")
  ll <- getME(model, "lower")
  if(min(tt[ll == 0]) < 10^-6){
    print(paste0("Singularity: Try removing random effects (", min(tt[ll == 0]), ")"))
  } else {
    print(paste0("Singularity: Okay (", min(tt[ll == 0]), ")"))
  }
}

update_model <- function(model){
  ss <- getME(model, c("theta", "fixef"))
  m2 <- update(model, start = ss, control = glmerControl(optCtrl = list(maxfun = 2e4)))
}
