# This funciton will take the appropriate subset of all non-gdx csv files and return a list of the appropriate csv's 
# based on user selection; assumes csvs have been loaded
select_cat_col_cat_ord <- function(cat.color = category.color, cat.order = category.order, st.data = state_data, 
                                   tech.data = technology, ts = timeslice, yr = years, tech_agg, 
                                   region_agg, years.inc = c(2017, 2047)){
  
  # Technologies
  col_select <- colnames(tech.data)[c(1, fmatch(tech_agg, c("gams.out", "comp", "simp", "userdef")))]
  tech_s <- tech.data[, ..col_select, ]
  # tech_s[[1]] <- paste0("^", tech_s[[1]], "$")
  setnames(tech_s, old = names(tech_s), new = c("pattern", "replacement")) 
  
  # States
  if(region_agg == "gams.out"){
    state_s <- data.table("pattern" = st.data[[1]], "replacement" = st.data[[1]])
  }else{
    col_select <- colnames(st.data)[c(1, fmatch(region_agg, c("gams.out", "st", "reg", "natl", "userdef")))]
    state_s <- st.data[, ..col_select, ]
    # state_s[[1]] <- paste0("^", state_s[[1]], "$")
    setnames(state_s, old = names(state_s), new = c("pattern", "replacement"))
  }
  
  # Colors for Technologies
  col_select <- colnames(cat.color)[c(fmatch(tech_agg, c("gams.out", "comp", "simp", "userdef")),
                                      fmatch(tech_agg, c("gams.out", "comp", "simp", "userdef")) + 3)]
  color_s <- cat.color[, ..col_select,]
  color_s <- unique(color_s)
  color_s <- setNames(object = color_s[[2]], nm = color_s[[1]])
  
  # Color for mapping
  col_select <- colnames(cat.color)[c(fmatch(tech_agg, c("gams.out", "comp", "simp", "userdef")), 8)]
  color_m <- cat.color[, ..col_select,]
  color_m <- unique(color_m)
  color_m <- setNames(object = color_m[[2]], nm = color_m[[1]])
  
  
  # Category Order for Technologies
  col_select <- colnames(cat.order)[fmatch(tech_agg, c("comp", "simp", "userdef"))]
  order_s <- cat.order[, ..col_select,]
  order_s <- order_s[!is.na(order_s[[1]]),,][[1]]
  
  # Years
  yrs_set <- yr[Year %fin% years.inc,,][[1]]
  
  # Timeslices
  ts_set <- ts[, Season_TOD := do.call(paste, .SD), .SDcols = c("season", "tod")]
  ts_set <- ts_set[,.SD, .SDcols = c("timeslice", "Season_TOD")]
  # ts_set[["timeslice"]] <- paste0("^",  ts_set[["timeslice"]], "$")
  setnames(ts_set, old = names(ts_set), new = c("pattern", "replacement"))
  
  ls_ret <- list("Tech.Re" = tech_s, "State.Re" = state_s, 
                 "Color.NV" = color_s, "Order.V" = order_s,
                 "Year.V" = yrs_set, "TimeSlice.Re" = ts_set, 
                 "Color.Map.NV" = color_m)
  return(ls_ret)
  
}

