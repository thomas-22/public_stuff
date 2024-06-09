#install.packages("dplyr")
#install.packages("plotly")
#install if needed
library(dplyr)
library(plotly)

#Written by Thomas
#Enjoy

dsp_resource_vu_func <- function(n_start, VU_start, VU_end, cost_per_matrix){
  lvls <- seq(VU_start-1, VU_end)
  n_eff <- numeric(length(lvls))
  n_abs <- numeric(length(lvls))
  eff_n_result <- cbind(lvls, n_eff)
  abs_n_result <- cbind(lvls, n_abs)
  
  abs_n_result[1, 2] <- n_start
  eff_n_result[1, 2] <- abs_n_result[1, 2]/0.94^(VU_start-1)
  
  for (i in 2:length(n_eff)) {
    curr_lv <- abs_n_result[i, "lvls"]
    abs_n_result[i, 2] <- abs_n_result[[i-1, 2]]-(4000*(curr_lv - 5)*cost_per_matrix*(0.94^(curr_lv-1)))
    if (abs_n_result[i, 2] <= 0) {
      abs_n_result[i, 2] <- 0
      break
    }
    eff_n_result[i, 2] <- abs_n_result[i, 2]/(0.94^(curr_lv))
  }
  combined_result <- cbind(abs_n_result, eff_n_result[,2])
  colnames(combined_result) <- c("lvls", "n_abs", "n_eff")
  return(combined_result)
}
#returns vector for all values: absolute and effective spread over the VUs

dsp_resource_vu_func_single_val_VU_end_eff <- function(n_start, VU_start, VU_end, cost_per_matrix = 3.3){
  lvls <- seq(VU_start-1, VU_end)
  n_eff <- numeric(length(lvls))
  n_abs <- numeric(length(lvls))
  eff_n_result <- cbind(lvls, n_eff)
  abs_n_result <- cbind(lvls, n_abs)
  
  abs_n_result[1, 2] <- n_start
  eff_n_result[1, 2] <- abs_n_result[1, 2]/0.94^(VU_start-1)
  
  for (i in 2:length(n_eff)) {
    curr_lv <- abs_n_result[i, "lvls"]
    abs_n_result[i, 2] <- abs_n_result[[i-1, 2]]-(4000*(curr_lv - 5)*cost_per_matrix*(0.94^(curr_lv-1)))
    if (abs_n_result[i, 2] <= 0) {
      abs_n_result[i, 2] <- 0
      eff_n_result[i, 2] <- 0
      break
    }
    eff_n_result[i, 2] <- abs_n_result[i, 2]/(0.94^(curr_lv))
  }
  combined_result <- cbind(abs_n_result, eff_n_result[,2])
  colnames(combined_result) <- c("lvls", "n_abs", "n_eff")
  
  matching_rows <- combined_result[, "lvls"] == VU_end
  return(combined_result[matching_rows, "n_eff"])
}
dsp_resource_vu_func_single_val_VU_end_abs <- function(n_start, VU_start, VU_end, cost_per_matrix = 3.3){
  lvls <- seq(VU_start-1, VU_end)
  n_eff <- numeric(length(lvls))
  n_abs <- numeric(length(lvls))
  eff_n_result <- cbind(lvls, n_eff)
  abs_n_result <- cbind(lvls, n_abs)
  
  abs_n_result[1, 2] <- n_start
  eff_n_result[1, 2] <- abs_n_result[1, 2]/0.94^(VU_start-1)
  
  for (i in 2:length(n_eff)) {
    curr_lv <- abs_n_result[i, "lvls"]
    abs_n_result[i, 2] <- abs_n_result[[i-1, 2]]-(4000*(curr_lv - 5)*cost_per_matrix*(0.94^(curr_lv-1)))
    if (abs_n_result[i, 2] <= 0) {
      abs_n_result[i, 2] <- 0
      eff_n_result[i, 2] <- 0
      break
    }
    eff_n_result[i, 2] <- abs_n_result[i, 2]/(0.94^(curr_lv))
  }
  combined_result <- cbind(abs_n_result, eff_n_result[,2])
  colnames(combined_result) <- c("lvls", "n_abs", "n_eff")
  
  matching_rows <- combined_result[, "lvls"] == VU_end
  return(combined_result[matching_rows, "n_abs"])
}
#returns a single value for the effective n for the VU_End

# Example Usage:
# res <- dsp_resource_vu_func(1000000, 10, 100, 3.3)
# res_2 <- dsp_resource_vu_func_single_val_VU_end(1000000, 10, 100, 3.3) #get the 100th value
# res
# res_2
# res <- dsp_resource_vu_func(2000000, 20, 100, 3.3)

#Specify input parameters
n_start <- 2000000 #absolute resource range start
n_end <-   3000000 #absolute resource range end
VU_start <- 6 #VU lvl start (Keep it at >= 6)
VU_end <- 200  #VU lvl end
line_res <- 30 #resolution of the plot: "amount of situations to check"


#Calc Data
n_vals <- seq(n_start, n_end, by=(n_end - n_start)/line_res)
VU_values <- seq(VU_start, VU_end, by=1)
grid <- expand.grid(n_vals, VU_values)
effective_values <- mapply(dsp_resource_vu_func_single_val_VU_end_eff, grid$Var1, VU_start, grid$Var2)
absolute_values <- mapply(dsp_resource_vu_func_single_val_VU_end_abs, grid$Var1, VU_start, grid$Var2)

data <- cbind(grid, effective_values)

# Plotting
fig <- plot_ly(data, x = ~Var1, y = ~Var2, z = ~log10(effective_values), type = 'scatter3d', mode = 'markers', name = 'Effective n', marker = list(size = 1)) %>%
  add_markers(z = ~log10(absolute_values), name = 'Absolute n', marker = list(size = 1, color = 'red')) %>%  # Adding absolute values with a different color
  layout(scene = list(xaxis = list(title = 'Resource Amount Start', autorange = 'reversed'),
                      yaxis = list(title = paste('VU lvl starting at lvl: ', VU_start)),
                      zaxis = list(title = 'log_10(Resource Amount left) ', 
                                   range = c(min(log10(c(effective_values, absolute_values))), 
                                             max(log10(c(effective_values, absolute_values)))))))

# Show the plot
fig