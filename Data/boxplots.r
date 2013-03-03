local({
## Prepare
## Compute
## Print result
data_list <- rk.list (combined.data[["min_rect.center.y.6"]], combined.data[["min_rect.center.y.5"]], combined.data[["min_rect.center.y.4"]], combined.data[["min_rect.center.y.3"]], combined.data[["min_rect.center.y.2"]])		#convert single sample variables to list
rk.header ("Boxplot", list ("Variable(s)", paste (names (data_list), collapse=", ")))
rk.graph.on()
try (boxplot (data_list, notch = FALSE, outline = TRUE, horizontal = FALSE)) #actuall boxplot function
rk.graph.off ()
})
