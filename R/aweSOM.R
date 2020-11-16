





aweSOM <- function() {
  shiny::runApp(system.file('shiny', package='aweSOM'))
}

################################################################################
################################################################################
## S3 methods

print.somQual <- function(x, ...) {
  cat("\n## Quality measures:\n", 
      "* Quantization error     : ", x$err.quant, "\n",
      "* (% explained variance) : ", x$err.varratio, "\n",
      "* Topographic error      : ", x$err.topo, "\n",
      "* Kaski-Lagus error      : ", x$err.kaski, "\n", 
      "\n## Number of obs. per map cell:")
  print(x$cellpop)
  
}

