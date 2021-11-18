save_png <- function(plot, file){
  DiagrammeRsvg::export_svg(plot) %>%
    charToRaw() %>%
    rsvg::rsvg() %>%
    png::writePNG(file)
  return(file)
}