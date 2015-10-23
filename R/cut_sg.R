#' cut edges
#'
#' @export

cut.sg <- function(g, x, R) {
  if(missing(R)) stop("Cutting length R must be given.")
  if(!is(g,"sg")) stop("g is not sg-object.")

  en <- cut_c(g$edges, sg_parse_coordinates(x), R)
  g$edges <- en
  g$note <- c(g$note, paste0("cut with R=",R))
  g
}
