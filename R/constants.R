#' Supported graphs constants
#'
#
# SG_SUPPORTED_GRAPHS<-c("geometric",
#                        "knn",
#                        "mass_geometric",
#                        "gabriel",
#                        "delaunay",
#                        "MST",
#                        "markcross",
#                        "SIG",
#                        "RST",
#                        "RNG",
#                        "CCC",
#                        "STIR",
#                        "bgeometric",
#                        "random")
#
# SG_GRAPH_PARS<-list(R="numeric>0",
#                     k="integer>0","",
#                     k="integer>=0",
#                     "",
#                     "",
#                     "",
#                     "",list(x0="numeric",y0="numeric",z0="numeric=0"),"",type0="factor",
#                     list(noise="numeric>0",alpha="numeric",beta="numeric>0",gamma="numeric>=0"), #STIR
#                     R="numeric>0", f="function")

SG_GRAPH_PARAMETERS <- list(geometric = list(R="numeric>0"),
                            knn = list(k="integer>0"),
                            mass_geometric=list(mass="numeric vector of sizes"),
                            markcross=list(mass="numeric vector of sizes"),
                            gabriel=list(),
                            MST=list(),
                            SIG=list(),
                            RST=list(center="coordinates of the center"),
                            RNG=list(),
                            CCC=list(types="factor vector of types")
                            )
