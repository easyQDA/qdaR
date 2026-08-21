#' qdaR: analyse qualitative coding exported from Zotero
#'
#' Reads the versioned exchange files written by the Zotero plugins zotQDA
#' and qdaZ, checks them against the contract they ship with, reproduces the
#' plugins' graphics with 'ggplot2', and adds the inferential statistics the
#' plugins deliberately leave out.
#'
#' The division of labour is intentional: zotQDA writes the data, qdaZ
#' describes it, and inferential claims are made here, where the person
#' making them has to choose the test.
#'
#' Start with [qda_read_fragments()] and [qda_example()].
#'
#' @keywords internal
"_PACKAGE"
