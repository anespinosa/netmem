#' Lazega Law Firm
#'
#' The data is part of a study carried out in a Northeastern US corporate law firm,
#' referred to as SG&R, 1988-1991 in New England.
#' The data were collected by Emmanuel Lazega (2001).
#' This is a multiplex network of attorneys (patners and associates) of this firm.
#' It includes (among others) measurements of networks among the 71 attorneys (partners and associates) of this firm, i.e. their strong-coworker network, advice network, friendship network, and indirect control networks.
#'
#' @format Three 71 X 71 matrices.
#' \describe{
#'     \item{cowork}{"Because most firms like yours are also organized very
#'     informally, it is difficult to get a clear idea of how the members really
#'     work together. Think back over the past year, consider all the lawyers
#'     in your Firm. Would you go through this list and check the names of
#'     those with whom you have worked with.
#'     (By "worked with" I mean that you have spent time together on at least
#'     one case, that you have been assigned to the same case, that they
#'     read or used your work product or that you have read or used their
#'     work product; this includes professional work done within the
#'     Firm like Bar association work, administration, etc.)".}
#'     \item{advice}{"Think back over the past year, consider all the lawyers
#'     in your Firm. To whom did you go for basic professional advice?
#'     For instance, you want to make sure that you are handling a case right,
#'     making a proper decision, and you want to consult someone whose
#'     professional opinions are in general of great value to you.
#'     By advice I do not mean simply technical advice."}
#'     \item{friends}{"Would you go through this list, and check the names of
#'     those you socialize with outside work. You know their family,
#'     they know yours, for instance. I do not mean all the people you are
#'     simply on a friendly level with, or people you happen to meet at
#'     Firm functions."}
#'     \item{attributes:}{Attributes of the actors:}
#'
#'     \itemize{
#'     \item{seniority}{Seniority.}
#'     \item{status}{1=partner; 2=associate}
#'     \item{gender}{1=man; 2=woman}
#'     \item{office}{1=Boston; 2=Hartford; 3=Providence}
#'     \item{years}{years with the firm}
#'     \item{age}{age}
#'     \item{practice}{1=litigation; 2=corporate}
#'     \item{law school}{1 = harvard, yale; 2 = ucon; 3 = other}
#'     }
#' }
#'
#' @source Lazega, Emmanuel (2001) The Collegial Phenomenon: The Social Mechanisms of Cooperation Among Peers in a Corporate Law Partnership. Oxford University Press.
#'
#' @usage data(lazega_lawfirm)
#'
#' @docType data
#'
#'
"lazega_lawfirm"
