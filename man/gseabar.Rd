% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gseabar.R
\name{gseabar}
\alias{gseabar}
\title{gseabar}
\usage{
gseabar(
  object,
  color = "p.adjust",
  n = 10,
  font.size = 12,
  title = "",
  length = 40,
  reverse = FALSE,
  divide = FALSE,
  ...
)
}
\arguments{
\item{object}{GSEA enrich results.}

\item{color}{one of 'pvalue', 'p.adjust', 'qvalue', 'qvalues', 'pavl' or 'padj'}

\item{n}{number of categories to show}

\item{font.size}{font size}

\item{title}{plot title}

\item{length}{a numeric value sets wrap length, alternatively a
custom function to format axis labels.
by default wraps names longer that 40 characters}

\item{reverse}{order}

\item{divide}{divide the label by group}

\item{...}{other parameter, ignored}
}
\value{
ggplot2 object
}
\description{
gseabar
}
\examples{
\donttest{
# load data
test_data <- system.file("extdata", "gseares.RDS", package = "GSEAbar")
gseares <- readRDS(test_data)

# bar plot for GSEA
gseabar(gseares,
       color='p.adjust',
       n=12,
       font.size=12,
       title="GSEA-barplot",
       length=40,
       reverse= FALSE,
       divide=FALSE)
}
}
