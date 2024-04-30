\name{StreetscapeDataFrame-class}
\Rdversion{1.1}
\docType{class}
\alias{StreetscapeDataFrame-class}

\title{Class \code{"StreetscapeDataFrame"}}
\description{
"The output of strview_search family
functions is constructed in this data format -
A specialized data frame for streetscape package for
initializing the object with streetscape data and
extracting and decoding segmentation information
of streetscape dataframe."
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}

\author{
"Xiaohao Yang"
}

\examples{
showClass("StreetscapeDataFrame")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{data}:}{Object of class \code{data.frame} ~~ }
    \item{\code{epsg}:}{Object of class \code{numeric} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{download_data(path, items)}:}{ ~~ }
    \item{\code{get_mask(index)}:}{ ~~ }
    \item{\code{gvi()}:}{ ~~ }
    \item{\code{decodeDetection()}:}{ ~~ }
    \item{\code{mapPreview(maptype, fields)}:}{ ~~ }
    \item{\code{initialize(data, epsg)}:}{ ~~ }
  }
}
