#' calculate_RFM
#'
#  Description
#' Function that returns the highest PurchAmount for a given date.
#'
#  Detail arguments like a data description
#' @details
#' \code{data} contains the transaction data. The data set must contain a
#'             column labeled "Customer" that allows unique customer identification
#'             and a column labeled "TransDate", indicating the purchase date.
#'             The column "PurchAmount" specifies the total spending per purchase.
#'
#  Arguments that are passed as input to the function
#' @param x a data object with columns TransDate, Customer, PurchAmount, Cost
#' @param date a specification for the time of interest. Must be of format "dd.mm.yyy"
#'
#  Returned values with a description of what the function returns
#' @return The Customer with the highest profit for a given date \code{date} of data \code{x}
#'
#  Examples with a set of example R code on how to use the function
#' @examples
#' data(transactions)
#' myFun(transactions, date="01.10.2020")
#'
#  Import packages that are required for using your package
#' @import data.table
#  Careful: some packages have functions with overlapping names. If this is the case,
#           only import specific functions from a package. Here, lubridate and data.table
#           share the function quarter(). To avoid conflicts, only load functions you need
#           from lubridate with @importFrom package function1 function2
#' @importFrom lubridate dmy
#'
#  Include export to make sure roxygen2 knows to create the NAMESPACE file, to make
#  the package accessible to other users
#' @export

calculate_RFM <- function(data, weight_recency=1, weight_frequency=1, weight_monetary=1){

  # adjusting values to ensure that the weights add up to one
  weight_recency2 <- weight_recency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_frequency2 <- weight_frequency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_monetary2 <- weight_monetary/sum(weight_recency, weight_frequency, weight_monetary)

  print("weights are calculated")

  # RFM measures
  max.Date <- max(data[,TransDate])

  temp <- data[,list(
    recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = mean(PurchAmount)),
    by=Customer
  ]

  print("RFM Measure done")

  # RFM scores
  temp <- temp[,list(Customer,
                     recency = as.numeric(cut2(-recency, g=3)),
                     frequency = as.numeric(cut2(frequency, g=3)),
                     monetary = as.numeric(cut2(monetary, g=3)))]

  # Overall RFM score
  temp[,finalscore:=weight_recency2*recency+weight_frequency2*frequency+weight_monetary2*monetary]

  print("Overall RFM Measure done")

  # RFM group
  temp[,group:=round(finalscore)]

  # Return final table
  return(temp)
}
