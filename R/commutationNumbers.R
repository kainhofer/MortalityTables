#' @include mortalityTable.R pensionTable.R
NULL

#' Calculate the commutation numbers for the given parameters, using the mortality table and an interest rate
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters to be passed to the deathProbabilities call (e.g. YOB)
#' @param i Interest rate used for the calculation of the commutation numbers
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' commutationNumbers(AVOe2005R.male, i = 0.03, YOB = 1975)
#'
#' @exportMethod commutationNumbers
setGeneric("commutationNumbers", function(object, ..., i = 0.03) standardGeneric("commutationNumbers"));

#' @describeIn commutationNumbers Calculate the commutation numbers for the given
#'             parameters, using the mortality table and an interest rate
setMethod("commutationNumbers", "mortalityTable",
          function(object, ..., i = 0.03) {
              ages = ages(object, ...)
              qx = deathProbabilities(object, ...)
              commutationNumbers(qx, ages = ages, i = i)
          })


#' @describeIn commutationNumbers Calculate the commutation numbers for the given
#'             death probabilities (passed as a numeric vector with argument
#'             name "object"), ages and an interest rate
#'             Return value is a list of data frames
setMethod("commutationNumbers", "numeric",
          function(object, ages, i = 0.03) {
              v = 1 / (1 + i)
              qx = object
              lx = cumprod(c(100000, 1 - object[-length(object)]))
              dx = -diff(c(lx, 0))
              Dx = v^ages * lx
              Nx = rev(cumsum(rev(Dx))) # Nx is sum of Dx from x to omega
              Sx = rev(cumsum(rev(Nx))) # Sx is sum of Nx from x to omega

              Cx = qx * v * Dx
              Mx = rev(cumsum(rev(Cx)))
              Rx = rev(cumsum(rev(Mx)))

              data.frame(age = ages, qx, lx, dx, Dx, Nx, Sx, Cx, Mx, Rx)
          })

#' @describeIn commutationNumbers Calculate the commutation numbers for the given
#'             parameters, using the pension table and an interest rate
#'             Return value is a list of data frames
setMethod("commutationNumbers", "pensionTable",
          function(object, ..., i = 0.03) {
              probs = transitionProbabilities(object, ...)
              ages = probs$x
              list(
                  q  = commutationNumbers(probs$q + probs$i, ages = ages, i = i),
                  qi = commutationNumbers(probs$qi, ages = ages, i = i),
                  qp = commutationNumbers(probs$qp, ages = ages, i = i),
                  qw = commutationNumbers(probs$qw, ages = ages, i = i),
                  qg = commutationNumbers(probs$qg, ages = ages, i = i)
              )
          })


# commutationNumbers(deathProbabilities(AVOe2008P.male@qpx, YOB = 1982), ages(AVOe2008P.male@qpx), i = 0.06)
# commutationNumbers(AVOe2008P.male@qpx, i = 0.06, YOB = 1982) %>% View

# AVOe2008P.male.Comm = commutationNumbers(AVOe2008P.male, i = 0.06, YOB = 1982)
# AVOe2008P.male.Comm[["q"]] %>% View
# AVOe2008P.male.Comm[["qi"]] %>% View
# AVOe2008P.male.Comm[["qp"]] %>% View
# AVOe2008P.male.Comm[["qw"]] %>% View
# AVOe2008P.male.Comm[["qg"]] %>% View
