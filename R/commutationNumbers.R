#' @include mortalityTable.R pensionTable.R
NULL

#' Calculate the commutation numbers for the given parameters, using the mortality table and an interest rate
#'
#' @param object The life table object (class inherited from mortalityTable)
#' @param ... Other parameters to be passed to the deathProbabilities call (e.g. YOB)
#' @param ages Vector of ages for which the probabilities should be extracted and commutation numbers calculates
#' @param i Interest rate used for the calculation of the commutation numbers
#'
#' @examples
#' mortalityTables.load("Austria_Annuities")
#' commutationNumbers(AVOe2005R.male, i = 0.03, YOB = 1975)
#'
#' @exportMethod commutationNumbers
setGeneric("commutationNumbers", function(object, ..., ages = NULL, i = 0.03) standardGeneric("commutationNumbers"));

#' @describeIn commutationNumbers Calculate the commutation numbers for the given
#'             parameters, using the mortality table and an interest rate
setMethod("commutationNumbers", "mortalityTable",
          function(object, ..., ages = NULL, i = 0.03) {
              ages = if(is.null(ages)) ages(object, ...) else ages
              qx = deathProbabilities(object, ..., ages = ages)
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
          function(object, ..., ages = NULL, i = 0.03) {
              probs = transitionProbabilities(object, ..., ages = ages)
              ages = probs$x
              # Exit probabilities of actives are: - not dead or invalid & no transition to pension
              act.exit = (1 - probs$q - probs$i) * (1 - probs$ap)
              inv.exit = (1 - probs$qi) * (1 - probs$api)
              list(
                  q  = commutationNumbers(1 - act.exit, ages = ages, i = i),
                  qi = commutationNumbers(1 - inv.exit, ages = ages, i = i),
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
