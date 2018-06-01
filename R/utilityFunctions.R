#' @include mortalityTable.R
NULL


fitExtrapolationLaw = function(data, ages, data.ages = ages, Dx = NULL, Ex = NULL, qx = NULL, method = "LF2", law = "HP", fit = 75:99, extrapolate = 80:120, fadeIn = 80:90, fadeOut = NULL, verbose = FALSE) {
    # Add the extrapolate ages to the needed ages
    neededAges = union(ages, extrapolate)
    # constrain the fit and fade-in range to given ages
    fit = intersect(ages, fit)
    if (!is.null(fadeIn))
        fadeIn = intersect(ages, fadeIn)
    if (!is.null(fadeOut))
        fadeOut = intersect(ages, fadeOut)

    # Hohe Alter: Fitte Heligman-Pollard im Bereich 75-99
    fitLaw = MortalityLaw(
        x = data.ages, Dx = Dx, Ex = Ex, qx = qx,
        law = law, opt.method = method,
        fit.this.x = fit)
    # summary(fitAP.m.75.99)
    # plot(fitAP.m.75.99)
    qPredict = predict(fitLaw, extrapolate)

    weights = rep(0, length(neededAges))
    names(weights) = neededAges

    if (!is.null(fadeIn)) {
        weights[neededAges < min(fadeIn)] = 0
        fadeInLen = length(fadeIn);
        weights[match(fadeIn, neededAges)] = (0:(fadeInLen - 1)) / (fadeInLen - 1)
        weights[neededAges > max(fadeIn)] = 1
    } else if (!is.null(fadeOut)) {
        weights[neededAges < min(fadeOut)] = 1
        fadeOutLen = length(fadeOut);
        weights[match(fadeOut, neededAges)] = ((fadeOutLen - 1):0) / (fadeOutLen - 1)
        weights[neededAges > max(fadeOut)] = 0
    }

    probs = fillAges(qPredict, givenAges = extrapolate, neededAges = neededAges, fill = 0) * weights +
        fillAges(data, givenAges = ages, neededAges = neededAges, fill = 0) * (1 - weights)

    if (verbose) {
        list(probs = probs, law = fitLaw, weights = weights)
    } else {
        probs
    }
}




# Fit an exponential function exp(-A*(x-x0)) to the last value (f(100) and f'(100) need to coincide):
fitExpExtrapolation = function(data, idx, up = TRUE, verbose = FALSE) {
    # browser()
    # Anchor point of the extrapolation
    f0 = data[[idx]]
    if (up) {
        A = -(data[[idx]] - data[[idx - 1]]) / f0
    } else {
        A = -(data[[idx + 1]] - data[[idx]]) / f0
    }
    x0 = idx + (log(f0) / A)
    fun.extra = function(x) exp(-A*(x - x0))
    if (up) {
        newdata = c(data[1:idx], sapply((idx + 1):length(data), fun.extra))
    } else {
        newdata = c(sapply(1:(idx - 1), fun.extra), data[idx:length(data)])
    }
    if (verbose) {
        list(data = newdata, A = A, x0 = x0, idx = idx)
    } else {
        newdata
    }
}


#' @export
mT.setName = function(table, name = table@name) {
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable.")

    table@name = name
    table
}



#' @export
mT.fillAges = function(table, neededAges, fill = 0) {
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable.")

    existingAges = ages(table)
    if (.hasSlot(table, "ages"))
        table@ages = neededAges
    if (.hasSlot(table, "deathProbs"))
        table@deathProbs = fillAges(table@deathProbs, givenAges = existingAges, neededAges = neededAges, fill = fill)
    if (.hasSlot(table, "exposures") && !is.null(table@exposures) && length(table@exposures) > 1)
        table@exposures = fillAges(table@exposures, givenAges = existingAges, neededAges = neededAges, fill = 0)
    if (.hasSlot(table, "trend") && !is.null(table@trend) && length(table@trend) > 1)
        table@trend = fillAges(table@trend, givenAges = existingAges, neededAges = neededAges, fill = 0)
    if (.hasSlot(table, "trend2") && !is.null(table@trend2) && length(table@trend2) > 1)
        table@trend2 = fillAges(table@trend2, givenAges = existingAges, neededAges = neededAges, fill = 0)
    if (.hasSlot(table, "loading") && !is.null(table@loading) && length(table@loading) > 1)
        table@loading = fillAges(table@loading, givenAges = existingAges, neededAges = neededAges, fill = 0)
    if (!is.null(table@data$deaths))
        table@data$deaths = fillAges(table@data$deaths, givenAges = existingAges, neededAges = neededAges, fill = 0)
    table
}

#' @export
mT.scaleProbs = function(table, factor = 1.0, name.postfix = "scaled", name = paste(table@name, name.postfix)) {
    table@deathProbs = factor * table@deathProbs
    if (!is.null(name)) {
        table@name = name
    }
    table
}


#' @export
mT.setTrend = function(table, trend, trendages = ages(table), baseYear = table@baseYear) {
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable.")

    t = mortalityTable.trendProjection(
        table,
        baseYear = baseYear,
        trend = trend[match(table@ages, trendages)]
    )
    t
}
#' @describeIn mT.setTrend Add a trend to the mortality table (returns a mortalityTable.trendProjection obect)
#' @export
mT.addTrend = mT.setTrend



#' @export
mT.extrapolateTrendExp = function(table, idx, up = TRUE) {
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable.")

    if (.hasSlot(table, "trend") && !is.null(table@trend))
        table@trend = fitExpExtrapolation(table@trend, idx = idx,up = up)
    if (.hasSlot(table, "tren2") && !is.null(table@trend2))
        table@trend2 = fitExpExtrapolation(table@trend2, idx = idx,up = up)
    table
}


#' @export
mT.translate = function(table, baseYear, name = table@name) {
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable.")

    table@deathProbs = periodDeathProbabilities(table, Period = baseYear)
    table@baseYear = baseYear
    table@name = name
    table
}


#' @export
mT.extrapolateProbsExp = function(table, age, up = TRUE) {
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable.")

    if (.hasSlot(table, "deathProbs")) {
        idx = match(age, ages(table))
        fit = fitExpExtrapolation(table@deathProbs, idx = idx, up = up, verbose = TRUE)
        table@deathProbs = fit$data
        table@data$extrapolationData = c(
            table@data$extrapolationData,
            list(list(law = "Exp", idx = idx, up = up, fit = fit)))
    }
    table
}


#' @export
mT.fitExtrapolationLaw = function(table, method = "LF2", law = "HP",
                                  fit = 75:99, extrapolate = 80:120,
                                  fadeIn = 80:90, fadeOut = NULL) {
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable.")

    ages = ages(table)
    if (!is.null(table@exposures) && !is.na(table@exposures)) {
        Ex = table@exposures
        qx = table@deathProbs
        # if (!is.null(table@data$deaths)) {
        #     Dx = table@data$deaths
        # } else {
        #     Dx = table@deathProbs * Ex
        # }
    } else {
        Ex = rep(1, length(ages))
        # Dx = table@deathProbs
        qx = table@deathProbs
    }
    table  = mT.fillAges(table, neededAges = union(ages, extrapolate), fill = 0)
    fitted = fitExtrapolationLaw(
        data = table@deathProbs, ages = ages(table),
        qx = qx, Ex = Ex, data.ages = ages,
        method = method, law = law,
        fit = fit, extrapolate = extrapolate,
        fadeIn = fadeIn, fadeOut = fadeOut,
        verbose = TRUE
    )
    # Store all fit parameters in the data slot of the mortality table
    table@data$extrapolationData = c(
        table@data$extrapolationData,
        list(list(law = law, method = method, fit = fit,
                  extrapolate = extrapolate, fadeIn = fadeIn, fadeOut = fadeOut,
                  fit = fitted)))
    table@deathProbs = fitted$probs

    table
}

#' @export
mT.setDimInfo = function(table, ..., append = TRUE) {
    if (!is(table, "mortalityTable"))
        stop("First argument must be a mortalityTable.")

    if (append) {
        table@data$dim[names(list(...))] = list(...)
    } else {
        table@data$dim = list(...)
    }
    table
}

