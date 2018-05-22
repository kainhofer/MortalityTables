#' Plot multiple mortality tables (life tables) in one plot, relative to a given reference table
#'
#' \code{plotMortalityTableComparisons} prints multiple life tables (objects of child classes of \code{mortalityTable}) in one plot and scales each by the given reference table, so that the relative mortality can be easily seen. A legend is added showing the names of the tables.
#'
#' @inheritParams plotMortalityTables
#' @param reference The reference table that determines the 100\% values. If not given, the first argument of \code{data} is used as reference table.
#'
#' @examples
#' # Load the Austrian census data
#' mortalityTables.load("Austria_Census")
#'
#' # Compare some census tables with the mortality of 2011 Austrian males
#' # plot with the reference argument is the same as calling plotMortalityTableComparisons
#' plot(mort.AT.census.1869.male, mort.AT.census.1869.female,
#'      mort.AT.census.1971.male, mort.AT.census.1971.female,
#'      mort.AT.census.2011.male, mort.AT.census.2011.female,
#'      title = "Austrian Census tables, relative to 1971 males",
#'      reference = mort.AT.census.1971.male)
#' plotMortalityTableComparisons(mort.AT.census.1869.male, mort.AT.census.1869.female,
#'      mort.AT.census.1971.male, mort.AT.census.1971.female,
#'      mort.AT.census.2011.male, mort.AT.census.2011.female,
#'      title = "Austrian Census tables, relative to 1971 males",
#'      reference = mort.AT.census.1971.male)
#'
#' @import scales
#' @export
plotMortalityTableComparisons = function(
    data, ...,
    aes = NULL,
    ages = NULL,
    xlim = NULL, ylim = NULL,
    xlab = NULL, ylab = NULL,
    title = "",
    legend.position = c(0.9,0.1), legend.justification = c(1, 0),
    legend.title = "Sterbetafel",
    legend.key.width = unit(25, "mm"),
    reference = NULL)
{
    # If no reference mortality table is given, use the first table (data if its a mortality table)
    if (missing(reference)) {
        if (inherits(data, "mortalityTable")) {
            reference = data;
        } else {
            reference = NULL;# TODO;
        }
    }
    if (!is.data.frame(data)) {
        data = makeQxDataFrame(data, ..., reference = reference);
    }
    if (!is.null(ages)) {
        data = data[data$x %in% ages,]
    }
    if (missing(xlab)) xlab = "Alter";
    if (missing(ylab)) {
        ylab = substitute(paste("Sterbewahrscheinlichkeit  ", q[x],
                                " relativ zu ", refname),
                          env=list(refname=reference@name));
    }

    pl = ggplot(data, aes(x = x, y = y, color = group))
    if (!is.null(aes)) {
        pl = pl + aes
    }
    pl = pl +
        theme_bw() +
        theme(
            plot.title = element_text(size=18, face="bold"),
            legend.title = element_text(size=14, face="bold.italic"),
            # legend in bottom right corner of the plot
            legend.justification = legend.justification, legend.position=legend.position,
            # No box around legend entries
            legend.key = element_blank(),
            legend.key.width = legend.key.width,
            legend.background = element_rect(colour="gray50", linetype="solid")
        ) +
        geom_line() +
        coord_cartesian(xlim=xlim, ylim=ylim) +
        scale_y_continuous(
            name=ylab,
            labels=percent
            #            # breaks = scales::trans_breaks('log10', function(x) 10^x),
            #            # labels = scales::trans_format('log10', scales::math_format(10^.x))
            #            #minor_breaks = log(c(sapply(x, function(x) seq(0, x, x/10))), 10)
        ) +
        scale_x_continuous(
            name = xlab,
            #breaks = function (limits) scales::trans_breaks('', function(x) 10^x),
            # breaks = function (limits) seq(max(min(limits),0),max(limits),5),
            minor_breaks = function (limits) seq(max(round(min(limits)),0),round(max(limits)),1)#,
            #labels = scales::trans_format('log10', scales::math_format(10^.x))

        ) +
        # annotation_logticks(sides="lr") +
        xlab("Alter") + labs(colour = legend.title);
    if (title != "") {
        pl = pl + ggtitle(title);
    }
    pl
}

globalVariables(c("x", "y"))

