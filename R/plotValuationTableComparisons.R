#' Plot multiple valuation tables (life tables) in one plot, relative to a given reference table
#'
#' \code{plotValuationTableComparisons} prints multiple life tables (objects of child classes of \code{valuationTable}) in one plot and scales each by the given reference table, so that the relative mortality can be easily seen. A legend is added showing the names of the tables.
#'
#' @inheritParams plotValuationTables
#' @param reference The reference table that determines the 100\% values. If not given, the first argument of \code{data} is used as reference table.
#'
#' @import scales
#' @export
plotValuationTableComparisons = function(
    data, ...,
    xlim = NULL, ylim = NULL,
    xlab = NULL, ylab = NULL,
    title = "",
    legend.position = c(0.9,0.1), legend.key.width = unit(25, "mm"),
    reference = NULL)
{
    # If no reference mortality table is given, use the first table (data if its a valuation table)
    if (missing(reference)) {
        if (inherits(data, "valuationTable")) {
            reference = data;
        } else {
            reference = NULL;# TODO;
        }
    }
    if (!is.data.frame(data)) {
        data = makeQxDataFrame(data, ..., reference=reference);
    }
    if (missing(xlab)) xlab = "Alter";
    if (missing(ylab)) {
        ylab = substitute(paste("Sterbewahrscheinlichkeit  ", q[x],
                                " relativ zu ", refname),
                          env=list(refname=reference@name));
    }

    pl = ggplot(data, aes(x = x, y = y, colour = data$group)) +
        theme_bw() +
        theme(
            plot.title = element_text(size=18, face="bold"),
            legend.title = element_text(size=14, face="bold.italic"),
            # legend in bottom right corner of the plot
            legend.justification=c(1,0), legend.position=legend.position,
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
            breaks = function (limits) seq(max(min(limits),0),max(limits),5),
            minor_breaks = function (limits) seq(max(round(min(limits)),0),round(max(limits)),1)#,
            #labels = scales::trans_format('log10', scales::math_format(10^.x))

        ) +
        # annotation_logticks(sides="lr") +
        xlab("Alter") + labs(colour="Sterbetafel");
    if (title != "") {
        pl = pl + ggtitle(title);
    }
    pl
}

globalVariables(c("x", "y"))

