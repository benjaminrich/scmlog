#' @importFrom utils read.fwf
parse_scmlog <- function(rundir=getwd(), input=file.path(rundir, "scmlog.txt")) {

    dat <- readLines(input)

    dat <- gsub("  SIGNIFICANT", "SIGNIFICANT  ", dat)
    dat <- gsub(" INSIGNIFICANT", "INSIGNIFICANT ", dat)
    dat <- gsub(">", " ", dat)

    i <- grep("^MODEL", dat)
    j <- which(dat == "")


    x <- data.frame(start=sort(c(i,j)), end=c(sort(c(i,j))[-1], NA))
    x <- x[x$start %in% i,]

    #         1         2         3         4         5         6         7         8         9         0         1
    #12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
    #MODEL            TEST     BASE OFV     NEW OFV         TEST OFV (DROP)    GOAL     dDF  SIGNIFICANT   PVAL
    #CLALB-5          PVAL  19462.15533  19461.70189              0.45345  >   6.63490    1              0.500700
    #MODEL            TEST     BASE OFV     NEW OFV         TEST OFV (DROP)    GOAL     dDF INSIGNIFICANT  PVAL
    #CLSEX-1          PVAL  19363.37999  19376.52683            -13.14684  > -10.82800   -1              0.000288

    tbls <- lapply(1:nrow(x), function(i) {
     
        t1 <-read.fwf(textConnection(paste(dat[(x$start[i]):(x$end[i]-1)], collapse="\n")),
            skip=1, stringsAsFactors=F,
            widths=diff(c(0, 17, 23, 36, 55, 71, 83, 87,  100, 110)))

        t2 <-read.fwf(textConnection(paste(dat[(x$start[i]):(x$end[i]-1)], collapse="\n")),
            n=1, stringsAsFactors=F,
            widths=diff(c(0, 17, 23, 36, 55, 71, 83, 87, 100, 110)))

        t1 <- sapply(t1, trimws)
        t1

        t2 <- sapply(t2, trimws)
        t2

        colnames(t1) <- t2
        t1[is.na(t1)] <- ""
        t1 <- as.data.frame(t1)
        t1$GOAL <- paste0("> ", t1$GOAL)
        t1
    })

    return(tbls)
}

make_flextable <- function(x) {
    ft <- flextable::flextable(x)
    ft <- flextable::border_remove(ft)
    ft <- flextable::border_outer(ft, part="all",
        border=officer::fp_border(color="black", width = 1))
    ft <- flextable::border_inner_h(ft, part="all",
        border=officer::fp_border(color="black", width = 1))
    ft <- flextable::border_inner_v(ft, part="all",
        border=officer::fp_border(color="black", width = 1))
    ft <- flextable::font(ft, part="all", fontname="Times New Roman")
    ft <- flextable::fontsize(ft, part="all", size=9)
    ft <- flextable::align(ft, part="all", align="center")
    ft <- flextable::bg(ft, part="header", bg="#cccccc")
    ft <- flextable::bold(ft, part="header")
    ft <- flextable::autofit(ft)
    ft <- flextable::height_all(ft, part="all", height=0.183)
    ft <- flextable::fit_to_width(ft, max_width=9)
    return(ft)
}


#' Processes the scmlog.txt file and generates a Word document containing
#' tables for each forward inclusion and backwards elimination step.
#' @param rundir A directory containing the scmlog file (typically, the
#' directory that the scm command was run in).
#' @param input A file name for the input scm log file (typically, "scmlog.txt").
#' @param output A file name for the generated Word document.
#' @param template Optional. A Word document can be specified for use as a
#' template for the genrated output.
#' @param overwrite Should existing files be overwritten?
#' @return Called for its side effects.
#' @examples
#' \dontrun{
#' scmlog <- system.file("sample_scmlog.txt", package="scmlog")
#' process_scmlog(scmlog)  # Creates output in the current directory
#' }
#' @export
process_scmlog <- function(
    rundir    = getwd(),
    input     = file.path(rundir, "scmlog.txt"),
    output    = file.path(rundir, "scmlog.docx"),
    template  = NULL,
    overwrite = FALSE)
{

    if (is.null(template)) {
        template <- system.file("scmlog-template.docx", package="scmlog")
    }

    doc <- officer::read_docx(template) # new document

    if (is.character(input)) {
        tbls <- parse_scmlog(rundir=rundir, input=input)
    } else if (is.list(input)) {
        tbls <- input
    } else {
        stop("Invalid input.")
    }

    summtabback <- NULL
    summtabforw <- NULL
    for (i in 1:length(tbls)) {

        if (i > 1) {
            doc <- officer::body_add_break(doc, pos="after")
        }

        ft <- make_flextable(tbls[[i]])

        if (any(grepl("INSIGNIFICANT", names(tbls[[i]])))) {
            heading <- sprintf("Source of Variability \U{2014} Stepwise Covariate Analysis \U{2014} Backward Elimination \U{2014} Step %d", i)
            if (any(tbls[[i]]$INSIGNIFICANT == "YES!")) {
                row <-which.max(as.numeric(as.character(tbls[[i]]$PVAL))) 
                ft <- flextable::bold(ft, i=row)
                summtabback <- rbind(summtabback, tbls[[i]][row,])
            }
        } else {
            heading <- sprintf("Source of Variability \U{2014} Stepwise Covariate Analysis \U{2014} Forward Inclusion \U{2014} Step %d", i)
            if (any(tbls[[i]]$SIGNIFICANT == "YES!")) {
                row <-which.min(as.numeric(as.character(tbls[[i]]$PVAL))) 
                ft <- flextable::bold(ft, i=row)
                summtabforw <- rbind(summtabforw, tbls[[i]][row,])
            }
        }

        doc <- officer::body_add_par(doc, value=heading, style="heading 2")
        doc <- flextable::body_add_flextable(doc, ft, align="left")
    }

    if (file.exists(output)) {
        if (!isTRUE(overwrite)) {
            stop("File exists. Use overwrite = TRUE if you want to overwrite it.")
        } else {
            message("File exists. Overwriting it because overwrite = TRUE.")
        }
    }

    summtabforw$STEP <- sprintf("Forward step %s", 1:nrow(summtabforw))
    summtabback$STEP <- sprintf("Backward step %s", 1:nrow(summtabback))

    summtabforw$STEP <- sprintf("Forward step %s", 1:nrow(summtabforw))
    summtabback$STEP <- sprintf("Backward step %s", 1:nrow(summtabback))

    summtabforw$DIRECTION <- "F - Inclusion"
    summtabback$DIRECTION <- "B - Elimination"

    v <- c("STEP", "DIRECTION", "MODEL", "BASE OFV", "NEW OFV", "TEST OFV (DROP)", "GOAL", "dDF", "PVAL")
    summtab <- rbind(summtabforw[,v], summtabback[,v])
    summtab$STEP <- 1:nrow(summtab)

    if (nrow(summtab) > 0) {
        doc <- officer::body_add_break(doc, pos="after")
        ft <- make_flextable(summtab)
        doc <- officer::body_add_par(doc, value="Summary of Stepwise Covariate Analysis", style="heading 2")
        doc <- flextable::body_add_flextable(doc, ft, align="left")
    }

    print(doc, target=output)
}

