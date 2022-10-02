##' @title gseabar
##' @name gseabar
##' @param object GSEA enrich results.
##' @param color one of 'pvalue', 'p.adjust', 'qvalue', 'qvalues', 'pavl' or 'padj'
##' @param showCategory number of categories to show
##' @param font.size font size
##' @param title plot title
##' @param label_length a numeric value sets wrap length, alternatively a
##' custom function to format axis labels.
##' by default wraps names longer that 40 characters
##' @param ... other parameter, ignored

##' @import ggplot2
##' @import DOSE
##' @import stringr
##' @import dplyr
##' @import ggfun
##' @import forcats
##' @import RColorBrewer
##' @return ggplot2 object
##' @export
##'
##' @examples
##' \donttest{
##' # load data
##' test_data <- system.file("extdata", "gseares.RDS", package = "GSEAbar")
##'gseares <- readRDS(test_data)
##'
##' # bar plot for GSEA
##' gseabar(object,
##'        color='p.adjust',
##'        showCategory=12,
##'        font.size=12,
##'        title="",
##'        label_length=40)
##' }

# define function

  gseabar <- function(object,
                      color="p.adjust",
                      showCategory=12,
                      font.size=12,
                      title="",
                      label_length=40, ...) {
    ## use *gsdata* to satisy barplot generic definition
    ## actually here is an gseaResult object.

    gsdata <- data.frame(object)

    colorBy <- match.arg(color, c("pvalue", "p.adjust", "qvalue", "qvalues","pval","padj"))
  #### add a new sign group by |NES|>0
  ## gsdata$sign<-ifelse(gsdata$NES>0,"Activated","Suppressed")
      p <- gsdata %>%
        group_by(sign(NES)) %>%
        arrange(pvalue) %>%
        slice(1:showCategory) %>%
        ggplot(aes(NES, fct_reorder(Description, NES))) +
        theme_bw(font.size)+
        scale_fill_continuous(low="#f87669", high="#2874C5",
                              guide=guide_colorbar(reverse=TRUE))


    label_func <- default_labeller(label_length)
    if(is.function(label_length)) {
      label_func <- label_length
    }

    p + geom_col(aes_string(fill=colorBy)) + # geom_bar(stat = "identity") + coord_flip() +
      scale_y_discrete(labels = label_func) +
      ggtitle(title) + ylab(NULL) +  xlab("(Suppressed) ---NES--- (Activated)")+
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.background=element_roundrect(color="#808080", linetype=4))
  }

