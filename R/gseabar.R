##' @title gseabar
##' @name gseabar
##' @param object GSEA enrich results.
##' @param color one of 'pvalue', 'p.adjust', 'qvalue', 'qvalues', 'pavl' or 'padj'
##' @param n number of categories to show
##' @param font.size font size
##' @param title plot title
##' @param reverse order
##' @param divide divide the label by group
##' @param length a numeric value sets wrap length, alternatively a
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
##' gseares <- readRDS(test_data)
##'
##' # bar plot for GSEA
##' gseabar(gseares,
##'        color='p.adjust',
##'        n=12,
##'        font.size=12,
##'        title="GSEA-barplot",
##'        length=40,
##'        reverse= FALSE,
##'        divide=FALSE)
##' }

# define function

  gseabar <- function(object,
                      color="p.adjust",
                      n=10,
                      font.size=12,
                      title="",
                      length=40,
                      reverse= FALSE,
                      divide = FALSE,...) {
    ## use *gsdata* to satisy barplot generic definition
    ## actually here is an gseaResult object.

    gsdata <- data.frame(object)

    colorBy <- match.arg(color, c("pvalue", "p.adjust", "qvalue", "qvalues","pval","padj"))
  #### add a new sign group by |NES|>0
  # gsdata$sign<-ifelse(gsdata$NES>0,"Activated","Suppressed")
      p <- gsdata %>%
        group_by(sign(NES)) %>%
        arrange(pvalue) %>%
        slice(1:n) %>%
        ggplot(aes(NES, fct_reorder(Description, NES)))+
        theme_bw(font.size)

### 是否更换排序方式
    if (reverse){
      p <- gsdata %>%
        group_by(sign(NES)) %>%
        arrange(p.adjust) %>%
        slice(1:n) %>%
        ggplot(aes(NES, fct_reorder(Description, desc(NES))))+
        theme_bw(font.size)
    }
### 是否分侧显示结果名称
    if (divide){
      plotdata<-gsdata %>%
        group_by(sign(NES)) %>%
        arrange(p.adjust) %>%
        slice(1:n)

      p<-p+theme(axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.border = element_blank())+
        geom_text(aes(x=ifelse(plotdata$NES>1,-0.05,0.05),label=plotdata$Description),
                  hjust=ifelse(plotdata$NES>1,1,0))
    }

### 定义文本长度
      label_func <- default_labeller(length)
      if(is.function(length)) {
        label_func <- length
      }

    p +
      scale_fill_continuous(low="#f87669", high="#2874C5",
                            guide=guide_colorbar(reverse=TRUE))+
      geom_col(aes_string(fill=colorBy)) + # geom_bar(stat = "identity") + coord_flip() +
      scale_y_discrete(labels = label_func) +
      ggtitle(title) + ylab(NULL) +  xlab("(Suppressed) ---NES--- (Activated)")+
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.background=element_roundrect(color="#808080", linetype=4))

  }

