##' @title gseabar2
##' @name gseabar2
##' @param object GSEA enrich results.
##' @param n number of categories to show
##' @param font.size font size
##' @param title plot title
##' @param length a numeric value sets wrap length, alternatively a
##' custom function to format axis labels.
##' by default wraps names longer that 40 characters
##' @param color manual colores for bar plot
##' @param reverse order
##' @param divide divide the label by group
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
##' gseabar2(gseares,
##'        n=12,
##'        font.size=12,
##'        title="GSEA-barplot",
##'        length=40,
##'        color = c("#f87669","#2874C5"),
##'        reverse= FALSE,
##'        divide=FALSE)
##' }


# define function

gseabar2 <- function(object,
                    n=12,
                    font.size=10,
                    title="",
                    length=40,
                    color = c("#f87669","#2874C5"),
                    reverse= FALSE,
                    divide=FALSE,...) {
  ## use *gsdata* to satisy barplot generic definition
  ## actually here is an gseaResult object.

  gsdata <- data.frame(object)
  #### add a new sign group by |NES|>0
  gsdata$sign<-ifelse(gsdata$NES>0,"Activated","Suppressed")
  #### add a new group which defined the -log10 pvalue negative or positive
  gsdata$pl = ifelse(gsdata$NES>0,-log10(gsdata$p.adjust),
                     log10(gsdata$p.adjust))

   p <- gsdata %>%
    group_by(sign) %>%
    arrange(p.adjust) %>%
    slice(1:n) %>%
    ggplot( aes(pl, fct_reorder(Description, pl),fill=sign))+theme_bw(font.size)
### 是否更换排序方式
   if (reverse) {
     p <- gsdata %>%
       group_by(sign) %>%
       arrange(p.adjust) %>%
       slice(1:n) %>%
       ggplot( aes(pl, fct_reorder(Description, desc(pl)),fill=sign))+theme_bw(font.size)
   }
### 是否分侧显示结果名称
   if (divide){
     plotdata<-gsdata %>%
       group_by(sign) %>%
       arrange(p.adjust) %>%
       slice(1:n)

     p<-p+theme(axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.border = element_blank())+
       geom_text(aes(x=ifelse(plotdata$NES>1,-0.1,0.1),label=plotdata$Description),
                 hjust=ifelse(plotdata$NES>1,1,0))
   }
### 定义文本长度
  label_func <- default_labeller(length)
  if(is.function(length)) {
    label_func <- length
  }

  p + geom_col(orientation='y') + # geom_bar(stat = "identity") + coord_flip() +
    scale_fill_manual(values=color)+
    scale_y_discrete(labels = label_func) +
    ggtitle(title) + ylab(NULL) +  xlab("-log10(pval)")+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.background=element_roundrect(color="#808080", linetype=4))
}
