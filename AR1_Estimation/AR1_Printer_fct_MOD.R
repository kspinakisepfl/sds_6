AR1_Printer_fct_MOD <- function(Result1, Result2, size_axis, size_title, C3, C4)
{
  
  A31<-ggplot(Result1, aes(x = phi, y = phi_hat_3)) +
    xlab(TeX(r"($\phi$)")) +
    ylab(TeX(r"($\hat{\phi_3}$)")) +
    geom_smooth(aes(ymin = T3L,ymax = T3U), fill = C3, colour = C3, stat = "identity") +
    geom_abline(slope = 1, intercept = 0) +
    coord_fixed(xlim = c(-1,1),ylim = c(-1,1)) +
    theme(axis.text=element_text(size=size_axis),axis.title=element_text(size=size_title,face="bold"))
  
  
  
  A41<-ggplot(Result1, aes(x = phi, y = phi_hat_4)) +
    xlab(TeX(r"($\phi$)")) +
    ylab(TeX(r"($\hat{\phi_4}$)")) +
    geom_smooth(aes(ymin = T4L,ymax = T4U), fill = C4, colour = C4, stat = "identity") +
    geom_abline(slope = 1, intercept = 0) +
    coord_fixed(xlim = c(-1,1),ylim = c(-1,1)) +
    theme(axis.text=element_text(size=size_axis),axis.title=element_text(size=size_title,face="bold"))
  
  A32<-ggplot(Result2, aes(x = phi, y = phi_hat_3)) +
    xlab(TeX(r"($\phi$)")) +
    ylab(TeX(r"($\hat{\phi_3}$)")) +
    geom_smooth(aes(ymin = T3L,ymax = T3U), fill = C3, colour = C3, stat = "identity") +
    geom_abline(slope = 1, intercept = 0) +
    coord_fixed(xlim = c(-1,1),ylim = c(-1,1)) +
    theme(axis.text=element_text(size=size_axis),axis.title=element_text(size=size_title,face="bold"))
  
  
  A42<-ggplot(Result2, aes(x = phi, y = phi_hat_4)) +
    xlab(TeX(r"($\phi$)")) +
    ylab(TeX(r"($\hat{\phi_4}$)")) +
    geom_smooth(aes(ymin = T4L,ymax = T4U), fill = C4, colour = C4, stat = "identity") +
    geom_abline(slope = 1, intercept = 0) +
    coord_fixed(xlim = c(-1,1),ylim = c(-1,1)) +
    theme(axis.text=element_text(size=size_axis),axis.title=element_text(size=size_title,face="bold"))
  
  A5 <- plot_grid(A31, A32, A41, A42, align = "hv", ncol = 2, labels = c("A1", "A2", "B1", "B2"), label_size = 25)
  
  ggsave('AR_MOD_FULL.png', plot = A5, path = "C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/AR1_Estimation/images",width = 339, height = 316, units = "mm")
  
  
  # print(list(A3,A4))
  
}