MA1_Printer_fct <- function(Result, size_axis, size_title, C1, C2, C3, C4)
{
  
  
A1<-ggplot(Result, aes(x = theta, y = theta_hat_1)) + 
    xlab(TeX(r"($\theta$)")) + 
    ylab(TeX(r"($\hat{\theta_1}$)")) + 
    geom_smooth(aes(ymin = T1L,ymax = T1U), fill = C1, colour = C1, stat = "identity") + 
    geom_abline(slope = 1, intercept = 0) + 
    coord_fixed(xlim = c(-1,1),ylim = c(-1,1)) +
    theme(axis.text=element_text(size=size_axis),axis.title=element_text(size=size_title,face="bold")) +
    facet_grid()
    

   
A2<-ggplot(Result, aes(x = theta, y = theta_hat_2)) +
    xlab(TeX(r"($\theta$)")) +
    ylab(TeX(r"($\hat{\theta_2}$)")) +
    geom_smooth(aes(ymin = T2L,ymax = T2U), fill = C2, colour = C2, stat = "identity") +
    geom_abline(slope = 1, intercept = 0) +
    coord_fixed(xlim = c(-1,1),ylim = c(-1,1)) +
    theme(axis.text=element_text(size=size_axis),axis.title=element_text(size=size_title,face="bold"))
    


A3<-ggplot(Result, aes(x = theta, y = theta_hat_3)) +
    xlab(TeX(r"($\theta$)")) +
    ylab(TeX(r"($\hat{\theta_3}$)")) +
    geom_smooth(aes(ymin = T3L,ymax = T3U), fill = C3, colour = C3, stat = "identity") +
    geom_abline(slope = 1, intercept = 0) +
    coord_fixed(xlim = c(-1,1),ylim = c(-1,1)) +
    theme(axis.text=element_text(size=size_axis),axis.title=element_text(size=size_title,face="bold"))
    


A4<-ggplot(Result, aes(x = theta, y = theta_hat_4)) +
    xlab(TeX(r"($\theta$)")) +
    ylab(TeX(r"($\hat{\theta_4}$)")) +
    geom_smooth(aes(ymin = T4L,ymax = T4U), fill = C4, colour = C4, stat = "identity") +
    geom_abline(slope = 1, intercept = 0) +
    coord_fixed(xlim = c(-1,1),ylim = c(-1,1)) +
    theme(axis.text=element_text(size=size_axis),axis.title=element_text(size=size_title,face="bold"))
    




    # ggsave('MA_200_20_1.png', plot = A1, path = "C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/MA1_Estimation/images",width = 339, height = 316, units = "mm")
    # ggsave('MA_200_20_2.png', plot = A2, path = "C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/MA1_Estimation/images",width = 339, height = 316, units = "mm")
    # ggsave('MA_200_20_3.png', plot = A3, path = "C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/MA1_Estimation/images",width = 339, height = 316, units = "mm")
    # ggsave('MA_200_20_4.png', plot = A4, path = "C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/MA1_Estimation/images",width = 339, height = 316, units = "mm")
    # 
A5 <- plot_grid(A1, A2, A3, A4, align = "hv", ncol = 2, labels = "AUTO", label_size = 25)

  ggsave('MAFULL.png', plot = A5, path = "C:/Users/kspin/Documents/R Outputs/Projects/SDS_6/MA1_Estimation/images",width = 339, height = 316, units = "mm")

# print(list(A1,A2,A3,A4))

}