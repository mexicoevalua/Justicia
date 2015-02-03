# Porcentaje de presos sin sentencia por fuero
# FUENTE: INEGI. Censo Nacional de Gobierno, Seguridad Pública y Sistema Penitenciario Estatales 2014.
require(rCharts)
require(ggplot2)
require(scales)
require(wordcloud)

# Cargar datos
data  <- read.csv("data/justicia.csv", as.is=T, encoding="utf8")

# Presos sin sentencia fuero común y fuero feeral

pdf("images/justicia2014.pdf", width=7.7, height=7.7)
textplot(data$fuero_comun,data$fuero_federal, data$abreviatura,xlim=c(min(data$fuero_comun),
  max(data$fuero_comun)), ylim=c(min(data$fuero_federal),max(data$fuero_federal)), show.lines=T, cex=1) + 
  title(main = "Porcentaje de reclusos en proceso (sin sentencia) \n  de los fueros común y federal en 2014", 
        xlab = "Porcentaje de presos sin sentencia fuero común", 
        ylab ="Porcentaje de presos sin sentencia fuero federal")
grid(NULL, lty =2, lwd =2)
dev.off()

# Fuero común
p  <- ggplot(data, aes(x=fuero_comun, y= reorder(abreviatura, fuero_comun))) + geom_point(size=3) 

mex_eval_theme  <- theme_bw() + theme(text = element_text(family="Helvetica"), 
                                      plot.title = element_text(face="bold",hjust=.5), panel.grid.major = element_line(size =.8),
                                      legend.position=c(1,0))

png("images/comun2014.png",  width=567, height=567)
p + ggtitle("Reclusos en proceso  (sin sentencia) del fuero común") + 
  xlab("Porcentaje de reclusos, fuero común") + ylab("Entidad") + mex_eval_theme
dev.off()



# Fuero federal
p  <- ggplot(data, aes(x=fuero_federal, y= reorder(abreviatura, fuero_federal))) + geom_point(size=3) 

mex_eval_theme  <- theme_bw() + theme(text = element_text(family="Helvetica"), 
                                      plot.title = element_text(face="bold",hjust=.5), panel.grid.major = element_line(size =.8),
                                      legend.position=c(1,0))

png("images/federal2014.png",  width=567, height=567)
p + ggtitle("Reclusos en proceso  (sin sentencia) del fuero federal") + 
  xlab("Porcentaje de reclusos, fuero federal") + ylab("Entidad") + mex_eval_theme
dev.off()

# Incidencia, prevalencia y percepción inseguridad (scatter plot)
names(data)

q  <- ggplot(data, aes(x = fuero_comun, y = fuero_federal)) + geom_text(aes(label=abreviatura)) +
  scale_y_continuous(labels=comma) + scale_x_continuous(labels=comma) +mex_eval_theme

q  <- q + scale_size(range=c(1,6)) + xlab("Fuero Común") + ylab("Fuero Federal")
q
# png
mex_eval_theme  <- theme_bw() + theme(text = element_text(family="Helvetica"), 
  plot.title = element_text(face="bold", hjust=.5), 
  panel.grid.major = element_line(size =.8),
  legend.position=c(1,0), legend.justification=c(1,0),
  legend.background=element_rect(fill="white",colour="black"))

q  <- ggplot(data, aes(x = fuero_comun, y = fuero_federal)) + 
  geom_text(aes(label=abreviatura)) +
  geom_abline(intercept=0, slope=1) +
  scale_y_continuous(lim=c(0,1), labels=comma) + 
  scale_x_continuous(lim=c(0,1), labels=comma) +mex_eval_theme
q <- q + xlab("Fuero Comun") + ylab("Fuero Federal") +  scale_size(range=c(1,6)) +
  ggtitle("Porcentaje de reclusos en proceso (sin sentencia) \n  de los fueros común y federal en 2014")
q
# Exportar imagen como PDF
png("images/sinsentencia.png", width=567, height=490)
q 
dev.off()

pdf("images/sinsentencia.pdf", width=6.35, height=5.34)
q 
dev.off()
