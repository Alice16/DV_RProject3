myplot <- function(df, x) {
  names(df) <- c("x", "n")
  ggplot(df, aes(x=x, y=n)) + geom_point()
}
categoricals <- eval(parse(text=substring(getURL(URLencode('http://129.152.144.84:5001/rest/native/?query="select * from LEGISLATOR_ROLE"'), httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_yj2946', PASS='orcl_yj2946', MODE='native_mode', MODEL='model', returnFor = 'R', returnDimensions = 'True'), verbose = TRUE), 1, 2^31-1)))
l <- list()
for (i in names(dfR)) {
  if (i %in% categoricals[[1]]) {
    r <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select \\\""i"\\\", count(*) n from LEGISLATOR_ROLE group by \\\""i"\\\" "'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_yj2946', PASS='orcl_yj2946', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', i=i), verbose = TRUE)))
    p <- myplot(r,i)
    print(p) 
    l[[i]] <- p
  }
}
png("./../03_Visualizations/Categoricals.png", width = 25, height = 20, units = "in", res = 72)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 12)))   
print(l[[1]]+ggtitle('Legislator ID')+theme(plot.title=element_text(size=20, face="bold", vjust=2))+labs(x=paste("Legislator ID"),y=paste("Occurances"))+theme(axis.text.x=element_text(angle=90, size=5))+scale_x_discrete(breaks = c("TXL000513","TXL000212","TXL000284","TXL000329","TXL000439")), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:4))
print(l[[2]]+ggtitle('Type')+theme(plot.title=element_text(size=20, face="bold", vjust=2))+labs(x=paste("Member Type"),y=paste("Occurances")), vp = viewport(layout.pos.row = 1, layout.pos.col = 5:8))
print(l[[3]]+ggtitle('Chamber')+theme(plot.title=element_text(size=20, face="bold", vjust=2))+labs(x=paste("Chamber Type"),y=paste("Occurances")), vp = viewport(layout.pos.row = 1, layout.pos.col = 9:12))
print(l[[4]]+ggtitle('Party')+theme(plot.title=element_text(size=20, face="bold", vjust=2))+labs(x=paste("Party Affiliation"),y=paste("Occurances")), vp = viewport(layout.pos.row = 2, layout.pos.col = 1:4))
print(l[[5]]+ggtitle('Committee ID')+theme(plot.title=element_text(size=20, face="bold", vjust=2))+labs(x=paste("Committee ID"),y=paste("Occurances"))+theme(axis.text.x=element_text(angle=90, size=5))+scale_x_discrete(breaks = c("TXC000035","TXC000101","TXC000062","TXC000057","TXC000033")), vp = viewport(layout.pos.row = 2, layout.pos.col = 5:8))
print(l[[6]]+ggtitle('Committee')+theme(plot.title=element_text(size=20, face="bold", vjust=2))+labs(x=paste("Committee Name"),y=paste("Occurances"))+theme(axis.text.x=element_text(angle=90, size=5))+scale_x_discrete(breaks = c("Business & Industry","Technology","Energy Resources","Calendars","Insurance")), vp = viewport(layout.pos.row = 2, layout.pos.col = 9:12))
dev.off()

myplot1 <- function(df, x) {
  names(df) <- c("x")
  ggplot(df, aes(x=x)) + geom_histogram()
}
l1 <- list()
for (i in names(dfR)) {   
  if (i %in% categoricals[[2]]) {
    r1 <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select \\\""i"\\\" from LEGISLATOR_ROLE where \\\""i"\\\" is not null"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_yj2946', PASS='orcl_yj2946', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', i=i), verbose = TRUE)))
    p <- myplot1(r1,i)
    print(p) 
    l1[[i]] <- p
  }
}
png("./../03_Visualizations/Measures.png", width = 25, height = 10, units = "in", res = 72)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 12)))   
print(l1[[1]]+ggtitle('Term')+theme(plot.title=element_text(size=20, face="bold", vjust=2))+labs(x=paste("Term"),y=paste("Occurances")), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:4))
print(l1[[2]]+ggtitle('District')+theme(plot.title=element_text(size=20, face="bold", vjust=2))+labs(x=paste("District"),y=paste("Occurances")), vp = viewport(layout.pos.row = 1, layout.pos.col = 5:8))
print(l1[[3]]+ggtitle('Serial ID')+theme(plot.title=element_text(size=20, face="bold", vjust=2))+labs(x=paste("Serial ID"),y=paste("Occurances")), vp = viewport(layout.pos.row = 1, layout.pos.col = 9:12))
dev.off()
