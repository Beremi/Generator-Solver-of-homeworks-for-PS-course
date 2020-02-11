library(readxl)
library(htmlTable)
library(magrittr)
library(moments)

source_data_dir = "../DU_LS20_prez/standard_dat_format/"
output_data_dir = "../reseni_LS20_prez/"
back_route_dir = "../solver/"

dir.create(output_data_dir)
system(paste0("cp styl.css ", output_data_dir, "styl.css"))

for (c_ukolu in 1:200) {
  data <-
    read_excel(paste0(source_data_dir, "ukol_", c_ukolu, ".xlsx"))
  dir.create(paste0(output_data_dir, "du_", c_ukolu, "_img"),
             showWarnings = FALSE)
  filename = paste0(output_data_dir, "du_", c_ukolu, "_reseni.html")
  fileConn <- file(filename)
  writeLines(
    c(
      "<!DOCTYPE html>",
      "<html lang=\"cs\">",
      "<head>",
      "<meta charset=\"utf-8\">",
      paste0("<title>Úkol ", c_ukolu, ".</title>"),
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"styl.css\">",
      "</head>",
      "<body>",
      "<center>"
    ),
    fileConn
  )
  close(fileConn)
  
  write(
    paste0("<strong><h1>Zadání ", c_ukolu, ".</h1></strong>"),
    file = filename,
    append = TRUE
  )
  ###########################ukol1#######################
  write("</center><h2>Úkol 1.</h2><center>",
        file = filename,
        append = TRUE)
  #---------------------------A--------------------------
  write("</center><h2>a)</h2><center>",
        file = filename,
        append = TRUE)
  
  rozdeleni = table(data$`typ abraziva`)
  rozdeleni[5] = sum(rozdeleni)
  rozdeleni = rbind(rozdeleni, round(rozdeleni / rozdeleni[5], digits = 5))
  rownames(rozdeleni) = c("absolutní èetnosti", "relativní èetnosti")
  rownames_html = htmlTable(rozdeleni,
                            col.columns = c("none", "#CCCCCC"),
                            css.cell = "padding-left: .7em; padding-right: .7em;")
  rownames_html
  write(
    "<h4>Tabulka 1: Zastoupení typù abraziv ve výbìru</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(rownames_html), file = filename, append = TRUE)
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/pie.jpg"),
    width = 700,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  pie(
    rozdeleni[1,-5],
    col = heat.colors(4),
    main = "Graf 1: Zastoupení testovaných typù abrasiv",
    labels = paste0(
      names(rozdeleni[1,-5]),
      "\n",
      rozdeleni[1,-5],
      "; ",
      round(rozdeleni[2,-5] * 100, digits = 1),
      "%"
    )
  )
  
  bp = barplot(
    rozdeleni[1,-5],
    col = heat.colors(4),
    main = "Graf 2: Zastoupení testovaných typù abrasiv",
    names.arg = paste(names(rozdeleni[1,-5]))
  )
  text(bp,
       rozdeleni[1,-5], paste0(round(rozdeleni[2,-5] * 100, digits = 1), "%"),
       pos = 1)
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/pie.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  #---------------------------B--------------------------
  write("</center><h2>b)</h2><center>",
        file = filename,
        append = TRUE)
  
  my_summary <- function(x)
    data.frame(
      rozsah = NROW(x),
      min = round(min(x), digits = 8),
      dolni_kvartil = round(quantile(x, 0.25), digits =
                              5),
      median = round(quantile(x, 0.5), digits =
                       5),
      mean = round(mean(x), digits = 5),
      horni_kvartil = round(quantile(x, 0.75), digits =
                              5),
      max = round(max(x), digits = 8),
      sd = round(sd(x), digits = 5),
      variacni_koeficient = round(sd(x) /
                                    mean(x) * 100, digits = 1),
      sikmost = round(skewness(x), digits =
                        5),
      spicatost = round(kurtosis(x) - 3, digits =
                          5),
      dolni_mez = round(quantile(x, 0.25) -
                          1.5 * (quantile(x, 0.75) - quantile(x, 0.25)), digits = 9),
      horni_mez = round(quantile(x, 0.75) +
                          1.5 * (quantile(x, 0.75) - quantile(x, 0.25)), digits = 9)
    )
  
  summary_stack = tapply(data$`automaticke mereni`, data$`typ abraziva`, my_summary)
  summary_matrix <- matrix(unlist(t(summary_stack)), ncol = 4)
  colnames(summary_matrix) = colnames(t(summary_stack))
  rownames(summary_matrix) = colnames(summary_stack[[1]])
  
  data_bezOP = data
  pom = boxplot(data$`automaticke mereni` ~ data$`typ abraziva`)
  data_bezOP$`automaticke mereni`[data_bezOP$`automaticke mereni` %in% pom$out] =
    NaN
  data_bezOP = na.omit(data_bezOP)
  
  summary_op_stack = tapply(data_bezOP$`automaticke mereni`,
                            data_bezOP$`typ abraziva`,
                            my_summary)
  summary_op_matrix <- matrix(unlist(t(summary_op_stack)), ncol = 4)
  colnames(summary_op_matrix) = colnames(t(summary_op_stack))
  rownames(summary_op_matrix) = colnames(summary_op_stack[[1]])
  
  summary_html = htmlTable(
    cbind(summary_matrix[, 1:2], summary_op_matrix[, 1:2]),
    cgroup = c("Vèetnì odlehlých pozorování", "Bez odlehlých pozorování"),
    n.cgroup = c(2, 2),
    col.columns = c("none", "#CCCCCC"),
    rgroup = c(
      "",
      "Míry polohy",
      "Míry variability",
      "Míry šikmosti a špièatosti",
      "Vnitøní hradby"
    ),
    n.rgroup = c(1, 6, 2, 2, 2),
    css.cell = "padding-left: .7em; padding-right: .7em;"
  )
  summary_html
  write(
    "<h4>Tabulka 2: Výbìrové charakteristiky pro mìøení automatickou metodou (abraziva A1 a A2)</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html), file = filename, append = TRUE)
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot.jpg"),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  boxplot(
    data$`automaticke mereni` ~ data$`typ abraziva`,
    main = "Graf 3: Velikost zrn abrazivní složky (vèetnì OP)",
    xlab = "Typ abraziva",
    ylab = "Velikost zrn [nm]",
    xlim = c(0.5, 2.5)
  )
  boxplot(
    data_bezOP$`automaticke mereni` ~ data_bezOP$`typ abraziva`,
    main = "Graf 4: Velikost zrn abrazivní složky (bez OP)",
    xlab = "Typ abraziva",
    ylab = "Velikost zrn [nm]",
    xlim = c(0.5, 2.5)
  )
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/histogramy.jpg"),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  hist(
    data_bezOP$`automaticke mereni`[data_bezOP$`typ abraziva` == "A1"],
    main = "Graf 5: Histogram velikosti zrn abrazivní složky A1 (bez OP)",
    xlab = "Velikost zrn [nm]",
    ylab = "Absolutní èetnosti",
    breaks = 20
  )
  hist(
    data_bezOP$`automaticke mereni`[data_bezOP$`typ abraziva` == "A2"],
    main = "Graf 6: Histogram velikosti zrn abrazivní složky A2 (bez OP)",
    xlab = "Velikost zrn [nm]",
    ylab = "Absolutní èetnosti",
    breaks = 20
  )
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/histogramy.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/qq_ploty.jpg"),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  qqnorm(
    data_bezOP$`automaticke mereni`[data_bezOP$`typ abraziva` == "A1"],
    main = "Graf 7: QQ graf velikosti zrn abrazivní složky A1",
    xlab = "Teoretické kvantily normálního rozdìlení",
    ylab = "Namìøené kvantily výbìru"
  )
  qqline(data_bezOP$`automaticke mereni`[data_bezOP$`typ abraziva` == "A1"])
  qqnorm(
    data_bezOP$`automaticke mereni`[data_bezOP$`typ abraziva` == "A2"],
    main = "Graf 8: QQ graf velikosti zrn abrazivní složky A2",
    xlab = "Teoretické kvantily normálního rozdìlení",
    ylab = "Namìøené kvantily výbìru"
  )
  qqline(data_bezOP$`automaticke mereni`[data_bezOP$`typ abraziva` == "A2"])
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/qq_ploty.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  A1_auto = data_bezOP$`automaticke mereni`[data_bezOP$`typ abraziva` == "A1"]
  A2_auto = data_bezOP$`automaticke mereni`[data_bezOP$`typ abraziva` == "A2"]
  
  write(
    "</center><br><br>Pro automatické mìøení a data bez odlehlých pozorování.<br>",
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "Pro abrazivum A1 je 2 sigma interval: ",
      round(mean(A1_auto) - 2 * sd(A1_auto), digits = 4),
      " nm - ",
      round(mean(A1_auto) + 2 * sd(A1_auto), digits = 4),
      " nm.<br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "Pro abrazivum A2 je 2 sigma interval: ",
      round(mean(A2_auto) - 2 * sd(A2_auto), digits = 4),
      " nm - ",
      round(mean(A2_auto) + 2 * sd(A2_auto), digits = 4),
      " nm.<br><center>"
    ),
    file = filename,
    append = TRUE
  )
  
  
  
  ###########################ukol2#######################
  write(
    "</center><br><br><br><br><br><br><br><br><br><br><br><h2>Úkol 2.</h2><center>",
    file = filename,
    append = TRUE
  )
  #---------------------------A--------------------------
  write("</center><h2>a)</h2><center>",
        file = filename,
        append = TRUE)
  write("</center>Pro data bez OP.<center><br>",
        file = filename,
        append = TRUE)
  write(
    paste0(
      "\n</center>\n",
      "Rozdíl støedních hodnot abraziva A1 a A2 je ",
      round(mean(A1_auto) - mean(A2_auto), digits = 4),
      " nm, rozdíl mediánù je ",
      round(quantile(A1_auto, 0.5) - quantile(A2_auto, 0.5), digits = 4),
      " nm.<br>\n<center>\n"
    ),
    file = filename,
    append = TRUE
  )
  
  
  #---------------------------B--------------------------
  write("</center><h2>b)</h2><center>",
        file = filename,
        append = TRUE)
  write("</center>Pro data bez OP.<center><br>",
        file = filename,
        append = TRUE)
  write(
    "</center>Jedná se o jednovýbìrové testy, mám na výbìr <strong>t-test, Wilcoxonùv test nebo mediánový test</strong>.<center><br>",
    file = filename,
    append = TRUE
  )
  write(
    "</center>Nejprve ovìøím <strong>normalitu</strong> u obou typù abraziv.<center><br>",
    file = filename,
    append = TRUE
  )
  
  A1_SW = shapiro.test(A1_auto)
  A1_SW$p.value
  if (A1_SW$p.value >= 0.0005) {
    t_p = paste0("= ", round(A1_SW$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0(
      "</center>Shapiro-Wilk test normality pro typ A1 vyšel s p-hodnotou ",
      t_p,
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  A2_SW = shapiro.test(A2_auto)
  A2_SW$p.value
  if (A2_SW$p.value >= 0.0005) {
    t_p = paste0("= ", round(A2_SW$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0(
      "</center>Shapiro-Wilk test normality pro typ A2 vyšel s p-hodnotou ",
      t_p,
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>Šikmost A1 je ",
      round(skewness(A1_auto), digits = 2),
      ", šikmost A2 je ",
      round(skewness(A2_auto), digits = 2),
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  
  
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/kvantil_A1A2.jpg"),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  
  # x = quantile(A1_auto, seq(0.005, 0.5, 0.005))
  # y = quantile(A1_auto, 1 - seq(0.005, 0.5, 0.005))
  # mm = quantile(A1_auto, 0.5)
  # yy = y - mm
  # xx = mm - x
  # plot(
  #   c(0, max(max(xx), max(yy))),
  #   c(0, max(max(xx), max(yy))),
  #   type = 'l',
  #   main = "Graf 9: Srovnání kvantilù A1 (sym. rozdìlení leží na lince)",
  #   xlab = "Medián - kvantily (0,0.5)",
  #   ylab = "Kvantily (0.5,1) - medián"
  # )
  # points(xx, yy)
  #
  # x = quantile(A2_auto, seq(0.005, 0.5, 0.005))
  # y = quantile(A2_auto, 1 - seq(0.005, 0.5, 0.005))
  # mm = quantile(A2_auto, 0.5)
  # yy = y - mm
  # xx = mm - x
  # plot(
  #   c(0, max(max(xx), max(yy))),
  #   c(0, max(max(xx), max(yy))),
  #   type = 'l',
  #   main = "Graf 10: Srovnání kvantilù A2 (sym. rozdìlení leží na lince)",
  #   xlab = "Medián - kvantily (0,0.5)",
  #   ylab = "Kvantily (0.5,1) - medián"
  # )
  # points(xx, yy)
  
  x = quantile(A1_auto, seq(0.01, 0.5 - 0.01, 0.01))
  mm = quantile(A1_auto, 0.5)
  plot(
    c(0, 0.5),
    c(0, 0.5),
    type = 'l',
    main = "Graf 9: Srovnání kvantilù A1 (sym. rozdìlení leží na lince)",
    xlab = "Kvantily (0,0.5)",
    ylab = "1 - odpovídající kvantily (0.5,1)"
  )
  tmp = ecdf(A1_auto)(mm + mm - x)
  points(seq(0.01, 0.5 - 0.01, 0.01), 1 - tmp)
  
  x = quantile(A2_auto, seq(0.01, 0.5 - 0.01, 0.01))
  mm = quantile(A2_auto, 0.5)
  plot(
    c(0, 0.5),
    c(0, 0.5),
    type = 'l',
    main = "Graf 10: Srovnání kvantilù A2 (sym. rozdìlení leží na lince)",
    xlab = "Kvantily (0,0.5)",
    ylab = "1 - odpovídající kvantily (0.5,1)"
  )
  tmp = ecdf(A2_auto)(mm + mm - x)
  points(seq(0.01, 0.5 - 0.01, 0.01), 1 - tmp)
  
  
  
  dev.off()
  write(
    paste0(
      "</center>Poznámka ke Grafùm 9,10, jedná se o vykreslení dvojic kvantilù (q,1-q) (napø.: X0.2 a X0.8). Vykreslení je provedeno následovnì: napoètou se vybrané kvantily z intervalu (0,0.5), pro každý takovýto kvantil se napoèítá ideální symetrická hodnota vzhledem k mediánu (medián+(medián-kvantil)) a pomocí \"ecdf\" se spoète cdf tohoto symetrického kvantilu, do grafu se pak zakreslí 1- tato hodnota. Èím je rozdìlení blíže symetrickému rozdìlení, tím více bude okolo identity (nikdy však mimo ètverec <0,0.5>x<0,0.5>).",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/kvantil_A1A2.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  
  
  
  write(
    paste0(
      "</center>Test symetrie (balíèek \"lawstat\", funkce \"symmetry.test(data,boot=FALSE)\"). ",
      "Symmetry test by Miao, Gel, and Gastwirth (2006).",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  
  library(lawstat)
  
  A1_symtest = symmetry.test(A1_auto, boot = FALSE)
  if (A1_symtest$p.value >= 0.0005) {
    t_p = paste0("= ", round(A1_symtest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>Test symetrie A1, p-hodnota ", t_p,
           "<center><br>"),
    file = filename,
    append = TRUE
  )
  
  A2_symtest = symmetry.test(A2_auto, boot = FALSE)
  if (A2_symtest$p.value >= 0.0005) {
    t_p = paste0("= ", round(A2_symtest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>Test symetrie A2, p-hodnota ", t_p,
           "<center><br>"),
    file = filename,
    append = TRUE
  )
  
  
  
  
  
  
  A1_ttest = t.test(A1_auto)
  A1_ttest$conf.int
  A2_ttest = t.test(A2_auto)
  A2_ttest$conf.int
  A1_wtest = wilcox.test(A1_auto, conf.int = TRUE)
  A1_wtest$conf.int
  A2_wtest = wilcox.test(A2_auto, conf.int = TRUE)
  A2_wtest$conf.int
  
  library(signmedian.test)
  A1_mtest = signmedian.test(
    A1_auto,
    mu = 0,
    conf.level = 0.95,
    alternative = "two.sided",
    conf.int = TRUE
  )
  A1_mtest$conf.int
  A2_mtest = signmedian.test(
    A2_auto,
    mu = 0,
    conf.level = 0.95,
    alternative = "two.sided",
    conf.int = TRUE
  )
  A2_mtest$conf.int
  write(
    "</center><h4>Odhad støední hodnoty dle t-testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>",
      "A1: Bodový odhad = ",
      round(mean(A1_auto), digits = 5),
      ", intervalový odhad=<",
      round(A1_ttest$conf.int[1], digits = 5),
      ", ",
      round(A1_ttest$conf.int[2], digits = 5),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>",
      "A2: Bodový odhad = ",
      round(mean(A2_auto), digits = 5),
      ", intervalový odhad=<",
      round(A2_ttest$conf.int[1], digits = 5),
      ", ",
      round(A2_ttest$conf.int[2], digits = 5),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    "</center><h4>Odhad mediánu dle Wilcoxonova testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>",
      "A1: Bodový odhad = ",
      round(quantile(A1_auto, 0.5), digits = 5),
      ", intervalový odhad=<",
      round(A1_wtest$conf.int[1], digits = 5),
      ", ",
      round(A1_wtest$conf.int[2], digits = 5),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>",
      "A2: Bodový odhad = ",
      round(quantile(A2_auto, 0.5), digits = 5),
      ", intervalový odhad=<",
      round(A2_wtest$conf.int[1], digits = 5),
      ", ",
      round(A2_wtest$conf.int[2], digits = 5),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    "</center><h4>Odhad mediánu dle mediánového testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>",
      "A1: Bodový odhad = ",
      round(quantile(A1_auto, 0.5), digits = 5),
      ", intervalový odhad=<",
      round(A1_mtest$conf.int[1], digits = 5),
      ", ",
      round(A1_mtest$conf.int[2], digits = 5),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>",
      "A2: Bodový odhad = ",
      round(quantile(A2_auto, 0.5), digits = 5),
      ", intervalový odhad=<",
      round(A2_mtest$conf.int[1], digits = 5),
      ", ",
      round(A2_mtest$conf.int[2], digits = 5),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  
  
  #---------------------------C--------------------------
  write("</center><h2>c)</h2><center>",
        file = filename,
        append = TRUE)
  
  write("</center>Pro data bez OP.<center><br>",
        file = filename,
        append = TRUE)
  write(
    "</center>Jedná se o dvouvýbìrové testy, mám na výbìr <strong>t-test, Aspinové-Welshùv test nebo Mannùv-Whitneyùv test</strong>.<center><br>",
    file = filename,
    append = TRUE
  )
  write(
    "</center>Normalita viz výše. Ovìøíme shodu rozptylù pomocí F-testu.<center><br>",
    file = filename,
    append = TRUE
  )
  f_test = var.test(A1_auto, A2_auto, conf.level = 0.95)
  
  if (f_test$p.value >= 0.0005) {
    t_p = paste0("= ", round(f_test$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  
  write(
    paste0(
      "</center>Exploraènì: pomìr rozptylù = ",
      round(max(sd(A1_auto) ^ 2, sd(A2_auto) ^ 2) / min(sd(A1_auto) ^ 2, sd(A2_auto) ^
                                                          2), digits = 3),
      ".<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0("</center>Pro F-test vyšla p-hodnota ", t_p, ".<center><br>"),
    file = filename,
    append = TRUE
  )
  
  
  
  A1A2_ttest = t.test(A1_auto, A2_auto, var.equal = TRUE)
  A1A2_AWtest = t.test(A1_auto, A2_auto)
  A1A2_MWtest = wilcox.test(A1_auto, A2_auto, conf.int = TRUE)
  
  
  
  write(
    "</center><h4>Odhad rozdílu støedních hodnot (A1-A2) dle t-testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  
  
  write(
    paste0(
      "</center>",
      "A1-A2: Bodový odhad = ",
      round(mean(A1_auto) - mean(A2_auto), digits = 5),
      ", intervalový odhad=<",
      round(A1A2_ttest$conf.int[1], digits = 6),
      ", ",
      round(A1A2_ttest$conf.int[2], digits = 6),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    "</center><h4>Odhad rozdílu støedních hodnot (A1-A2) dle Aspinové-Welshova testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>",
      "A1-A2: Bodový odhad = ",
      round(mean(A1_auto) - mean(A2_auto), digits = 5),
      ", intervalový odhad=<",
      round(A1A2_AWtest$conf.int[1], digits = 6),
      ", ",
      round(A1A2_AWtest$conf.int[2], digits = 6),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    "</center><h4>Odhad rozdílu mediánù (A1-A2) dle Mannova-Whitneyova testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>",
      "A1-A2: Bodový odhad = ",
      round(quantile(A1_auto, 0.5) - quantile(A2_auto, 0.5), digits = 5),
      ", intervalový odhad=<",
      round(A1A2_MWtest$conf.int[1], digits = 5),
      ", ",
      round(A1A2_MWtest$conf.int[2], digits = 5),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  
  #---------------------------D--------------------------
  write("</center><h2>d)</h2><center>",
        file = filename,
        append = TRUE)
  
  write(
    "</center><h4>Test H0 : (A1-A2)=0 vs. oboustranná alt. dle t-testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  
  if (A1A2_ttest$p.value >= 0.0005) {
    t_p = paste0("= ", round(A1A2_ttest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  
  write(
    paste0("</center>",
           "p-hodnota ", t_p, "",
           "<center><br>"),
    file = filename,
    append = TRUE
  )
  write(
    "</center><h4>Test H0 : (A1-A2)=0 vs. oboustranná alt. dle Aspinové-Welshova testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  if (A1A2_ttest$p.value >= 0.0005) {
    t_p = paste0("= ", round(A1A2_ttest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>",
           "p-hodnota ", t_p, "",
           "<center><br>"),
    file = filename,
    append = TRUE
  )
  write(
    "</center><h4>Test H0 : (A1-A2)=0 vs. oboustranná alt. dle Mannova-Whitneyova testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  if (A1A2_ttest$p.value >= 0.0005) {
    t_p = paste0("= ", round(A1A2_ttest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>",
           "p-hodnota ", t_p, "",
           "<center><br>"),
    file = filename,
    append = TRUE
  )
  
  
  #---------------------------e--------------------------
  write("</center><br><br><h2>e)</h2><center>",
        file = filename,
        append = TRUE)
  
  rozdil_OP = data$`automaticke mereni` - data$`manualni mereni`
  rozdil_bezOP = rozdil_OP
  pom = boxplot(rozdil_bezOP)
  rozdil_bezOP[rozdil_bezOP %in% pom$out] = NaN
  rozdil_bezOP = na.omit(rozdil_bezOP)
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot_rozdil.jpg"),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  boxplot(rozdil_OP,
          main = "Graf 11: Rozdíl v mìøeních auto. - man. (dohromady, s OP)",
          ylab = "Rozdíl ve velikosti zrn [nm]",
          xlab = "Automaticky - manuálnì")
  boxplot(rozdil_bezOP,
          main = "Graf 12: Rozdíl v mìøeních auto. - man. (dohromady, bez OP)",
          ylab = "Rozdíl ve velikosti zrn [nm]",
          xlab = "Automaticky - manuálnì")
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot_rozdil.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  write("</center>Pro data bez OP.<center><br>",
        file = filename,
        append = TRUE)
  write(
    "</center>Jedná se o jednovýbìrové (párové) testy, mám na výbìr <strong>t-test, Wilcoxonùv test nebo mediánový test</strong>.<center><br>",
    file = filename,
    append = TRUE
  )
  write(
    "</center>Nejprve ovìøím <strong>normalitu</strong> u rozdílu.<center><br>",
    file = filename,
    append = TRUE
  )
  
  
  Rozdil_SW = shapiro.test(rozdil_bezOP)
  Rozdil_SW$p.value
  if (Rozdil_SW$p.value >= 0.0005) {
    t_p = paste0("= ", round(Rozdil_SW$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0(
      "</center>Shapiro-Wilk test normality pro rozdíl vyšl s p-hodnotou ",
      t_p,
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>Šikmost rozdílu je ",
      round(skewness(rozdil_bezOP), digits = 2),
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  library(lawstat)
  
  RZ_symtest = symmetry.test(rozdil_bezOP, boot = FALSE)
  if (RZ_symtest$p.value >= 0.0005) {
    t_p = paste0("= ", round(RZ_symtest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>Test symetrie, p-hodnota ", t_p,
           "<center><br>"),
    file = filename,
    append = TRUE
  )
  
  
  
  jpeg(
    paste0(
      output_data_dir,
      "du_",
      c_ukolu,
      "_img/histogram_rozdil.jpg"
    ),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  hist(
    rozdil_bezOP,
    main = "Graf 13: Rozdíl v mìøeních auto. - man. (dohromady, bez OP)",
    xlab = "Rozdíl ve velikosti zrn [nm]",
    ylab = "Absolutní èetnosti",
    breaks = 20
  )
  
  # x = quantile(rozdil_bezOP, seq(0.005, 0.5, 0.005))
  # y = quantile(rozdil_bezOP, 1 - seq(0.005, 0.5, 0.005))
  # mm = quantile(rozdil_bezOP, 0.5)
  # yy = y - mm
  # xx = mm - x
  # plot(
  #   c(0, max(max(xx), max(yy))),
  #   c(0, max(max(xx), max(yy))),
  #   type = 'l',
  #   main = "Graf 14: Srovnání kvantilù (symetrické rozdìlení leží na lince)",
  #   xlab = "Medián - kvantily (0,0.5)",
  #   ylab = "Kvantily (0.5,1) - medián"
  # )
  # points(xx, yy)
  
  
  x = quantile(rozdil_bezOP, seq(0.01, 0.5 - 0.01, 0.01))
  mm = quantile(rozdil_bezOP, 0.5)
  plot(
    c(0, 0.5),
    c(0, 0.5),
    type = 'l',
    main = "Graf 14: Srovnání kvantilù (symetrické rozdìlení leží na lince)",
    xlab = "Kvantily (0,0.5)",
    ylab = "1 - odpovídající kvantily (0.5,1)"
  )
  tmp = ecdf(rozdil_bezOP)(mm + mm - x)
  
  points(seq(0.01, 0.5 - 0.01, 0.01), 1 - tmp)
  
  dev.off()
  # write(
  #   paste0(
  #     "</center>Poznámka ke Grafu 14, jedná se o vykreslení dvojic kvantilù (q,1-q) (napø.: X0.2 a X0.8). Vykreslení je provedeno následovnì: napoètou se vybrané kvantily z intervalu (0,0.5), pro každý takovýto kvantil se napoèítá ideální symetrická hodnota vzhledem k mediánu (medián+(medián-kvantil)) a pomocí \"ecdf\" se spoète cdf tohoto symetrického kvantilu, do grafu se pak zakreslí 1- tato hodnota. Èím je rozdìlení blíže symetrickému rozdìlení, tím více bude okolo identity (nikdy však mimo ètverec <0,0.5>x<0,0.5>).",
  #     "<center><br>"
  #   ),
  #   file = filename,
  #   append = TRUE
  # )
  write(paste0(
    "<img src=",
    "\"",
    paste0(
      output_data_dir,
      "du_",
      c_ukolu,
      "_img/histogram_rozdil.jpg"
    ),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  write(
    paste0(
      "</center>Støední hodnota je ",
      round(mean(rozdil_bezOP), digits = 5),
      ", medián je ",
      round(quantile(rozdil_bezOP, 0.5), digits = 5),
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  
  RZ_ttest = t.test(rozdil_bezOP)
  RZ_wtest = wilcox.test(rozdil_bezOP, conf.int = TRUE)
  library(signmedian.test)
  RZ_mtest = signmedian.test(
    rozdil_bezOP,
    mu = 0,
    conf.level = 0.95,
    alternative = "two.sided",
    conf.int = TRUE
  )
  
  write(
    "</center><h4>Test H0 : rozdíl=0 vs. oboustranná alt. dle t-testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  
  if (RZ_ttest$p.value >= 0.0005) {
    t_p = paste0("= ", round(RZ_ttest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  
  write(
    paste0("</center>",
           "p-hodnota ", t_p, "",
           "<center><br>"),
    file = filename,
    append = TRUE
  )
  write(
    "</center><h4>Test H0 : rozdíl=0 vs. oboustranná alt. dle Wilcoxonova testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  if (RZ_wtest$p.value >= 0.0005) {
    t_p = paste0("= ", round(RZ_wtest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>",
           "p-hodnota ", t_p, "",
           "<center><br>"),
    file = filename,
    append = TRUE
  )
  write(
    "</center><h4>Test H0 : rozdíl=0 vs. oboustranná alt. dle znaménkového testu:</h4><center><br>",
    file = filename,
    append = TRUE
  )
  if (RZ_mtest$p.value >= 0.0005) {
    t_p = paste0("= ", round(RZ_mtest$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>",
           "p-hodnota ", t_p, "",
           "<center><br>"),
    file = filename,
    append = TRUE
  )
  
  
  
  ###########################ukol3#######################
  write("</center><h2>Úkol 3.</h2><center>",
        file = filename,
        append = TRUE)
  #---------------------------A--------------------------
  write("</center><h2>a)</h2><center>",
        file = filename,
        append = TRUE)
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot_multi.jpg"),
    width = 900,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  boxplot(
    data$`automaticke mereni` ~ data$`typ abraziva`,
    main = "Graf 15: Velikost zrn abrazivní složky (vèetnì OP)",
    xlab = "Typ abraziva",
    ylab = "Velikost zrn [nm]"
  )
  boxplot(
    data_bezOP$`automaticke mereni` ~ data_bezOP$`typ abraziva`,
    main = "Graf 16: Velikost zrn abrazivní složky (bez OP)",
    xlab = "Typ abraziva",
    ylab = "Velikost zrn [nm]"
  )
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot_multi.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  
  #---------------------------B--------------------------
  write("</center><h2>b)</h2><center>",
        file = filename,
        append = TRUE)
  
  write("</center>Dále pracujeme bez OP.<center>",
        file = filename,
        append = TRUE)
  
  normality_exact_empiric <- function(x)
    data.frame(
      Shapiro_Wilk = round(shapiro.test(x)$p.value, digits = 3),
      sikmost = round(skewness(x), digits =
                        5),
      spicatost = round(kurtosis(x) - 3, digits =
                          5)
    )
  
  summary_stack = tapply(
    data_bezOP$`automaticke mereni`,
    data_bezOP$`typ abraziva`,
    normality_exact_empiric
  )
  summary_matrix <- matrix(unlist(t(summary_stack)), ncol = 4)
  colnames(summary_matrix) = colnames(t(summary_stack))
  rownames(summary_matrix) = colnames(summary_stack[[1]])
  
  summary_html = htmlTable(
    summary_matrix,
    col.columns = c("none", "#CCCCCC"),
    rgroup = c("Exaktní",
               "Exploraèní"),
    n.rgroup = c(1, 2),
    css.cell = "padding-left: .7em; padding-right: .7em;"
  )
  summary_html
  write(
    "<h4>Tabulka 3: Exploraèní a exaktní (Shapiro-Wilk test) posouzení normality</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html), file = filename, append = TRUE)
  
  
  #---------------------------C--------------------------
  write("</center><h2>c)</h2><center>",
        file = filename,
        append = TRUE)
  res_lev = levene.test(data_bezOP$`automaticke mereni` , data_bezOP$`typ abraziva`)
  res_bart = bartlett.test(data_bezOP$`automaticke mereni` , data_bezOP$`typ abraziva`)
  
  rozptyly_empiric <- function(x)
    data.frame(rozptyl = round(var(x), digits =
                                 5))
  
  vars = tapply(data_bezOP$`automaticke mereni` ,
                data_bezOP$`typ abraziva`,
                rozptyly_empiric)
  summary_matrix <- matrix(unlist(t(vars)), ncol = 4)
  colnames(summary_matrix) = colnames(t(vars))
  rownames(summary_matrix) = "Rozptyl &nbsp"
  
  summary_html = htmlTable(summary_matrix,
                           col.columns = c("none", "#CCCCCC"),
                           css.cell = "padding-left: .7em; padding-right: .7em;")
  summary_html
  write(
    "<h4>Tabulka 4: Rozptyly jednotlivých typù abraziv</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html), file = filename, append = TRUE)
  stds = tapply(data_bezOP$`automaticke mereni` ,
                data_bezOP$`typ abraziva`,
                sd)
  
  
  
  
  write(
    paste0(
      "</center>Pomìr nejvìtšího a nejmenšího rozptylu ",
      round((max(stds) / min(stds)) ^ 2, digits = 3),
      "<center>"
    ),
    file = filename,
    append = TRUE
  )
  if (res_bart$p.value >= 0.0005) {
    t_p = paste0("= ", round(res_bart$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>Bartlett test p-hodnota ", t_p, "<center>"),
    file = filename,
    append = TRUE
  )
  if (res_lev$p.value >= 0.0005) {
    t_p = paste0("= ", round(res_lev$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>Levene test p-hodnota ", t_p, "<center>"),
    file = filename,
    append = TRUE
  )
  
  
  #---------------------------D--------------------------
  write("</center><h2>d)</h2><center>",
        file = filename,
        append = TRUE)
  
  
  IO_mean <- function(x) {
    res = t.test(x)
    data.frame(
      bodovy_odhad = round(mean(x), digits = 5),
      dolni_mez = round(res$conf.int[1], digits = 5),
      horni_mez = round(res$conf.int[2], digits = 5)
    )
  }
  
  IO_median <- function(x) {
    res = wilcox.test(x, conf.int = TRUE)
    data.frame(
      bodovy_odhad = round(median(x), digits = 5),
      dolni_mez = round(res$conf.int[1], digits = 5),
      horni_mez = round(res$conf.int[2], digits = 5)
    )
  }
  
  
  
  mean_stack = tapply(data$`automaticke mereni`, data$`typ abraziva`, IO_mean)
  summary_matrix_mean <- matrix(unlist(t(mean_stack)), ncol = 4)
  colnames(summary_matrix_mean) = colnames(t(mean_stack))
  rownames(summary_matrix_mean) = colnames(mean_stack[[1]])
  
  median_stack = tapply(data$`automaticke mereni`, data$`typ abraziva`, IO_median)
  summary_matrix_median <- matrix(unlist(t(median_stack)), ncol = 4)
  colnames(summary_matrix_median) = colnames(t(median_stack))
  rownames(summary_matrix_median) = colnames(median_stack[[1]])
  
  
  summary_html = htmlTable(
    rbind(summary_matrix_mean, summary_matrix_median),
    col.columns = c("none", "#CCCCCC"),
    rgroup = c("Odhady støední hodnoty",
               "Odhady mediánu"),
    n.rgroup = c(3, 3),
    css.cell = "padding-left: .7em; padding-right: .7em;"
  )
  
  write(
    "<h4>Tabulka 5: Bodové a 95% intervalové odhady støední hodnoty (t-test) a mediánu (Wilcox test)</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html), file = filename, append = TRUE)
  
  #---------------------------E--------------------------
  write("</center><h2>e)</h2><center>",
        file = filename,
        append = TRUE)
  
  anova_res = summary(aov(data_bezOP$`automaticke mereni` ~ data_bezOP$`typ abraziva`))
  
  if (anova_res[[1]]$`Pr(>F)`[1] >= 0.0005) {
    t_p = paste0("= ", round(anova_res[[1]]$`Pr(>F)`[1], digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>Anova p-hodnota ", t_p, "<center>"),
    file = filename,
    append = TRUE
  )
  data_bezOP$`typ abraziva` = as.factor(data_bezOP$`typ abraziva`)
  KW_res = kruskal.test(data_bezOP$`automaticke mereni` , data_bezOP$`typ abraziva`)
  if (KW_res$p.value >= 0.0005) {
    t_p = paste0("= ", round(KW_res$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  write(
    paste0("</center>Kruskal-Wallis test p-hodnota ", t_p, "<center>"),
    file = filename,
    append = TRUE
  )
  
  t_mean = mean(data_bezOP$`automaticke mereni`)
  t_median = quantile(data_bezOP$`automaticke mereni`, 0.5)
  
  efekty <- function(x) {
    data.frame(
      efekt_prumery = round(-t_mean + mean(x), digits = 5),
      efekt_median = round(-t_median + quantile(x, 0.5), digits = 5)
    )
  }
  efekty_stack = tapply(data$`automaticke mereni`, data$`typ abraziva`, efekty)
  summary_matrix_efekty <- matrix(unlist(t(efekty_stack)), ncol = 4)
  colnames(summary_matrix_efekty) = colnames(t(efekty_stack))
  rownames(summary_matrix_efekty) = colnames(efekty_stack[[1]])
  
  summary_html = htmlTable(
    summary_matrix_efekty,
    col.columns = c("none", "#CCCCCC"),
    rgroup = c("Efekty pro ANOVU",
               "Efekty pro Kruskal-Wallise"),
    n.rgroup = c(1, 1),
    css.cell = "padding-left: .7em; padding-right: .7em;"
  )
  
  write("<h4>Tabulka 6: Efekty</h4>",
        file = filename,
        append = TRUE)
  write(paste0(summary_html), file = filename, append = TRUE)
  write(
    paste0(
      "</center> V tabulkách níže jsou abrazivní složky setøízeny sestupnì dle efektù. <center>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    "<h4>Tabulka 7: Post-Hoc Tukey HSD &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp Tabulka 8: Dunové test (Bonferroni)</h4>",
    file = filename,
    append = TRUE
  )
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/post_hoc.jpg"),
    width = 800,
    height = 400,
    quality = 100,
    pointsize = 15
  )
  par(mfrow = c(1, 2), pty = "s")
  source("anova.R")
  source("KW.R")
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/post_hoc.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  
  
  ###########################ukol4#######################
  write(
    "</center><h2><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>Úkol 4.</h2><center>",
    file = filename,
    append = TRUE
  )
  write(
    "</center><h2>Pokud jsou použita pùvodní data (tedy i ta odstranìná v rámci úkolù 1-3)</h2><center>",
    file = filename,
    append = TRUE
  )
  
  #---------------------------A--------------------------
  write("</center><h2>a)</h2><center>",
        file = filename,
        append = TRUE)
  
  tab = table(data$`typ abraziva`, data$`vysledek chemickeho testu`)
  
  library(lsr)
  cv = round(cramersV(tab), digits = 3)
  res = chisq.test(tab)
  cc = round(sqrt(res$statistic / (res$statistic + sum(tab))), digits = 3)
  cc_corig = round(cc / sqrt(1 / 2), digits = 3)
  
  abs_tab = addmargins(tab)
  rel_tab = round(addmargins(tab / sum(tab)), digits = 3)
  prob_fail = addmargins(t(tab[, 1] / abs_tab[1:4, 3]), 2)
  rownames(prob_fail) = "Relativní èetnost"
  summary_html = htmlTable(
    rbind(t(abs_tab), t(rel_tab), round(prob_fail, digits = 3)),
    col.columns = c("none", "#CCCCCC"),
    rgroup = c(
      "Absolutní hodnoty",
      "Relativní hodnoty",
      "Riziko negativního výsledku chem. testu"
    ),
    n.rgroup = c(3, 3, 1),
    css.cell = "padding-left: .7em; padding-right: .7em;"
  )
  
  
  write(
    "<h4>Tabulka 9: Kontingenèní tabulka (absolutní a relativní hodnoty)</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html), file = filename, append = TRUE)
  
  write(
    paste0(
      "</center>",
      "CramersV = ",
      cv,
      ", koeficient kontingence = ",
      cc,
      ", korigovaný koeficient kontingence = ",
      cc_corig,
      "<center>"
    ),
    file = filename,
    append = TRUE
  )
  
  
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic.jpg"),
    width = 850,
    height = 400,
    quality = 100,
    pointsize = 15
  )
  par(mfrow = c(1, 1), pty = "s")
  mosaicplot(tab, main = "Graf 17: Mosaikový graf rozdìlení výsledku chemického testu mezi jednotlivé typy abraziv")
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  #---------------------------b--------------------------
  write("</center><h2>b)</h2><center>",
        file = filename,
        append = TRUE)
  
  p = tab[3, 1] / (tab[3, 1] + tab[3, 2])
  write(
    paste0(
      "</center>Bodový odhad pravdìpodobnosti p= ",
      round(p, digits = 3),
      "<center>"
    ),
    file = filename,
    append = TRUE
  )
  n0 = 9 / (p * (1 - p))
  write(
    paste0(
      "</center>Pøepoklad testu: minimální poèet pozorování (9/(p*(1-p))) = ",
      ceiling(n0),
      ", skuteèný poèet pozorování = ",
      (tab[3, 1] + tab[3, 2]),
      "<center>"
    ),
    file = filename,
    append = TRUE
  )
  res = binom.test(tab[3, 1], (tab[3, 1] + tab[3, 2]))
  
  write(
    paste0(
      "</center>",
      "A3 absolutní riziko: Bodový odhad = ",
      round(p, digits = 3),
      ", intervalový odhad=<",
      round(res$conf.int[1], digits = 3),
      ", ",
      round(res$conf.int[2], digits = 3),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  #---------------------------c+d--------------------------
  write("</center><h2>c,d)</h2><center>",
        file = filename,
        append = TRUE)
  
  
  a_tab = tab[3:4, ]
  a_tab[2, ] = colSums(tab[c(1, 2, 4), ])
  rownames(a_tab) = c('A3', 'A1+A2+A4')
  
  summary_html = htmlTable(a_tab,
                           col.columns = c("none", "#CCCCCC"),
                           css.cell = "padding-left: .7em; padding-right: .7em;")
  write(
    "<h4>Tabulka 10: Asociaèní tabulka, Exposed=A3,Outcome=neprosel)</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html), file = filename, append = TRUE)
  library(epiR)
  res = epi.2by2(a_tab)
  RR = round(res$massoc$RR.strata.wald, digits = 2)
  OR = round(res$massoc$OR.strata.wald, digits = 2)
  
  summary_html1 = htmlTable(res$tab,
                            col.columns = c("none", "#CCCCCC"),
                            css.cell = "padding-left: .7em; padding-right: .7em;")
  
  write(paste0(summary_html1), file = filename, append = TRUE)
  
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic2.jpg"),
    width = 850,
    height = 350,
    quality = 100,
    pointsize = 15
  )
  par(mfrow = c(1, 1), pty = "s")
  mosaicplot(a_tab, main = "Graf 18: Mosaikový graf pro asociaèní tabulku")
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic2.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  write(
    paste0(
      "</center>",
      "Relativní riziko: Bodový odhad = ",
      RR[1],
      ", intervalový odhad=<",
      RR[2],
      ", ",
      RR[3],
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>",
      "Pomìr šancí: Bodový odhad = ",
      OR[1],
      ", intervalový odhad=<",
      OR[2],
      ", ",
      OR[3],
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  #---------------------------e--------------------------
  write("</center><h2>e)</h2><center>",
        file = filename,
        append = TRUE)
  res = chisq.test(tab)
  
  
  abs_tab = round(addmargins(res$expected), digits = 3)
  rel_tab = round(addmargins(res$expected / sum(res$expected)), digits = 3)
  
  summary_html = htmlTable(rbind(t(abs_tab)),
                           col.columns = c("none", "#CCCCCC"),
                           css.cell = "padding-left: .7em; padding-right: .7em;")
  
  
  
  write(
    "<h4>Tabulka 11: Oèekávané absolutní èetnosti pro ChiSq test nezávislosti</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html), file = filename, append = TRUE)
  
  
  if (res$p.value >= 0.0005) {
    t_p = paste0("= ", round(res$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  
  
  write(
    paste0(
      "</center>",
      "ChiSqtest p-hodnota",
      t_p,
      ", poèet stupòù volnosti=",
      res$parameter,
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  
  
  write(
    "</center><h2>Pokud jsou použita upravená data (tedy bez odlehlých pozorování pro automatické mìøení)</h2><center>",
    file = filename,
    append = TRUE
  )
  
  #---------------------------A--------------------------
  write("</center><h2>a)</h2><center>",
        file = filename,
        append = TRUE)
  
  tab = table(data_bezOP$`typ abraziva`,
              data_bezOP$`vysledek chemickeho testu`)
  
  library(lsr)
  cv = round(cramersV(tab), digits = 3)
  res = chisq.test(tab)
  cc = round(sqrt(res$statistic / (res$statistic + sum(tab))), digits = 3)
  cc_corig = round(cc / sqrt(1 / 2), digits = 3)
  
  abs_tab = addmargins(tab)
  rel_tab = round(addmargins(tab / sum(tab)), digits = 3)
  prob_fail = addmargins(t(tab[, 1] / abs_tab[1:4, 3]), 2)
  rownames(prob_fail) = "Relativní èetnost"
  summary_html = htmlTable(
    rbind(t(abs_tab), t(rel_tab), round(prob_fail, digits = 3)),
    col.columns = c("none", "#CCCCCC"),
    rgroup = c(
      "Absolutní hodnoty",
      "Relativní hodnoty",
      "Riziko negativního výsledku chem. testu"
    ),
    n.rgroup = c(3, 3, 1),
    css.cell = "padding-left: .7em; padding-right: .7em;"
  )
  
  
  write(
    "<h4>Tabulka 9: Kontingenèní tabulka (absolutní a relativní hodnoty)</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html), file = filename, append = TRUE)
  
  write(
    paste0(
      "</center>",
      "CramersV = ",
      cv,
      ", koeficient kontingence = ",
      cc,
      ", korigovaný koeficient kontingence = ",
      cc_corig,
      "<center>"
    ),
    file = filename,
    append = TRUE
  )
  
  
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic.jpg"),
    width = 850,
    height = 400,
    quality = 100,
    pointsize = 15
  )
  par(mfrow = c(1, 1), pty = "s")
  mosaicplot(tab, main = "Graf 17: Mosaikový graf rozdìlení výsledku chemického testu mezi jednotlivé typy abraziv")
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  
  #---------------------------b--------------------------
  write("</center><h2>b)</h2><center>",
        file = filename,
        append = TRUE)
  
  p = tab[3, 1] / (tab[3, 1] + tab[3, 2])
  write(
    paste0(
      "</center>Bodový odhad pravdìpodobnosti p= ",
      round(p, digits = 3),
      "<center>"
    ),
    file = filename,
    append = TRUE
  )
  n0 = 9 / (p * (1 - p))
  write(
    paste0(
      "</center>Pøepoklad testu: minimální poèet pozorování (9/(p*(1-p))) = ",
      ceiling(n0),
      ", skuteèný poèet pozorování = ",
      (tab[3, 1] + tab[3, 2]),
      "<center>"
    ),
    file = filename,
    append = TRUE
  )
  res = binom.test(tab[3, 1], (tab[3, 1] + tab[3, 2]))
  
  write(
    paste0(
      "</center>",
      "A3 absolutní riziko: Bodový odhad = ",
      round(p, digits = 3),
      ", intervalový odhad=<",
      round(res$conf.int[1], digits = 3),
      ", ",
      round(res$conf.int[2], digits = 3),
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  #---------------------------c+d--------------------------
  write("</center><h2>c,d)</h2><center>",
        file = filename,
        append = TRUE)
  
  
  a_tab = tab[3:4, ]
  a_tab[2, ] = colSums(tab[c(1, 2, 4), ])
  rownames(a_tab) = c('A3', 'A1+A2+A4')
  
  summary_html = htmlTable(a_tab,
                           col.columns = c("none", "#CCCCCC"),
                           css.cell = "padding-left: .7em; padding-right: .7em;")
  write(
    "<h4>Tabulka 10: Asociaèní tabulka, Exposed=A3,Outcome=neprosel)</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html), file = filename, append = TRUE)
  library(epiR)
  res = epi.2by2(a_tab)
  RR = round(res$massoc$RR.strata.wald, digits = 2)
  OR = round(res$massoc$OR.strata.wald, digits = 2)
  
  summary_html1 = htmlTable(res$tab,
                            col.columns = c("none", "#CCCCCC"),
                            css.cell = "padding-left: .7em; padding-right: .7em;")
  
  write(paste0(summary_html1), file = filename, append = TRUE)
  
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic2.jpg"),
    width = 850,
    height = 350,
    quality = 100,
    pointsize = 15
  )
  par(mfrow = c(1, 1), pty = "s")
  mosaicplot(a_tab, main = "Graf 18: Mosaikový graf pro asociaèní tabulku")
  dev.off()
  write(paste0(
    "<img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/mosaic2.jpg"),
    "\"",
    ">"
  ),
  file = filename,
  append = TRUE)
  write(
    paste0(
      "</center>",
      "Relativní riziko: Bodový odhad = ",
      RR[1],
      ", intervalový odhad=<",
      RR[2],
      ", ",
      RR[3],
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "</center>",
      "Pomìr šancí: Bodový odhad = ",
      OR[1],
      ", intervalový odhad=<",
      OR[2],
      ", ",
      OR[3],
      ">",
      "<center><br>"
    ),
    file = filename,
    append = TRUE
  )
  #---------------------------e--------------------------
  write("</center><h2>e)</h2><center>",
        file = filename,
        append = TRUE)
  res = chisq.test(tab)
  
  
  abs_tab = round(addmargins(res$expected), digits = 3)
  rel_tab = round(addmargins(res$expected / sum(res$expected)), digits = 3)
  
  summary_html = htmlTable(rbind(t(abs_tab)),
                           col.columns = c("none", "#CCCCCC"),
                           css.cell = "padding-left: .7em; padding-right: .7em;")
  
  
  
  write(
    "<h4>Tabulka 11: Oèekávané absolutní èetnosti pro ChiSq test nezávislosti</h4>",
    file = filename,
    append = TRUE
  )
  write(paste0(summary_html), file = filename, append = TRUE)
  
  
  if (res$p.value >= 0.0005) {
    t_p = paste0("= ", round(res$p.value, digits = 3))
  } else {
    t_p = "<< 0.001"
  }
  
  
  write(
    paste0(
      "</center>",
      "ChiSqtest p-hodnota",
      t_p,
      ", poèet stupòù volnosti=",
      res$parameter,
      "<center>"
    ),
    file = filename,
    append = TRUE
  )
  
  
  write("</center></body>", file = filename, append = TRUE)
  write("</html>", file = filename, append = TRUE)
  
  setwd(paste0(output_data_dir, "du_", c_ukolu, "_img"))
  system(paste0("../", back_route_dir, "crop_pictures.sh"))
  setwd("..")
  setwd(back_route_dir)
}
setwd(output_data_dir)
system(paste0(back_route_dir, "convert_all_to_pdf.sh"))
