###########################ukol1#######################

uloha1 = function(filename,
                  c_ukolu,
                  output_data_dir,
                  all_data1,
                  all_data2,
                  composition,
                  data1,
                  data2,
                  nazvy_sloupcu = c("22 stupnu", "5 stupnu"),
                  name_classes = "výrobců",
                  data_names = c("A", "B"),
                  dataset_name = "Amber",
                  velicina = "Svetelny tok [lm]") {
  
  write(" <center><h1>Úkol 1.</h1></center> ",
        file = filename,
        append = TRUE)
  
  all_data1_bezOP = all_data1
  for (i in unique(composition)) {
    pom = boxplot(all_data1_bezOP[composition == i])
    all_data1_bezOP[(all_data1_bezOP %in% pom$out) &
                      (composition == i)] = NaN
  }
  composition1_OP = composition
  composition1_OP = composition1_OP[!is.nan(all_data1_bezOP)]
  all_data1_bezOP = na.omit(all_data1_bezOP)
  
  
  all_data2_bezOP = all_data2
  for (i in unique(composition)) {
    pom = boxplot(all_data2_bezOP[composition == i])
    all_data2_bezOP[(all_data2_bezOP %in% pom$out) &
                      (composition == i)] = NaN
  }
  composition2_OP = composition
  composition2_OP = composition2_OP[!is.nan(all_data2_bezOP)]
  all_data2_bezOP = na.omit(all_data2_bezOP)
  
  
  #---------------------------A--------------------------
  write(" <h2>a)</h2> ",
        file = filename,
        append = TRUE)
  
  rozdeleni = table(composition)
  rozdeleni[5] = sum(rozdeleni)
  rozdeleni = rbind(rozdeleni, round(rozdeleni / rozdeleni[5], digits = 5))
  rownames(rozdeleni) = c("absolutní četnosti", "relativní četnosti")
  rownames_html = htmlTable(rozdeleni,
                            col.columns = c("none", "#CCCCCC"),
                            css.cell = "padding-left: .7em; padding-right: .7em;")
  rownames_html
  write(
    paste0("<center><h4>Tabulka 1: Zastoupení ", name_classes, " ve výběru</h4></center>"),
    file = filename,
    append = TRUE
  )
  write(paste0("<center>",rownames_html,"</center>"), file = filename, append = TRUE)
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/pie.jpg"),
    width = 700,
    height = 350
  )
  par(mfrow = c(1, 2), pty = "s")
  pie(
    rozdeleni[1, -5],
    col = heat.colors(4),
    main = paste0("Graf 1: Zastoupení testovaných ", name_classes),
    labels = paste0(
      names(rozdeleni[1, -5]),
      "\n",
      rozdeleni[1, -5],
      "; ",
      round(rozdeleni[2, -5] * 100, digits = 1),
      "%"
    )
  )
  
  bp = barplot(
    rozdeleni[1, -5],
    col = heat.colors(4),
    main = paste0("Graf 2: Zastoupení testovaných ", name_classes),
    names.arg = paste(names(rozdeleni[1, -5]))
  )
  text(bp,
       rozdeleni[1, -5], paste0(round(rozdeleni[2, -5] * 100, digits = 1), "%"),
       pos = 1)
  dev.off()
  write(paste0(
    " <center><img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/pie.jpg"),
    "\"",
    " ></center><br>"
  ),
  file = filename,
  append = TRUE)
  
  #---------------------------B--------------------------
  write(" <h2>b)</h2> ",
        file = filename,
        append = TRUE)
  
  my_summary <- function(x){
    tmp = data.frame(
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
    return(tmp)
  }
  
  summary_matrix1 = t(my_summary(data1))
  colnames(summary_matrix1) = c(data_names[1])
  
  summary_matrix2 = t(my_summary(data2))
  colnames(summary_matrix2) = c(data_names[2])
  
  data1_OP = data1
  pom = boxplot(data1_OP)
  data1_OP[data1_OP %in% pom$out] = NaN
  data1_OP = na.omit(data1_OP)
  
  data2_OP = data2
  pom = boxplot(data2_OP)
  data2_OP[data2_OP %in% pom$out] = NaN
  data2_OP = na.omit(data2_OP)
  
  summary_matrix_OP1 = t(my_summary(data1_OP))
  colnames(summary_matrix_OP1) = c(data_names[1])
  
  summary_matrix_OP2 = t(my_summary(data2_OP))
  colnames(summary_matrix_OP2) = c(data_names[2])
  
  summary_html = htmlTable(
    cbind(
      summary_matrix1,
      summary_matrix2,
      summary_matrix_OP1,
      summary_matrix_OP2
    ),
    cgroup = c("Včetně odlehlých pozorování",
               "Bez odlehlých pozorování"),
    n.cgroup = c(2, 2),
    col.columns = c("none", "#CCCCCC"),
    rgroup = c(
      "",
      "Míry polohy",
      "Míry variability",
      "Míry šikmosti a špičatosti",
      "Vnitřní hradby"
    ),
    n.rgroup = c(1, 6, 2, 2, 2),
    css.cell = "padding-left: .7em; padding-right: .7em;"
  )
  summary_html
  write(
    paste0(
      "<center><h4>Tabulka 2: Výběrové charakteristiky pro ",
      dataset_name ,
      " ",
      data_names[1],
      " ",
      data_names[2],
      " </h4></center>"
    ),
    file = filename,
    append = TRUE
  )
  write(paste0("<center>",summary_html,"</center>"), file = filename, append = TRUE)
  
  jpeg(
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot.jpg"),
    width = 900,
    height = 700
  )
  par(mfrow = c(2, 2), pty = "s")
  boxplot(
    all_data1 ~ composition,
    main = paste0("Graf 3: ", velicina, " ", nazvy_sloupcu[1], " (včetně OP)"),
    xlab = name_classes,
    ylab = velicina
  )
  boxplot(
    all_data1_bezOP ~ composition1_OP,
    main = paste0("Graf 4: ", velicina, " ", nazvy_sloupcu[1], " (bez OP)"),
    xlab = name_classes,
    ylab = velicina
  )
  boxplot(
    all_data2 ~ composition,
    main = paste0("Graf 3: ", velicina, " ", nazvy_sloupcu[2], " (včetně OP)"),
    xlab = name_classes,
    ylab = velicina
  )
  boxplot(
    all_data2_bezOP ~ composition2_OP,
    main = paste0("Graf 4: ", velicina, " ", nazvy_sloupcu[2], " (bez OP)"),
    xlab = name_classes,
    ylab = velicina
  )
  dev.off()
  write(paste0(
    " <center><img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/boxplot.jpg"),
    "\"",
    " ></center><br>"
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
    data1_OP,
    main = paste0("Graf 5: ", nazvy_sloupcu[1], dataset_name, " (bez OP)"),
    xlab = velicina,
    ylab = "Absolutní četnosti",
    breaks = 20
  )
  hist(
    data2_OP,
    main = paste0("Graf 6: ", nazvy_sloupcu[2], dataset_name, " (bez OP)"),
    xlab = velicina,
    ylab = "Absolutní četnosti",
    breaks = 20
  )
  dev.off()
  write(paste0(
    " <center><img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/histogramy.jpg"),
    "\"",
    " ></center><br>"
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
    data1_OP,
    main = paste0(
      "Graf 7: QQ graf ",
      velicina,
      " ",
      dataset_name,
      " ",
      nazvy_sloupcu[1]
    ),
    xlab = "Teoretické kvantily normálního rozdělení",
    ylab = "Naměřené kvantily výběru"
  )
  qqline(data1_OP)
  qqnorm(
    data2_OP,
    main = paste0(
      "Graf 8: QQ graf ",
      velicina,
      " ",
      dataset_name,
      " ",
      nazvy_sloupcu[2]
    ),
    xlab = "Teoretické kvantily normálního rozdělení",
    ylab = "Naměřené kvantily výběru"
  )
  qqline(data2_OP)
  dev.off()
  write(paste0(
    " <center><img src=",
    "\"",
    paste0(output_data_dir, "du_", c_ukolu, "_img/qq_ploty.jpg"),
    "\"",
    " ></center><br>"
  ),
  file = filename,
  append = TRUE)
  
  A1_auto = data1_OP
  A2_auto = data2_OP
  
  write(
    paste0(
      " <br ></center><br>Pro ",
      dataset_name,
      " a data bez odlehlých pozorování.<br>"
    ),
    file = filename,
    append = TRUE
  )
  write(
    paste0(
      "Pro ",
      nazvy_sloupcu[1],
      " je 2 sigma interval: ",
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
      "Pro ",
      nazvy_sloupcu[2],
      " je 2 sigma interval: ",
      round(mean(A2_auto) - 2 * sd(A2_auto), digits = 4),
      " nm - ",
      round(mean(A2_auto) + 2 * sd(A2_auto), digits = 4),
      " nm.<br> "
    ),
    file = filename,
    append = TRUE
  )
}
