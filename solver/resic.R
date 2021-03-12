library(readxl)
library(htmlTable)
library(magrittr)
library(moments)

source_data_dir = "../DU_LS21_prez/standard_dat_format/"
output_data_dir = "../reseni_LS21_prez/"
back_route_dir = "../solver/"

dir.create(output_data_dir)
system(paste0("cp styl.css ", output_data_dir, "styl.css"))

for (c_ukolu in 1:301) {
  data = read_excel(paste0(source_data_dir, "ukol_", c_ukolu, ".xlsx"))
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
      "<body>"
    ),
    fileConn
  )
  close(fileConn)
  
  write(
    paste0(
      "<center><strong><h1>Zadání ",
      c_ukolu,
      ".</h1></strong></center>"
    ),
    file = filename,
    append = TRUE
  )
  ########################### Ukol 1 xxxxxxxxxxxxxxxxxxxxxxx#######################
  source("uloha1.R")
  
  uloha1(
    filename = filename,
    c_ukolu = c_ukolu,
    output_data_dir = output_data_dir,
    all_data1 = data$`svítivost při teplotě 22 °C (lm)`,
    all_data2 = data$`svítivost při teplotě 5 °C (lm)`,
    composition = data$výrobce,
    data1 = data$`svítivost při teplotě 22 °C (lm)`[data$výrobce ==
                                                      "Amber"],
    data2 = data$`svítivost při teplotě 5 °C (lm)`[data$výrobce == "Amber"],
    nazvy_sloupcu = c(" 22 °C ", " 5 °C "),
    name_classes = " výrobců ",
    data_names = c(" 22 °C ", " 5 °C "),
    dataset_name = " Amber ",
    velicina = " Svetelny tok [lm] "
  )
  
  ########################### Ukol 2 ###############################################
  source("uloha2.R")
  data1 = data$`svítivost při teplotě 22 °C (lm)`[data$`výrobce` == "Amber"] -
    data$`svítivost při teplotě 5 °C (lm)`[data$`výrobce` == "Amber"]
  data2 = data$`svítivost při teplotě 22 °C (lm)`[data$`výrobce` == "Bright"] -
    data$`svítivost při teplotě 5 °C (lm)`[data$`výrobce` == "Bright"]
  uloha2(
    filename = filename,
    c_ukolu = c_ukolu,
    output_data_dir = output_data_dir,
    data1 = data1,
    data2 = data2,
    typy = " pokles světelného toku výrobců ",
    sloupce = c(" Amber ", " Bright "),
    vyrobci = " výrobci "
  )
  ########################### Ukol 3 ###############################################
  source("uloha3.R")
  uloha3(
    filename = filename,
    c_ukolu = c_ukolu,
    output_data_dir = output_data_dir,
    hodnoty = data$`svítivost při teplotě 5 °C (lm)`,
    trizeni = data$výrobce,
    typy = " světelný tok v 5 °C ",
    vyrobci = " výrobci "
  )
  ########################### Ukol 4 ###############################################
  source("uloha4.R")
  
  hodnoty = data$`svítivost při teplotě 5 °C (lm)`
  trizeni = data$výrobce
  hodnoty_OP = hodnoty
  
  for (i in unique(trizeni)) {
    pom = boxplot(hodnoty_OP[trizeni == i])
    hodnoty_OP[(hodnoty_OP %in% pom$out) & (trizeni == i)] = NaN
  }
  trizeni_OP = trizeni[!is.nan(hodnoty_OP)]
  hodnoty_OP = na.omit(hodnoty_OP)
  hodnoty = hodnoty >= 800
  hodnoty_OP = hodnoty_OP >= 800
  
  uloha4(
    filename = filename,
    c_ukolu = c_ukolu,
    output_data_dir = output_data_dir,
    hodnoty = hodnoty,
    trizeni = trizeni,
    hodnoty_OP = hodnoty_OP,
    trizeni_OP = trizeni_OP,
    fail_name = " nedosažení deklarovaného světelného toku (800 lm) ",
    vyrobci = " výrobci "
  )
  
  ########################### Finishing and printing to PDF ########################
  write("</body>", file = filename, append = TRUE)
  write("</html>", file = filename, append = TRUE)
  
  setwd(paste0(output_data_dir, "du_", c_ukolu, "_img"))
  system(paste0("../", back_route_dir, "crop_pictures.sh"))
  setwd("..")
  setwd(back_route_dir)
}

