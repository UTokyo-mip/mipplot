## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(mipplot)

## ------------------------------------------------------------------------
# mipplot �p�b�P�[�W���̃t�@�C�����g�����߂̓��ʂȃR�[�h�ł��B
# ���Ȃ����f�[�^ �t�@�C�����������ł���΁A���̃R�[�h�͕K�v����܂���B
# ���̃R�[�h�́Amipplot �p�b�P�[�W���̃f�[�^ �t�@�C���̃p�X���擾���܂��B
data_file_path <- system.file("mipplot", "iamc15_sample_data.csv", package = "mipplot") 

## ------------------------------------------------------------------------
iamc_data <- mipplot::mipplot_read_iamc(data_file_path)

## ------------------------------------------------------------------------
iamc_data$model %>% levels

## ------------------------------------------------------------------------
iamc_data$scenario %>% levels

## ------------------------------------------------------------------------
iamc_data$region %>% levels()

## ------------------------------------------------------------------------
iamc_data %>% dplyr::select(variable, unit)  %>% dplyr::distinct()

## ------------------------------------------------------------------------
iamc_data %>% 
  dplyr::filter(stringr::str_detect(scenario, "^SSP2")) %>%
  dplyr::select(model, scenario) %>% dplyr::distinct()

## ------------------------------------------------------------------------
iamc_data %>% 
  dplyr::filter(stringr::str_detect(variable, "Final Energy\\|Industry\\|")) %>% 
  dplyr::select(variable) %>%
  dplyr::distinct()

## ----fig1, fig.height = 4, fig.width = 7---------------------------------
iamc_data %>%
  dplyr::filter( model %in% c("AIM/CGE 2.0", "GCAM 4.2") ) %>%
  dplyr::filter(2000 <= period) %>%
  dplyr::filter(period <= 2100) %>%
  mipplot_line(
    variable = c("Emissions|CO2"),
    scenario = c("SSP1-19", "SSP1-26", "SSP1-34", "SSP1-45"),
    region = c("World"),
    legend = TRUE)
