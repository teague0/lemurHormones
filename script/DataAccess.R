library(tidyverse)
library(googlesheets4)

andro <- read_sheet("https://docs.google.com/spreadsheets/d/1Pfvn7ZOvgDL3BhvbbMCHLkb5xEueShSqGfUXINNaVjs/edit?usp=sharing")
e2 <- read_sheet("https://docs.google.com/spreadsheets/d/1GvFdXgXPYG89bpX8NrkC6UZo5aONd2_xOyqho3SOIpU/edit?usp=sharing")
gc <- read_sheet("https://docs.google.com/spreadsheets/d/12j8bzWNEdZ3SdTUqrbkXAa5Sb-JFNn25EkFakMO-G2E/edit?usp=sharing")
sampleInfo <- read_sheet("https://docs.google.com/spreadsheets/d/1yLeZAETb_u4z72rcgorikMvBX4d6Cb6eLVATOkl9O9U/edit?usp=sharing")
demos <- read_sheet("https://docs.google.com/spreadsheets/d/13jCF6xD0UBmJJ08J2d6z-4xHEV2_L5NY91MaNBlppuQ/edit?usp=sharing", sheet = "ID & Demo")
reproState <- read_sheet("https://docs.google.com/spreadsheets/d/13jCF6xD0UBmJJ08J2d6z-4xHEV2_L5NY91MaNBlppuQ/edit?usp=sharing", sheet = "IDs Repro Stage")
sampleWeek <- read_sheet("https://docs.google.com/spreadsheets/d/13jCF6xD0UBmJJ08J2d6z-4xHEV2_L5NY91MaNBlppuQ/edit?usp=sharing", sheet = "fecal study weeks")

aggMark <- read_sheet("https://docs.google.com/spreadsheets/d/1t4rXq04tns6TUvjkL8m5a8mtZsi5SA6bNBLzbrY9zmU/edit?usp=sharing")


