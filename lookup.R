# Copyright (C) 2018 Bailey Defino
# <https://bdefino.github.io>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# download indices

indices <- NULL

Download.Database <- function() {
  # attempt to download the database
  temp <- tempfile()
  
  download.file("http://www.nanpa.com/nanp1/allutlzd.zip", temp)
  
  database <- read.table(
    unz(temp, "allutlzd.txt"), fill = TRUE, header = TRUE,
    stringsAsFactors = FALSE, sep = "\t"
    )
  write.table(database, file = paste(getwd(), "allutlzd.txt", sep = "/"), sep = "\t")
  
  unlink(temp)
  
  return(database)
}

# download

try(indices <- Download.Database(), silent = TRUE)

if (is.null(indices)) {
  # unable to download
  
  indices <- read.table(
    "allutlzd.txt", fill = TRUE, header = TRUE,
    stringsAsFactors = FALSE, sep = "\t"
    )
}

Lookup <- function(num) {
  # gather location and other information on a phone number
  
  # establish variables
  
  info <- NULL
  
  # prepare number
  
  num <- Split(num)
  num <- paste(num[1], "-", num[2], sep = "")
  num <- gsub("NULL", "", num)
  
  # match location
  
  match <- indices[indices$NPA.NXX == num, ]
  line <- match[1, ]
  
  # get info
  
  info <- data.frame(
    State = line$State, City = line$RateCenter,
    Company = line$Company, Assigned = line$AssignDate
    )
  
  return(info)
}

Split <- function(phone = NULL) {
  # split phone number to its constituents
  
  # establish variables
  
  phone <- as.character(phone)
  
  split.code <- NULL
  
  places <- data.frame(c(1, 3), c(4, 6), c(7, 10))
  
  # split number
  
  for (p in places) {
    if (p[2] * -1 <= length(phone)) {
      first <- p[1]
      last <- p[2]
      
      split.code <- c(
        split.code, substr(phone, first, last)
        )
    }
  }
  
  return(split.code)
}
