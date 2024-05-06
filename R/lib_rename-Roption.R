# Centralized Library and Functions for the 01-A-renamefiles-Roption Rmd. 

library(stringr)
library(exifr)
if(!require(googledrive)) install.packages("googledrive")
library(googledrive)


#use: rename_cameraimage(folder_dir = folder_id, new_dir = new_img_folder, googledrive_dir = TRUE)

##Function
#folder directory and new directory must be either a file path if using a local directory, or a google drive folder id if using a google drive account (as well as the parameter googledrive_dir = TRUE). 

#' @param googledrive_dir Is the image directory and the desired directory of the renamed image within google drive account? 
#' @param folder_dir The folder directory where original images are, if googledrive_dir == TRUE, this will be the google drive folder id. 
#' @param new_dir The directory where the renamed image should be moved to, the default is the same folder as the original is
#'  if googledrive_dir == TRUE, this will be the folder id of the new directory
#' @return dataframe with original file basename, updated file name, and watershed id

rename_cameraimage <- function(folder_dir, new_dir = folder_dir, googledrive_dir = FALSE){#can specfiy new directory if desired, otherwise same
  
  #For any google drive directory 
  if(googledrive_dir == TRUE){
    
    #list all images to be renamed from initiated folder
    file_ids <- if(nrow(drive_ls(folder_dir)) == 1) {
      drive_ls(drive_ls(folder_dir)) } else drive_ls(folder_dir)  #twice as there is sometimes a SD card name as a subfolder. 
    #having watershed id here, allows a double check, seems like it was not carrying over
    watershed_id = tolower(str_sub(drive_reveal(folder_dir, "path")$name, 1, 2))
    
    rename_list <- lapply(file_ids$id, function(gd_id){
      # Exif Cant read GoogleDrive Path
      gd_name <- googledrive::as_dribble(gd_id)$name
      
      dl <- drive_download(gd_id, path = temp, overwrite = TRUE)
      datetime_taken = str_replace(str_remove_all(read_exif(dl$local_path)$CreateDate, ":"), " ", "_")
      
      new_filename = paste0("Hbwtr_", watershed_id, "_", datetime_taken, ".jpg")
      
      #move and rename
      file <- drive_mv(gd_id, path = new_dir, name = new_filename)
      
      #output dataframe with how renamed for review                 
      output <- data.frame(rename = new_filename, original= gd_name, googledrive_id = gd_id, watershed = watershed_id) 
    })
  }else{ #local file structure
    
    #Utilizes the folder structure to extract watershed id                     
    watershed_id = tolower(str_sub(basename(normalizePath(folder_dir)), 1, 2))
    #list all images to be renamed from initiated folder
    rename_list <- lapply(list.files(path = folder_dir), function(img){
      
      
      datetime_taken = str_replace(str_remove_all(read_exif(paste(folder_dir,img, sep="/"))$CreateDate, ":"), " ", "_")
      new_filename = paste0("Hbwtr_", watershed_id, "_", datetime_taken, ".jpg")
      
      
      ## now rename file to new directory
      #file.copy(file.path(folder_dir, img), file.path(new_dir, new_filename))
      #OR
      #this renames and moves file
      file.rename(file.path(folder_dir, img), file.path(new_dir, new_filename))
      
      output <- tibble(rename = new_filename, original= img, watershed = watershed_id) #output dataframe with how renamed for review
    })
  }
  
  output_filenames <- do.call(rbind, rename_list)#output dataframe with original id and new
  
  return(output_filenames)
}