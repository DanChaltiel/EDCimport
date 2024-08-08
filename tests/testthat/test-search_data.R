

#transform the snapshot so that the temp paths are consistent
transform_temp_path = function(x){
  x %>% str_replace_all(path_temp(), "CURRENT_PATH_TEMP")
}



test_that("search_for_newer_data() works", {
  
  expect_snapshot({
    archive = "data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_06_01_12_00.zip"
    newer_archive1 = "MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_07_01_12_00.zip"
    newer_archive2 = "MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip"
    
    pt = path_temp("search_for_newer_data")
    dir_create(path(pt, "data"))
    dir_create(path(pt, "downloads"))
    
    fs::file_create(path(pt, "data", basename(archive)))
    fs::file_create(path(pt, "data", basename(newer_archive1)))
    fs::file_create(path(pt, "downloads", basename(newer_archive2)))
    
    fs::dir_tree(pt)
    
    source = path(pt, c("data","downloads"))
    target = path(pt, "data")
    
    #normal case: more recent in downloads
    search_for_newer_data(archive, source=source, target=target, ask=2)
    
    #alternative: more recent already in data
    search_for_newer_data(archive, source=target, target=target, ask=2)
    
    #skip: no project in source
    search_for_newer_data("YOURPROJECT", source=target, target=target, ask=2)
    search_for_newer_data(archive, source=c(source, "foobar"), target=target, ask=2)
    
    #normal case, with copy
    search_for_newer_data(archive, source=source, target=target, ask=1)
    fs::dir_tree(pt)
    

    fs::dir_delete(pt) #cleaning
  }, 
  transform = transform_temp_path)
  
})
