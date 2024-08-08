# search_for_newer_data() works

    Code
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
    Output
      CURRENT_PATH_TEMP/search_for_newer_data
      +-- data
      |   +-- MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_06_01_12_00.zip
      |   \-- MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_07_01_12_00.zip
      \-- downloads
          \-- MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip
    Code
      source = path(pt, c("data", "downloads"))
      target = path(pt, "data")
      search_for_newer_data(archive, source = source, target = target, ask = 2)
    Message
      ! There is a database in 'CURRENT_PATH_TEMP/search_for_newer_data/downloads' that is 61 days more recent than the current extraction (2024-08-01 vs 2024-06-01).
      Run the following code to copy it to `target`:
      file.copy(
        "CURRENT_PATH_TEMP/search_for_newer_data/downloads/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip",
        "CURRENT_PATH_TEMP/search_for_newer_data/data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip",
        "copy.date=TRUE", 
      )
    Code
      search_for_newer_data(archive, source = target, target = target, ask = 2)
    Message
      ! There is already a database in 'CURRENT_PATH_TEMP/search_for_newer_data/data' that is 30 days more recent than the current extraction (2024-07-01 vs 2024-06-01).
      You might want to update your code to import the newer database.
      i Newer filename: "MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_07_01_12_00.zip"
    Code
      search_for_newer_data("YOURPROJECT", source = target, target = target, ask = 2)
    Message
      No project files were found in `source`.
    Code
      search_for_newer_data(archive, source = c(source, "foobar"), target = target,
      ask = 2)
    Message
      ! There is a database in 'CURRENT_PATH_TEMP/search_for_newer_data/downloads' that is 61 days more recent than the current extraction (2024-08-01 vs 2024-06-01).
      Run the following code to copy it to `target`:
      file.copy(
        "CURRENT_PATH_TEMP/search_for_newer_data/downloads/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip",
        "CURRENT_PATH_TEMP/search_for_newer_data/data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip",
        "copy.date=TRUE", 
      )
    Code
      search_for_newer_data(archive, source = source, target = target, ask = 1)
    Message
      ! There is a database in 'CURRENT_PATH_TEMP/search_for_newer_data/downloads' that is 61 days more recent than the current extraction (2024-08-01 vs 2024-06-01).
      v New file copied, change your code to
      'CURRENT_PATH_TEMP/search_for_newer_data/data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip'
    Code
      fs::dir_tree(pt)
    Output
      CURRENT_PATH_TEMP/search_for_newer_data
      +-- data
      |   +-- MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_06_01_12_00.zip
      |   +-- MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_07_01_12_00.zip
      |   \-- MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip
      \-- downloads
          \-- MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_08_01_12_00.zip
    Code
      fs::dir_delete(pt)

