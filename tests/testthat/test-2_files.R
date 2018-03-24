# ==========================================================================
# tests that  require a set of files and a given file hierarchy
# ==========================================================================



test_that("Demo setup ini", {
    
    expect_true( demo_setup() )

    expect_identical(getOption('DB_user') , user)
    expect_identical(getOption('host') , host)
    expect_identical(getOption('snbDB_v2') , db)
    expect_identical(getOption('path.to.raw_v2') , rawdata_root)

    })

test_that("repo checks", {
    
    expect_true( 
        well_formated_directory(getOption("path.to.raw_v2"), year(Sys.Date()) ) 
        )
  
    })


test_that("data path structure", {
    
    o = data_dirs()
    expect_s3_class(o, 'data.table')
    

    })

test_that("File readers and parsers", {
    
    o = data_dirs()
    expect_s3_class(o, 'data.table')
    # readRaw_v2
    # read_snb_txt_v2
    #hwid


    })
