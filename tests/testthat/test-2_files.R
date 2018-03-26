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
    
    # ok file
    o = read_boxtxt(system.file('test_files_SNB', '80', 'BOX0080.TXT', package = 'SNB2'))

    expect_s3_class(o,'data.table')

    # empty file
    o = read_boxtxt(system.file('test_files_SNB', '83', 'BOX0083.TXT', package = 'SNB2'))
    expect_equal(attributes(o)$SNB2$garbage, 1) # empty files are 100% garbage

    # only garbage file
    o = read_boxtxt(system.file('test_files_SNB', '84', 'BOX0084.TXT', package = 'SNB2'))
    expect_equal(attributes(o)$SNB2$garbage, 1) # empty files are 100% garbage

    # lots of garbage file
    o = read_boxtxt(system.file('test_files_SNB', '85', 'BOX0085.TXT', package = 'SNB2'))
    expect_equal(attributes(o)$SNB2$garbage, 0.08) #  #prop garbage (from the total of possibly good lines)







    })
