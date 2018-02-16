
# devtools::test()

# ==========================================================================
# butler elements
# ==========================================================================
require(SNB2)
require(data.table)
assignInNamespace("cedta.override", c(data.table:::cedta.override,"SNB2"), "data.table")


context("👳 butler")

test_that("butler has good tools.", {

    # settings

    # settings
        install_demo_SNB(user = 'testuser')
        con = dbcon( user = 'testuser' , host = getOption("host"), db = getOption("snbDB_v2") )
        scidb_snbUpdater(con, parallel = TRUE, ncores = 2)
        dbDisconnect(con); rm(con)

    # tests
        system_status() %>% expect_is( 'data.table')
        x = cardReader()  %>% expect_is( 'data.table')

        if(nrow(x) > 0)
            read.boxnumber(x$mountpoint[1], hwidCheck = FALSE)  %>% expect_is( 'data.table')

        x = file_copy_status(hwidCheck = FALSE) %>% expect_is( 'data.table')
        
         if(nrow(x) > 0) {
          sdcard_uploader(x)
          expect_error( sdcard_uploader(x) )
            
         }   






       
    })
