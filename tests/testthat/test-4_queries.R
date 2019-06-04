# ==========================================================================
# user queries
# ==========================================================================

require(sdb)
require(SNB2)
assignInNamespace("cedta.override", c(data.table:::cedta.override,"SNB2"), "data.table")

demo_setup(install.test.db = TRUE, admin.user = 'mihai')
scidb_snbUpdater()

context(" user queries")

test_that("user queries work.", {

    # tests
    
        x = dbqSNB(username =  getOption('DB_user'), host = getOption("host"), .boxes = getOption("boxes_v2"),
                q = 'select * from boxtables')
        expect_is(x, 'data.table')
        

        expect_warning(harwareIDs() )
        
        # no data thus warning 
        expect_warning( 
            dbqSNB(username = getOption('DB_user'), host = getOption("host"),  .boxes = getOption("boxes_v2"),
                q = paste('select * from boxtables where datetime_ > ', shQuote(Sys.Date())) )
            )
       
    })
