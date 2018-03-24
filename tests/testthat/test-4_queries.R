# ==========================================================================
# user queries
# ==========================================================================

require(SNB2)
assignInNamespace("cedta.override", c(data.table:::cedta.override,"SNB2"), "data.table")


context(" user queries")

test_that("user queries work.", {

    # tests
        con = dbcon(user = getOption('DB_user') , host = getOption("host"), db = getOption("snbDB_v2"))
       
        x = dbqSNB(user =  getOption('DB_user'), host = getOption("host"), ncores = 2, .boxes = 80:82,
                q = 'select * from boxtables')
        expect_is(x, 'data.table')
        

        expect_is(harwareIDs() , 'data.table')
        
        # no data thus warning 
        expect_warning( 
            dbqSNB(user = getOption('DB_user'), host = getOption("host"), ncores = 2,  .boxes = 80:82,
                q = paste('select * from boxtables where datetime_ > ', shQuote(Sys.Date())) )
            )
       
    })
