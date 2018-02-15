

# devtools::test()

# ==========================================================================
# user queries
# ==========================================================================
require(SNB2)
require(data.table)
assignInNamespace("cedta.override", c(data.table:::cedta.override,"SNB2"), "data.table")


context("💁 user queries")

test_that("dbqSNB is indeed a worthy of its name.", {

    # settings
        install_demo_SNB(user = 'testuser')
        con = dbcon(user = 'snbAdmin' , host = getOption("host"), db = getOption("snbDB_v2"))
        scidb_snbUpdater(con)
        dbDisconnect(con); rm(con)

    # tests
        con = dbcon(user = 'snbAdmin' , host = getOption("host"), db = getOption("snbDB_v2"))
       
        x = dbqSNB(user = user, host = host, ncores = 2, .boxes = 80:82,
                q = 'select * from boxtables')
        expect_is(x, 'data.table')
        
        # no data
        expect_warning( 
            dbqSNB(user = user, host = host, ncores = 2,  .boxes = 80:82,
                q = paste('select * from boxtables where datetime_ > ', shQuote(Sys.Date())) )
            )
       
    })
