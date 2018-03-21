# devtools::test()

# ==========================================================================
# pipe line tests
# ==========================================================================
require(SNB2)
require(data.table)
assignInNamespace("cedta.override", c(data.table:::cedta.override,"SNB2"), "data.table")


context("📌 txt to db")

test_that("txt to db pipeline elements work", {

    # settings

        install_demo_SNB(user = 'testuser')

        p    = getOption("path.to.raw_v2")
        db   = getOption("snbDB_v2")
        user = getOption("DB_user")
        host = getOption("host")
        y   = year(Sys.Date())

        con = dbcon(user = user , host = host)


    # tests

         # expect_true( well_formated_directory(p, y) )

         x = incoming_files(con, p, y, db)

         expect_is(x, 'data.table')
         expect_equal( nrow(x), 3)

         expect_true(file_status_update1(con, x))

         expect_null(incoming_files(con, p, y, db))

         h = hot_files(con, p, db)

         expect_is(h, 'data.table')
         expect_equal( nrow(h), 3)

         dat = load_clean_txt_v2(h)

         aa = lapply(dat, attr, which = 'snb')
         expect_true ( sapply(aa, function(x) inherits(x, 'data.table') ) %>% all )


         update_bTables(con, dat, db = db)

         expect_equal( nrow(hot_files(con, p, db) ), 0   )


        ii = dbq(con, 'select id from file_status')$id[ c(1,3)]

        drop_by_id( con, ii )


         
         dbDisconnect(con); rm(con)


    })

test_that("txt to db pipeline runs through", {

    # settings
        install_demo_SNB(user = 'testuser')
    # pipeline
        expect_true( scidb_snbUpdater() )
        expect_error( scidb_snbUpdater() ) # second time when called 
        

    })    


test_that("drop_and_reload drops and reload properly", {

    # settings
        install_demo_SNB(user = 'testuser')
        scidb_snbUpdater()

    # pipeline
        con = dbcon(user = getOption("DB_user") , host = getOption("host"), db = getOption("snbDB_v2") )

        ii = dbq(con, 'select id from file_status')$id[ c(1,3)]

        drop_and_reload( con, ii )

        dbDisconnect(con)

    })    