
# ==========================================================================
# tests for context independent functions
# ==========================================================================


context(" simple")

test_that("functions working on a file paths", {


    o = path2year("/ds/raw_data_kemp/FIELD/Westerholz/SNB_v2/RAWDATA/2016/2016.01.26/004/BOX001.TXT")
    expect_equal(o, 2016)

    o = path2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB_v2/RAWDATA/2016/2016.01.26/011/BOX001.TXT")
     expect_equal(o, 11)

    o  = basename2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA/2016/2016.01.26/011/BOX0101.TXT")
     expect_equal(o, 101)

    })

test_that("char functions", {

    expect_equal(int2b(1), 'b001')

    })




test_that("regexp-based functions", {

    o = snbstring2date_v2(x = '20170418-171742.202 Transponder: 4B76C4B43A6F0001')
    expect_equal(o, as.POSIXct("2017-04-18 17:17:42.201 CEST"))


    expect_s3_class(
        char2date(x = '2015/2015.01.26/001/box001.txt'),
        'Date')


    })


test_that("data constructor functions", {

    o = boxes()
    expect_s3_class(o, 'data.table')

    })


