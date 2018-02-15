
# ==========================================================================
# tests for all functions taking 'simple' arguments
# ==========================================================================


context("📌 simple")

test_that("functions working on a raw data file path return what they should", {


    o = path2year("/ds/raw_data_kemp/FIELD/Westerholz/SNB_v2/RAWDATA/2016/2016.01.26/004/BOX001.TXT")
    expect_equal(o, 2016)

    o = path2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB_v2/RAWDATA/2016/2016.01.26/011/BOX001.TXT")
     expect_equal(o, 11)

    o  = basename2box("/ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA/2016/2016.01.26/011/BOX0101.TXT")
     expect_equal(o, 101)


    })


test_that("regexp functions work as they should", {


    o = snbstring2date_v2(x = '20170418-171742.202 Transponder: 4B76C4B43A6F0001')
    expect_equal(o, as.POSIXct("2017-04-18 17:17:42.201 CEST"))




    })