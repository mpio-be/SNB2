# ====================================================================================
# test against MariaDB
# User and db should be in place
#   CREATE USER 'testuser'@'localhost' ;
#   UPDATE mysql.user SET Password=PASSWORD('cs') WHERE User='testuser' AND Host='localhost' ;
#   GRANT ALL  ON SNB_demo.* TO 'testuser'@'localhost' ;
#   FLUSH PRIVILEGES ;
#   sdb::saveCredentials('testuser', 'cs', '127.0.0.1') 
# ====================================================================================


context("📌 demo db")

test_that("Demo db and raw data can be installed locally", {
    
    user = 'testuser'
    host =  '127.0.0.1'
    db = 'SNB_demo'
    rawdata_root = paste(tempdir(), 'SNB_demo_RAWDATA', sep = '/')

    expect_true( install_demo_SNB(user,host, db = db, rawdata_root = rawdata_root) )

    expect_identical(getOption('DB_user') , user)
    expect_identical(getOption('host') , host)
    expect_identical(getOption('snbDB_v2') , db)
    expect_identical(getOption('path.to.raw_v2') , rawdata_root)



    })







