-- mysql -p <github/mpio-be/SNB2/tests/testdb.sql

DROP USER 'testuser'@'localhost';
CREATE USER 'testuser'@'localhost' IDENTIFIED BY  'cs';

DROP DATABASE IF EXISTS `tests`;
CREATE DATABASE `tests`;
GRANT ALL ON tests.* TO 'testuser'@'localhost';
FLUSH PRIVILEGES ;


USE `tests`;


CREATE TABLE IF NOT EXISTS b080 (
  datetime_ datetime DEFAULT NULL,
  sensor_value varchar(16) DEFAULT NULL COMMENT 'Transponder ID and Light barrier state',
  sensor varchar(3) DEFAULT NULL COMMENT 'sensor type: tra = transponder, lbi = internal light barrier, lbo = outer light barrier',
  path varchar(255) NOT NULL COMMENT 'rootdir /ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2',
  r_pk bigint(20) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (r_pk),
  KEY datetime_ (datetime_),
  KEY transp (sensor_value),
  KEY sensor (sensor)
) ENGINE=Aria ;


CREATE TABLE IF NOT EXISTS b081 (
  datetime_ datetime DEFAULT NULL,
  sensor_value varchar(16) DEFAULT NULL COMMENT 'Transponder ID and Light barrier state',
  sensor varchar(3) DEFAULT NULL COMMENT 'sensor type: tra = transponder, lbi = internal light barrier, lbo = outer light barrier',
  path varchar(255) NOT NULL COMMENT 'rootdir /ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2',
  r_pk bigint(20) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (r_pk),
  KEY datetime_ (datetime_),
  KEY transp (sensor_value),
  KEY sensor (sensor)
) ENGINE=Aria ;


CREATE TABLE IF NOT EXISTS b082 (
  datetime_ datetime DEFAULT NULL,
  sensor_value varchar(16) DEFAULT NULL COMMENT 'Transponder ID and Light barrier state',
  sensor varchar(3) DEFAULT NULL COMMENT 'sensor type: tra = transponder, lbi = internal light barrier, lbo = outer light barrier',
  path varchar(255) NOT NULL COMMENT 'rootdir /ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2',
  r_pk bigint(20) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (r_pk),
  KEY datetime_ (datetime_),
  KEY transp (sensor_value),
  KEY sensor (sensor)
) ENGINE=Aria ;



CREATE TABLE IF NOT EXISTS b083 (
  datetime_ datetime DEFAULT NULL,
  sensor_value varchar(16) DEFAULT NULL COMMENT 'Transponder ID and Light barrier state',
  sensor varchar(3) DEFAULT NULL COMMENT 'sensor type: tra = transponder, lbi = internal light barrier, lbo = outer light barrier',
  path varchar(255) NOT NULL COMMENT 'rootdir /ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2',
  r_pk bigint(20) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (r_pk),
  KEY datetime_ (datetime_),
  KEY transp (sensor_value),
  KEY sensor (sensor)
) ENGINE=Aria ;


CREATE TABLE IF NOT EXISTS b084 (
  datetime_ datetime DEFAULT NULL,
  sensor_value varchar(16) DEFAULT NULL COMMENT 'Transponder ID and Light barrier state',
  sensor varchar(3) DEFAULT NULL COMMENT 'sensor type: tra = transponder, lbi = internal light barrier, lbo = outer light barrier',
  path varchar(255) NOT NULL COMMENT 'rootdir /ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2',
  r_pk bigint(20) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (r_pk),
  KEY datetime_ (datetime_),
  KEY transp (sensor_value),
  KEY sensor (sensor)
) ENGINE=Aria ;


CREATE TABLE IF NOT EXISTS b085 (
  datetime_ datetime DEFAULT NULL,
  sensor_value varchar(16) DEFAULT NULL COMMENT 'Transponder ID and Light barrier state',
  sensor varchar(3) DEFAULT NULL COMMENT 'sensor type: tra = transponder, lbi = internal light barrier, lbo = outer light barrier',
  path varchar(255) NOT NULL COMMENT 'rootdir /ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2',
  r_pk bigint(20) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (r_pk),
  KEY datetime_ (datetime_),
  KEY transp (sensor_value),
  KEY sensor (sensor)
) ENGINE=Aria ;


CREATE TABLE IF NOT EXISTS boxid (
  box int(11) NOT NULL,
  hwid varchar(50) NOT NULL COMMENT 'hardware id',
  datetime_ datetime NOT NULL,
  action varchar(50) NOT NULL COMMENT 'start or stop',
  pk int(11) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY box (box),
  KEY hwid (hwid)
) ENGINE=InnoDB COMMENT='box-hardware id correspondence';



INSERT INTO boxid (box, hwid, datetime_, action) VALUES (80, '0x24F8A30457516E5C', '2017-02-11 12:48:00', 'start');
INSERT INTO boxid (box, hwid, datetime_, action) VALUES (81, '0x2493440157516E84', '2017-02-03 13:24:00', 'start');
INSERT INTO boxid (box, hwid, datetime_, action) VALUES (82, '0x247DBC0257516E5C', '2017-02-11 12:21:00', 'start');



CREATE TABLE IF NOT EXISTS black_list (
  path varchar(255) NOT NULL COMMENT 'rootdir /ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA_v2',
  pk int(10) unsigned NOT NULL AUTO_INCREMENT,
 
  PRIMARY KEY (pk)
)  ;



CREATE TABLE transponders (
site_type   int(2)        NOT NULL  COMMENT '1 = box, 2 = feeder',
site        int(6)        NOT NULL  COMMENT 'site ID: box or feeder ID',
transponder char(16)      NOT NULL  COMMENT 'transponder ID',
datetime_   datetime      NOT NULL  COMMENT 'CEST',
`path`      varchar(255)  NOT NULL ,
 pk          bigint       NOT NULL AUTO_INCREMENT,

PRIMARY KEY (pk),

KEY sitetype  (site_type)   ,
KEY site      (site)        ,
KEY transp    (transponder) ,
KEY datetime  (datetime_)   ,
KEY `path`      (`path`)        ,
KEY transponders_site_type (site_type,site,`path`)

) ENGINE=Aria