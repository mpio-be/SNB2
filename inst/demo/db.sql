

-- Dumping structure for table SNBatWESTERHOLZ_v2.b080
CREATE TABLE IF NOT EXISTS b080 (
  datetime_ datetime DEFAULT NULL,
  sensor_value varchar(16) DEFAULT NULL COMMENT 'Transponder ID and Light barrier state',
  sensor varchar(3) DEFAULT NULL COMMENT 'sensor type: trp = transponder, lbi = internal light barrier, lbo = outer ligt barrier',
  id int(10) DEFAULT NULL COMMENT 'Datafile id in the file status table',
  r_pk bigint(20) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (r_pk),
  KEY datetime_ (datetime_),
  KEY transp (sensor_value),
  KEY sensor (sensor)
) ENGINE=Aria DEFAULT CHARSET=latin1 PAGE_CHECKSUM=1;


-- Dumping structure for table SNBatWESTERHOLZ_v2.b081
CREATE TABLE IF NOT EXISTS b081 (
  datetime_ datetime DEFAULT NULL,
  sensor_value varchar(16) DEFAULT NULL COMMENT 'Transponder ID and Light barrier state',
  sensor varchar(3) DEFAULT NULL COMMENT 'sensor type: trp = transponder, lbi = internal light barrier, lbo = outer ligt barrier',
  id int(10) DEFAULT NULL COMMENT 'Datafile id in the file status table',
  r_pk bigint(20) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (r_pk),
  KEY datetime_ (datetime_),
  KEY transp (sensor_value),
  KEY sensor (sensor)
) ENGINE=Aria DEFAULT CHARSET=latin1 PAGE_CHECKSUM=1;


-- Dumping structure for table SNBatWESTERHOLZ_v2.b082
CREATE TABLE IF NOT EXISTS b082 (
  datetime_ datetime DEFAULT NULL,
  sensor_value varchar(16) DEFAULT NULL COMMENT 'Transponder ID and Light barrier state',
  sensor varchar(3) DEFAULT NULL COMMENT 'sensor type: trp = transponder, lbi = internal light barrier, lbo = outer ligt barrier',
  id int(10) DEFAULT NULL COMMENT 'Datafile id in the file status table',
  r_pk bigint(20) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (r_pk),
  KEY datetime_ (datetime_),
  KEY transp (sensor_value),
  KEY sensor (sensor)
) ENGINE=Aria DEFAULT CHARSET=latin1 PAGE_CHECKSUM=1;


-- Dumping structure for table SNBatWESTERHOLZ_v2.boxid
CREATE TABLE IF NOT EXISTS boxid (
  box int(11) NOT NULL,
  hwid varchar(50) NOT NULL COMMENT 'hardware id',
  datetime_ datetime NOT NULL,
  action varchar(50) NOT NULL COMMENT 'start or stop',
  pk int(11) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY box (box),
  KEY hwid (hwid)
) ENGINE=InnoDB AUTO_INCREMENT=139 DEFAULT CHARSET=latin1 COMMENT='box-hardware id correspondence';



INSERT INTO boxid (box, hwid, datetime_, action) VALUES (80, '0x24F8A30457516E5C', '2017-02-11 12:48:00', 'start');
INSERT INTO boxid (box, hwid, datetime_, action) VALUES (81, '0x2493440157516E84', '2017-02-03 13:24:00', 'start');
INSERT INTO boxid (box, hwid, datetime_, action) VALUES (82, '0x247DBC0257516E5C', '2017-02-11 12:21:00', 'start');



-- Dumping structure for table SNBatWESTERHOLZ_v2.file_status
CREATE TABLE IF NOT EXISTS file_status (
  box smallint(3) unsigned NOT NULL,
  id int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'id in each box table.',
  path varchar(255) NOT NULL COMMENT 'rootdir /ds/raw_data_kemp/FIELD/Westerholz/SNB/RAWDATA',
  filesize mediumint(10) unsigned NOT NULL COMMENT 'in KB',
  author varchar(4) DEFAULT NULL COMMENT 'who downloaded the SD card (Updated through SNB interface)',
  datetime_ datetime DEFAULT NULL COMMENT 'datetime when the SDcard got pulled',
  upload_status tinyint(2) DEFAULT NULL COMMENT '0 = not uploaded, 1 = clean load, -1 = load unsuccesful',
  dt_loaded datetime DEFAULT NULL COMMENT 'when the data got transferred to the database',
  remarks varchar(255) DEFAULT NULL,
  PRIMARY KEY (id),
  KEY box (box),
  KEY box_w_date (box,datetime_),
  KEY date_ (datetime_)
) ENGINE=InnoDB AUTO_INCREMENT=39 DEFAULT CHARSET=latin1;

