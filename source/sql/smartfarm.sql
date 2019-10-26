
/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;


# Dump of table auth
# ------------------------------------------------------------

DROP TABLE IF EXISTS `auth`;

CREATE TABLE `auth` (
  `aid` bigint(20) NOT NULL AUTO_INCREMENT,
  `client_id` bigint(20) DEFAULT '0',
  `ref_id` tinyint(4) DEFAULT '0',
  `user_id` bigint(20) DEFAULT '0',
  `token` varchar(70) DEFAULT NULL,
  `expired` datetime DEFAULT NULL,
  PRIMARY KEY (`aid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table issues
# ------------------------------------------------------------

DROP TABLE IF EXISTS `issues`;

CREATE TABLE `issues` (
  `iid` bigint(20) NOT NULL AUTO_INCREMENT,
  `station_id` bigint(20) DEFAULT '0',
  `node_id` bigint(20) DEFAULT '0',
  `date` datetime DEFAULT NULL,
  `activity` varchar(45) DEFAULT NULL,
  `type_id` tinyint(4) DEFAULT '0' COMMENT '0: general\n',
  `level_id` tinyint(4) DEFAULT '0',
  `description` text,
  `status_id` tinyint(4) DEFAULT '1',
  PRIMARY KEY (`iid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table node_history
# ------------------------------------------------------------

DROP TABLE IF EXISTS `node_history`;

CREATE TABLE `node_history` (
  `nhid` bigint(20) NOT NULL AUTO_INCREMENT,
  `slug` varchar(10) DEFAULT NULL,
  `date` datetime DEFAULT NULL,
  `station_id` bigint(20) DEFAULT '0',
  `node_id` bigint(20) NOT NULL DEFAULT '0',
  `state` tinyint(4) DEFAULT NULL,
  `value` double DEFAULT NULL,
  `activity` varchar(45) DEFAULT NULL,
  `description` text,
  `options` text,
  `status_id` tinyint(4) DEFAULT '1',
  PRIMARY KEY (`nhid`),
  KEY `fk_device_history_devices1_idx` (`node_id`),
  CONSTRAINT `fk_device_history_devices1` FOREIGN KEY (`node_id`) REFERENCES `nodes` (`nid`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;



# Dump of table nodes
# ------------------------------------------------------------

DROP TABLE IF EXISTS `nodes`;

CREATE TABLE `nodes` (
  `nid` bigint(20) NOT NULL AUTO_INCREMENT,
  `slug` varchar(10) DEFAULT NULL,
  `station_id` bigint(20) NOT NULL DEFAULT '0',
  `name` varchar(45) DEFAULT NULL,
  `type` tinyint(4) DEFAULT '0' COMMENT '0: general\n1: switch\n2: variable generic\n3: device with timer',
  `location` varchar(150) DEFAULT NULL,
  `state` tinyint(4) DEFAULT '1' COMMENT '0: on\n1: off\n2: ~\n',
  `value` double DEFAULT '0',
  `options` text,
  `schedules` text,
  `temperature_average` double DEFAULT NULL,
  `humidity_average` double DEFAULT NULL,
  `status_id` tinyint(4) DEFAULT '1' COMMENT '0: active\n1: inactive',
  PRIMARY KEY (`nid`),
  KEY `fk_devices_users_idx` (`station_id`),
  CONSTRAINT `fk_devices_users` FOREIGN KEY (`station_id`) REFERENCES `stations` (`sid`) ON DELETE NO ACTION ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `nodes` WRITE;
/*!40000 ALTER TABLE `nodes` DISABLE KEYS */;

INSERT INTO `nodes` (`nid`, `slug`, `station_id`, `name`, `type`, `location`, `state`, `value`, `options`, `schedules`, `temperature_average`, `humidity_average`, `status_id`)
VALUES
	(1,'SWwxW23',1,'Mawar Merah',0,'Ladang Hidroponik',0,0,'{\n  \"suhu\":\n    {\n      \"state\": 0,\n      \"value\": 30\n    },\n  \"kelembaban\":\n    {\n      \"state\": 0,\n      \"value\": 55\n    },\n  \"sprinkler\":\n    {\n      \"state\": 0\n    }\n}',NULL,NULL,NULL,0),
	(2,'735Xaw2',1,'Pasir Putih',1,'Kebun Sehat',1,1,'{ \"suhu\" : 31 }','{\"a\":\"A\"}',NULL,NULL,0),
	(3,'qsw345sxP',1,'Bata Merah',3,'Duren Bangkok Field',0,10,'{ \"devices\" : { \"suhu\" : { \"state\" : 0, \"date\" : \"2019-10-27 01:31:32\", \"value\" : 31 }, \"kelembaban\" : { \"value\" : 77, \"state\" : 0, \"date\" : \"2019-10-27 01:31:14\" }, \"sprinkle\" : { \"state\" : 1, \"date\" : \"2019-10-27 01:31:14\" } } }','{}',28.6875669638245,74.937668431147,0),
	(4,'Xqw345S',3,'Anggrek',3,'Farm #1',0,0,'{}',NULL,NULL,NULL,0),
	(5,'84urfPh',3,'Melati',0,'Kandang Bayam Utara #1',0,0,'{\"suhu\":32}',NULL,NULL,NULL,0),
	(6,'34Xx70b',4,'Gudeg',0,'Kandang Bayam Utara #1',1,0,NULL,NULL,NULL,NULL,0),
	(7,'pkKr214',4,'Bakpia',3,'Pekarangan',1,3,'{\"suhu\":{\"state\": 0,\"value\": 30},\"kelembaban\":{\"state\": 0,\"value\": 55},\"sprinkler\":{\"state\": 1}}',NULL,NULL,NULL,0),
	(8,'blkp22Q',4,'Bakpao',0,NULL,1,0,'{}','{}',NULL,NULL,0),
	(9,'smgWs234',2,'Bayam Baler',0,NULL,0,0,'{ \"devices\" : { \"suhu\" : { \"value\" : 21, \"state\" : 0, \"date\" : \"2019-10-24 16:16:22\" }, \"kelembaban\" : { \"value\" : 77, \"state\" : 0, \"date\" : \"2019-10-24 16:16:22\" }, \"sprinkle\" : { \"state\" : 1, \"date\" : \"2019-10-24 16:16:22\" } } }',NULL,NULL,NULL,0),
	(10,'smGas24',2,'Wortel',0,NULL,1,0,'{}',NULL,NULL,NULL,0),
	(11,'SMgw2GrtX',2,'Mentimuun',0,NULL,1,0,'{ \"suhu\" : 32 }',NULL,NULL,NULL,0);

/*!40000 ALTER TABLE `nodes` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table stations
# ------------------------------------------------------------

DROP TABLE IF EXISTS `stations`;

CREATE TABLE `stations` (
  `sid` bigint(20) NOT NULL AUTO_INCREMENT,
  `user_id` bigint(20) DEFAULT '0',
  `slug` varchar(10) NOT NULL,
  `name` varchar(45) DEFAULT NULL,
  `options` text,
  `status_id` tinyint(4) DEFAULT '1' COMMENT '0: active\n1: inactive\n',
  PRIMARY KEY (`sid`),
  KEY `slug_index` (`slug`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `stations` WRITE;
/*!40000 ALTER TABLE `stations` DISABLE KEYS */;

INSERT INTO `stations` (`sid`, `user_id`, `slug`, `name`, `options`, `status_id`)
VALUES
	(1,1,'tgr123','Cengkareng.',NULL,0),
	(2,2,'abcDef12Q','Semarang',NULL,0),
	(3,2,'Wsrt351X','Joglo',NULL,0),
	(4,1,'9DePx53q','Cilacap',NULL,0),
	(5,5,'m4RIoXL','Kebun Digital Mario',NULL,0);

/*!40000 ALTER TABLE `stations` ENABLE KEYS */;
UNLOCK TABLES;


# Dump of table users
# ------------------------------------------------------------

DROP TABLE IF EXISTS `users`;

CREATE TABLE `users` (
  `uid` bigint(20) NOT NULL AUTO_INCREMENT,
  `username` varchar(45) DEFAULT NULL,
  `email` varchar(45) DEFAULT NULL,
  `name` varchar(45) DEFAULT NULL,
  `phone` varchar(21) DEFAULT NULL,
  `token` varchar(70) DEFAULT NULL,
  `token_expired` datetime DEFAULT NULL,
  `status_id` tinyint(4) DEFAULT '1',
  PRIMARY KEY (`uid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

LOCK TABLES `users` WRITE;
/*!40000 ALTER TABLE `users` DISABLE KEYS */;

INSERT INTO `users` (`uid`, `username`, `email`, `name`, `phone`, `token`, `token_expired`, `status_id`)
VALUES
	(1,'tigor','einsthonk@gmail.com','Tigor Mangatur Manurung',NULL,'E9F3209649E9B62E89D25C0EAAF3D471B59CD2BEDB516D3AA3FAA2CEF3FA9EE5',NULL,0),
	(2,'luri','luri@kioss.com','Luri Darmawan',NULL,'1291E637149D656D2F4D211CA48A88B9FFEEA64EAE04EC3FB7A434233087E9E2',NULL,0),
	(3,'iwancs','iwan@iwancs.com','Iwan CS',NULL,'AB0A0E08ACFAA8CE35D8971CC25EB6E2A6EA5600083F0CE07D10791E1268A358',NULL,0),
	(4,'maskofa','kofa@iyem.id','Maskofa',NULL,'1B05AD28E74B330386F6F150D85C2BB904B376E02522653370411C5F317F063C',NULL,0),
	(5,'mario','leledumbo_cool@yahoo.co.id','Mario',NULL,'CD17B2A1BB79A1CB1AE57768AE8FA849395558E9FF38B5B0C0485CFA579F093E',NULL,0);

/*!40000 ALTER TABLE `users` ENABLE KEYS */;
UNLOCK TABLES;



/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
