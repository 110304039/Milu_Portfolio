CREATE DATABASE  IF NOT EXISTS `mrt_foodmap` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci */ /*!80016 DEFAULT ENCRYPTION='N' */;
USE `mrt_foodmap`;
-- MySQL dump 10.13  Distrib 8.0.30, for Win64 (x86_64)
--
-- Host: localhost    Database: mrt_foodmap
-- ------------------------------------------------------
-- Server version	8.0.30

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `chatrecord`
--

DROP TABLE IF EXISTS `chatrecord`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `chatrecord` (
  `ChatID` int NOT NULL AUTO_INCREMENT,
  `MealPalID` int NOT NULL,
  `P_ID` varchar(50) NOT NULL,
  `Time` datetime NOT NULL,
  `Content` varchar(50) NOT NULL,
  PRIMARY KEY (`ChatID`),
  KEY `MealPalID` (`MealPalID`),
  KEY `P_ID` (`P_ID`),
  CONSTRAINT `chatrecord_ibfk_1` FOREIGN KEY (`MealPalID`) REFERENCES `mealpal` (`MealPalID`),
  CONSTRAINT `chatrecord_ibfk_2` FOREIGN KEY (`P_ID`) REFERENCES `person` (`PersonID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `chatrecord`
--

LOCK TABLES `chatrecord` WRITE;
/*!40000 ALTER TABLE `chatrecord` DISABLE KEYS */;
/*!40000 ALTER TABLE `chatrecord` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `comment`
--

DROP TABLE IF EXISTS `comment`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `comment` (
  `CommentID` int NOT NULL AUTO_INCREMENT,
  `PersonID` varchar(50) NOT NULL,
  `StoreID` varchar(50) NOT NULL,
  `Content` varchar(50) NOT NULL,
  PRIMARY KEY (`CommentID`),
  KEY `PersonID` (`PersonID`),
  KEY `StoreID` (`StoreID`),
  CONSTRAINT `comment_ibfk_1` FOREIGN KEY (`PersonID`) REFERENCES `person` (`PersonID`),
  CONSTRAINT `comment_ibfk_2` FOREIGN KEY (`StoreID`) REFERENCES `store` (`StoreID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `comment`
--

LOCK TABLES `comment` WRITE;
/*!40000 ALTER TABLE `comment` DISABLE KEYS */;
/*!40000 ALTER TABLE `comment` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `event`
--

DROP TABLE IF EXISTS `event`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `event` (
  `EventID` int NOT NULL AUTO_INCREMENT,
  `P1_ID` varchar(50) NOT NULL,
  `P2_ID` varchar(50) NOT NULL,
  `Time` datetime NOT NULL,
  `FoodType` varchar(50) NOT NULL,
  `StationID` varchar(50) NOT NULL,
  PRIMARY KEY (`EventID`),
  KEY `P1_ID` (`P1_ID`),
  KEY `P2_ID` (`P2_ID`),
  KEY `StationID` (`StationID`),
  CONSTRAINT `event_ibfk_1` FOREIGN KEY (`P1_ID`) REFERENCES `person` (`PersonID`),
  CONSTRAINT `event_ibfk_2` FOREIGN KEY (`P2_ID`) REFERENCES `person` (`PersonID`),
  CONSTRAINT `event_ibfk_3` FOREIGN KEY (`StationID`) REFERENCES `station` (`StationID`)
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `event`
--

LOCK TABLES `event` WRITE;
/*!40000 ALTER TABLE `event` DISABLE KEYS */;
INSERT INTO `event` VALUES (2,'00001001','00001005','2023-05-22 16:00:00','romen','R10'),(3,'00001002','00001006','2023-05-22 16:00:00','romen','R10'),(4,'00001003','00001004','2023-05-22 19:01:00','romen','R10'),(5,'00001006','00000000','2023-05-22 19:01:00','romen','R10');
/*!40000 ALTER TABLE `event` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `favoritelist`
--

DROP TABLE IF EXISTS `favoritelist`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `favoritelist` (
  `FListID` int NOT NULL AUTO_INCREMENT,
  `PersonID` varchar(50) NOT NULL,
  `StoreID` varchar(50) NOT NULL,
  PRIMARY KEY (`FListID`),
  KEY `PersonID` (`PersonID`),
  KEY `StoreID` (`StoreID`),
  CONSTRAINT `favoritelist_ibfk_1` FOREIGN KEY (`PersonID`) REFERENCES `person` (`PersonID`),
  CONSTRAINT `favoritelist_ibfk_2` FOREIGN KEY (`StoreID`) REFERENCES `store` (`StoreID`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `favoritelist`
--

LOCK TABLES `favoritelist` WRITE;
/*!40000 ALTER TABLE `favoritelist` DISABLE KEYS */;
INSERT INTO `favoritelist` VALUES (1,'00001002','1');
/*!40000 ALTER TABLE `favoritelist` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `historylist`
--

DROP TABLE IF EXISTS `historylist`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `historylist` (
  `HListID` int NOT NULL AUTO_INCREMENT,
  `PersonID` varchar(50) NOT NULL,
  `StoreID` varchar(50) NOT NULL,
  PRIMARY KEY (`HListID`),
  KEY `PersonID` (`PersonID`),
  KEY `StoreID` (`StoreID`),
  CONSTRAINT `historylist_ibfk_1` FOREIGN KEY (`PersonID`) REFERENCES `person` (`PersonID`),
  CONSTRAINT `historylist_ibfk_2` FOREIGN KEY (`StoreID`) REFERENCES `store` (`StoreID`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `historylist`
--

LOCK TABLES `historylist` WRITE;
/*!40000 ALTER TABLE `historylist` DISABLE KEYS */;
INSERT INTO `historylist` VALUES (1,'00001002','1');
/*!40000 ALTER TABLE `historylist` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mealpal`
--

DROP TABLE IF EXISTS `mealpal`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `mealpal` (
  `MealPalID` int NOT NULL AUTO_INCREMENT,
  `P1_ID` varchar(50) NOT NULL,
  `P2_ID` varchar(50) NOT NULL,
  PRIMARY KEY (`MealPalID`),
  KEY `P1_ID` (`P1_ID`),
  KEY `P2_ID` (`P2_ID`),
  CONSTRAINT `mealpal_ibfk_1` FOREIGN KEY (`P1_ID`) REFERENCES `person` (`PersonID`),
  CONSTRAINT `mealpal_ibfk_2` FOREIGN KEY (`P2_ID`) REFERENCES `person` (`PersonID`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mealpal`
--

LOCK TABLES `mealpal` WRITE;
/*!40000 ALTER TABLE `mealpal` DISABLE KEYS */;
INSERT INTO `mealpal` VALUES (1,'00001001','00001005');
/*!40000 ALTER TABLE `mealpal` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `person`
--

DROP TABLE IF EXISTS `person`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `person` (
  `PersonID` varchar(50) NOT NULL,
  `Name` varchar(50) NOT NULL,
  `Password` varchar(12) NOT NULL,
  `Location` varchar(50) DEFAULT '動物園站',
  PRIMARY KEY (`PersonID`),
  UNIQUE KEY `name_unique_constraint` (`Name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `person`
--

LOCK TABLES `person` WRITE;
/*!40000 ALTER TABLE `person` DISABLE KEYS */;
INSERT INTO `person` VALUES ('00000000','Null','00000000','臺北市'),('00001001','Sunny','12345678','北投區'),('00001002','Sun','87654321','北投區'),('00001003','candy','000098','臺北市'),('00001004','candyy','000098','臺北市'),('00001005','cand','000098','臺北市'),('00001006','canddy','000098','臺北市');
/*!40000 ALTER TABLE `person` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `station`
--

DROP TABLE IF EXISTS `station`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `station` (
  `StationID` varchar(50) NOT NULL,
  `Name` varchar(50) NOT NULL,
  PRIMARY KEY (`StationID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `station`
--

LOCK TABLES `station` WRITE;
/*!40000 ALTER TABLE `station` DISABLE KEYS */;
INSERT INTO `station` VALUES ('BR09','Daan'),('BR10','Zhongxiao Fuxing'),('BR11','Nanjing Fuxing'),('G12','Ximen'),('O06','Dongmen'),('O07','Zhongxiao Xinsheng'),('O08','Songjiang Nanjing'),('R08','Chiang Kai-Shek Memorial Hall'),('R10','Taipei Main Station'),('R11','Zhongshan');
/*!40000 ALTER TABLE `station` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `store`
--

DROP TABLE IF EXISTS `store`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `store` (
  `StoreID` varchar(50) NOT NULL,
  `Name` varchar(50) NOT NULL,
  `Location` varchar(50) NOT NULL,
  `Category` varchar(50) NOT NULL,
  `URL` varchar(50) NOT NULL,
  `Distance` varchar(50) NOT NULL,
  `StationID` varchar(50) NOT NULL,
  PRIMARY KEY (`StoreID`),
  KEY `StationID` (`StationID`),
  CONSTRAINT `store_ibfk_1` FOREIGN KEY (`StationID`) REFERENCES `station` (`StationID`),
  CONSTRAINT `store_ibfk_2` FOREIGN KEY (`StationID`) REFERENCES `station` (`StationID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `store`
--

LOCK TABLES `store` WRITE;
/*!40000 ALTER TABLE `store` DISABLE KEYS */;
INSERT INTO `store` VALUES ('1','麵屋武藏 本店','台北市中正區忠孝西路一段36號B1','ramen','https://reurl.cc/GAmgmd','450m','R10'),('10','太陽蕃茄拉麵 站前本店','台北市中正區忠孝西路一段38號凱撤飯店B1F','ramen','https://reurl.cc/M8AWmW','350m','R10'),('11','奧特拉麵Ramen Ultra 微風台北車站店','台北市中正區北平西路3號2樓','ramen','https://reurl.cc/jD8VKm','450m','R10'),('12','藍象廷泰式火鍋 台北站前店','台北市中正區忠孝西路一段66號 b1','hotpot','https://reurl.cc/lDRnxQ','350m','R10'),('13','黑毛屋 微風台北車站店','台北市中正區北平西路3號2樓','hotpot','https://reurl.cc/qLgaQq','350m','R10'),('14','熊一頂級燒肉-西門店','台北市萬華區西寧南路155號2樓','BBQ','https://reurl.cc/7krY6D','350m','G12'),('2','一風堂微風台北車站店','台北市中正區北平西路3號2樓','ramen','https://reurl.cc/x7GeOz','350m','R10'),('3','屯京拉麵 台北站前店','台北市中正區忠孝西路一段38號B1','ramen','https://reurl.cc/3xayo9','400m','R10'),('4','石研室 爆炒石頭火鍋 - 和億北車店','台北市中正區忠孝西路一段36號和億北車站B1F','hotpot','https://reurl.cc/b9XeDX','31m','R10'),('5','八海食潮當代鍋物-北車店','台北市中正區北平西路3號2樓','hotpot','https://reurl.cc/Ov0QMD','30m','R10'),('6','瓦法奇朵（W.麻辣BAR）','台北市中正區信陽街29號','hotpot','https://reurl.cc/eDEQyQ','300m','R10'),('7','我!就厲害燒烤西門珍饌店','台北市萬華區漢中街127號5F','BBQ','https://reurl.cc/N0rOlk','200m','G12'),('8','燒肉眾精緻炭火燒肉 台北西門店','台北市萬華區成都路66號2.3樓','BBQ','https://reurl.cc/gDWNeN','350m','G12'),('9','瓦崎燒烤火鍋 西門店','台北市萬華區成都路17號2 樓','BBQ','https://reurl.cc/kXZ9nL','190m','G12');
/*!40000 ALTER TABLE `store` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2023-06-02 20:57:01
