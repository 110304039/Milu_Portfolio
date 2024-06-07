CREATE TABLE Person(
	PersonID varchar(50) NOT NULL,
    Name varchar(50) NOT NULL UNIQUE,
    Password varchar(12) NOT NULL,
    Location varchar(50),
    PRIMARY KEY (PersonID)
);

CREATE TABLE Station(
	StationID varchar(50) NOT NULL,
    Name varchar(50) NOT NULL,
    PRIMARY KEY (StationID)
);

CREATE TABLE Store(
	StoreID varchar(50) NOT NULL,
    Name varchar(50) NOT NULL,
    Location varchar(50) NOT NULL,
    Category varchar(50) NOT NULL,
    URL varchar(50) NOT NULL,
    Distance varchar(50) NOT NULL,
    StationID varchar(50) NOT NULL,
    PRIMARY KEY (StoreID),
    FOREIGN KEY (StationID) REFERENCES Station(StationID)
);

CREATE TABLE Event(
	EventID int NOT NULL AUTO_INCREMENT,
    P1_ID varchar(50) NOT NULL,
    P2_ID varchar(50) NOT NULL,
    Time DATETIME NOT NULL,
    FoodType varchar(50) NOT NULL,
    StationID varchar(50) NOT NULL,
    PRIMARY KEY (EventID),
    FOREIGN KEY (P1_ID) REFERENCES Person(PersonID),
    FOREIGN KEY (P2_ID) REFERENCES Person(PersonID),
    FOREIGN KEY (StationID) REFERENCES Station(StationID)
);

CREATE TABLE MealPal(
	MealPalID int NOT NULL AUTO_INCREMENT,
    P1_ID varchar(50) NOT NULL,
    P2_ID varchar(50) NOT NULL,
    PRIMARY KEY (MealPalID),
    FOREIGN KEY (P1_ID) REFERENCES Person(PersonID),
    FOREIGN KEY (P2_ID) REFERENCES Person(PersonID)
);

CREATE TABLE ChatRecord(
	ChatID int NOT NULL AUTO_INCREMENT,
    MealPalID int NOT NULL,
    P_ID varchar(50) NOT NULL,
    Time DATETIME NOT NULL,
    Content varchar(50) NOT NULL,
    PRIMARY KEY (ChatID),
    FOREIGN KEY (MealPalID) REFERENCES MealPal(MealPalID),
    FOREIGN KEY (P_ID) REFERENCES Person(PersonID)
);

CREATE TABLE HistoryList(
	HListID int NOT NULL AUTO_INCREMENT,
    PersonID varchar(50) NOT NULL,
    StoreID varchar(50) NOT NULL,
    PRIMARY KEY (HListID),
    FOREIGN KEY (PersonID) REFERENCES Person(PersonID),
    FOREIGN KEY (StoreID) REFERENCES Store(StoreID)
);

CREATE TABLE FavoriteList(
	FListID int NOT NULL AUTO_INCREMENT,
    PersonID varchar(50) NOT NULL,
    StoreID varchar(50) NOT NULL,
    PRIMARY KEY (FListID),
    FOREIGN KEY (PersonID) REFERENCES Person(PersonID),
    FOREIGN KEY (StoreID) REFERENCES Store(StoreID)
);

CREATE TABLE Comment(
	CommentID int NOT NULL AUTO_INCREMENT,
    PersonID varchar(50) NOT NULL,
    StoreID varchar(50) NOT NULL,
    Content varchar(150) NOT NULL,
    PRIMARY KEY (CommentID),
    FOREIGN KEY (PersonID) REFERENCES Person(PersonID),
    FOREIGN KEY (StoreID) REFERENCES Store(StoreID)
);