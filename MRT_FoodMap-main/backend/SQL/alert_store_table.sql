ALTER TABLE store 
ADD StationID varchar(50) NOT NULL,
ADD FOREIGN KEY (StationID) REFERENCES Station(StationID);