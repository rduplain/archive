--------------------------------------------------------------------------------
DROP TABLE IF EXISTS queries;
DROP TABLE IF EXISTS windows;
DROP TABLE IF EXISTS observations;
DROP TABLE IF EXISTS projects;
DROP TABLE IF EXISTS detectors;
DROP TABLE IF EXISTS observers;
DROP TABLE IF EXISTS polarizations;
DROP TABLE IF EXISTS polarization_types;
DROP TABLE IF EXISTS receivers;
DROP TABLE IF EXISTS sources;
DROP TABLE IF EXISTS telescopes;

--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS detectors
(
    id   INTEGER     NOT NULL AUTO_INCREMENT PRIMARY KEY
  , name VARCHAR(30) NOT NULL UNIQUE

  , INDEX(name)
) ENGINE=InnoDB;

--------------------------------------------------------------------------------
INSERT INTO detectors(id, name) VALUES
    (1, 'CCB')
  , (2, 'DCR')
  , (3, 'SpectralProcessor')
  , (4, 'Spectrometer')
  , (5, 'Spigot')
  ;

+----+-------------------+
| id | name              |
+----+-------------------+
|  6 | BCPM1             |
| 10 | BCPM2             |
|  1 | CCB               |
|  8 | CCB26_40          |
|  7 | CGSR2             |
|  2 | DCR               |
| 11 | LLRADAR           |
|  9 | RadarPfs          |
|  3 | SpectralProcessor |
|  4 | Spectrometer      |
|  5 | Spigot            |
| 12 | VLBA_DAR          |
| 13 | Zpectrometer      |
+----+-------------------+


--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS observers
(
    id   INTEGER     NOT NULL AUTO_INCREMENT PRIMARY KEY
  , name VARCHAR(30) NOT NULL UNIQUE

  , INDEX(name)
) ENGINE=InnoDB;

--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS polarization_types
(
    id   INTEGER    NOT NULL AUTO_INCREMENT PRIMARY KEY
  , name VARCHAR(8) NOT NULL

  , INDEX(name)
) ENGINE=InnoDB;

--------------------------------------------------------------------------------
INSERT INTO polarization_types(id, name) VALUES
    (1, 'Circular')
  , (2, 'Linear')
  ;

--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS polarizations
(
    id                   INTEGER    NOT NULL AUTO_INCREMENT PRIMARY KEY
  , polarization_type_id INTEGER    NOT NULL REFERENCES polarization_types(id)
  , name                 VARCHAR(2) NOT NULL UNIQUE

  , INDEX(name)
) ENGINE=InnoDB;

--------------------------------------------------------------------------------
INSERT INTO polarizations(id, polarization_type_id, name) VALUES
  , (1, 1, 'LL')
  , (2, 1, 'LR')
  , (3, 1, 'RL')
  , (4, 1, 'RR')
  , (5, 2, 'XX')
  , (6, 2, 'XY')
  , (7, 2, 'YX')
  , (8, 2, 'YY')
  ;

+----+----------------------+------+
| id | polarization_type_id | name |
+----+----------------------+------+
|  3 |                    1 | LL   | 
|  4 |                    1 | LR   | 
|  5 |                    1 | RL   | 
|  6 |                    1 | RR   | 
|  9 |                    2 | XX   | 
| 10 |                    2 | XY   | 
| 11 |                    2 | YX   | 
| 12 |                    2 | YY   | 
+----+----------------------+------+


--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS receivers
(
    id       INTEGER     NOT NULL AUTO_INCREMENT PRIMARY KEY
  , name     VARCHAR(12) NOT NULL UNIQUE
  , min_freq REAL        NOT NULL
  , max_freq REAL        NOT NULL
  , band     VARCHAR(2)  NULL

  , CHECK(min_freq < max_freq)
  , INDEX(name)
) ENGINE=InnoDB;

--------------------------------------------------------------------------------
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES
    ( 1, 'PF1.1',       290000000,   395000000, NULL)
  , ( 2, 'PF1.2',       385000000,   520000000, NULL)
  , ( 3, 'PF1.3',       510000000,   690000000, NULL)
  , ( 4, 'PF1.4',       680000000,   920000000, NULL)
  , ( 5, 'PF2',         910000000,  1230000000, NULL)
  , ( 6, 'Rcvr1_2',    1150000000,  1730000000, 'L')
  , ( 7, 'Rcvr2_3',    1730000000,  2600000000, 'S')
  , ( 8, 'Rcvr4_6',    3950000000,  5850000000, 'C')
  , ( 9, 'Rcvr8_10',   8000000000, 10000000000, 'X')
  , (10, 'Rcvr12_18', 12000000000, 15400000000, 'Ku')
  , (11, 'Rcvr18_26', 18000000000, 26500000000, 'K')
  , (12, 'Rcvr26_40', 26000000000, 40000000000, 'Ka')
  , (13, 'Rcvr40_52', 41000000000, 48000000000, 'Q')
  , (14, 'Rcvr_342',    290000000,   395000000, NULL)
  , (15, 'Rcvr_450',    385000000,   520000000, NULL)
  , (16, 'Rcvr_600',    510000000,   690000000, NULL)
  , (17, 'Rcvr_800',    680000000,   920000000, NULL)
  , (18, 'Rcvr_1070',   910000000,  1230000000, NULL)
  , (19, 'Rcvr_PAR',  86000000000, 94000000000, NULL)
  , (20, 'RcvrPF_1',    290000000,   920000000, NULL)
  ;

+----+-----------+-------------+-------------+------+
| id | name      | min_freq    | max_freq    | band |
+----+-----------+-------------+-------------+------+
|  1 | PF1.1     |   290000000 |   395000000 | NULL | 
|  2 | PF1.2     |   385000000 |   520000000 | NULL | 
|  3 | PF1.3     |   510000000 |   690000000 | NULL | 
|  4 | PF1.4     |   680000000 |   920000000 | NULL | 
|  5 | PF2       |   910000000 |  1230000000 | NULL | 
|  6 | Rcvr1_2   |  1150000000 |  1730000000 | L    | 
|  7 | Rcvr2_3   |  1730000000 |  2600000000 | S    | 
|  8 | Rcvr4_6   |  3950000000 |  5850000000 | C    | 
|  9 | Rcvr8_10  |  8000000000 | 10000000000 | X    | 
| 10 | Rcvr12_18 | 12000000000 | 15400000000 | Ku   | 
| 11 | Rcvr18_26 | 18000000000 | 26500000000 | K    | 
| 12 | Rcvr26_40 | 26000000000 | 40000000000 | Ka   | 
| 13 | Rcvr40_52 | 41000000000 | 48000000000 | Q    | 
| 14 | Rcvr_342  |   290000000 |   395000000 | NULL | 
| 15 | Rcvr_450  |   385000000 |   520000000 | NULL | 
| 16 | Rcvr_600  |   510000000 |   690000000 | NULL | 
| 17 | Rcvr_800  |   680000000 |   920000000 | NULL | 
| 18 | Rcvr_1070 |   910000000 |  1230000000 | NULL | 
| 19 | Rcvr_PAR  | 86000000000 | 94000000000 | NULL | 
| 20 | RcvrPF_1  |   290000000 |   920000000 | NULL | 
+----+-----------+-------------+-------------+------+


--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS sources
(
    id   INTEGER     NOT NULL AUTO_INCREMENT PRIMARY KEY
  , name VARCHAR(30) NOT NULL UNIQUE

  , INDEX(name)
) ENGINE=InnoDB;

--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS telescopes
(
    id   INTEGER    NOT NULL AUTO_INCREMENT PRIMARY KEY
  , name VARCHAR(6) NOT NULL UNIQUE

  , INDEX(name)
) ENGINE=InnoDB;

--------------------------------------------------------------------------------
INSERT INTO telescopes(id, name) VALUES
    (1, 'ALMA')
  , (2, 'GBT')
  , (3, 'VLA')
  , (4, 'VLBA')
  ;

--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS projects
(
    id           INTEGER     NOT NULL AUTO_INCREMENT PRIMARY KEY
  , observer_id  INTEGER     NOT NULL REFERENCES observers(id)
  , telescope_id INTEGER     NOT NULL REFERENCES telescopes(id)
  , name         VARCHAR(18) NOT NULL UNIQUE
  , lastdate     DATETIME        NULL

  , INDEX(name)
) ENGINE=InnoDB;

--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS observations
(
    id          INTEGER  NOT NULL AUTO_INCREMENT PRIMARY KEY
  , project_id  INTEGER  NOT NULL REFERENCES projects(id)
  , source_id   INTEGER  NOT NULL REFERENCES sources(id)
  , scan        INTEGER  NOT NULL
  , datetime    DATETIME NOT NULL
  , exposure    INTEGER  NOT NULL
  , ra2000      REAL     NOT NULL
  , dec2000     REAL     NOT NULL
  , filesize    INTEGER  NOT NULL

  , UNIQUE(project_id, scan)
  , CHECK('2001-01-01' <= datetime AND datetime < NOW())
  , CHECK(0 <= ra2000 AND ra2000 < 360)
  , CHECK(-180 <= dec2000 AND dec2000 <= 180)
  , CHECK(5 <= elevation AND elevation <= 95)
) ENGINE=InnoDB;

--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS windows
(
    id              INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY
  , detector_id     INTEGER NOT NULL REFERENCES detectors(id)
  , observation_id  INTEGER NOT NULL REFERENCES observations(id)
  , polarization_id INTEGER NOT NULL REFERENCES polarizations(id)
  , receiver_id     INTEGER NOT NULL REFERENCES receivers(id)
  , frequency       REAL    NOT NULL
  , bandwidth       REAL    NOT NULL

  , CHECK(20000000 <= frequency AND frequency < 150000000000)
) ENGINE=InnoDB;

--------------------------------------------------------------------------------
CREATE VIEW observation_details AS
SELECT q.id        AS observation_id
     , q.exposure  AS exposure
     , q.filesize  AS filesize
     , p.id        AS project_id
     , p.name      AS project_name
     , p.lastdate  AS lastdate
     , s.id        AS source_id
     , s.name      AS source_name
     , o.id        AS observer_id
     , o.name      AS observer_name
     , t.id        AS telescope_id
     , t.name      AS telescope_name
     , q.scan      AS scan
     , q.ra2000    AS ra2000
     , q.dec2000   AS dec2000
     , q.datetime  AS datetime
     , q.datetime  AS starttime
     , w.frequency AS frequency
     , w.bandwidth AS bandwidth
     , d.id        AS detector_id
     , d.name      AS detector_name
     , r.id        AS receiver_id
     , r.name      AS receiver_name
     , r.band      AS band
     , u.id        AS polarization_id
     , u.name      AS polarization_name
     , EXTRACT(YEAR FROM datetime)  AS year
     , EXTRACT(MONTH FROM datetime) AS month
FROM observations q INNER JOIN projects      p ON q.project_id      = p.id
                    INNER JOIN sources       s ON q.source_id       = s.id
                    INNER JOIN observers     o ON p.observer_id     = o.id
                    INNER JOIN telescopes    t ON p.telescope_id    = t.id
                    INNER JOIN windows       w ON w.observation_id  = q.id
                    INNER JOIN detectors     d ON w.detector_id     = d.id
                    INNER JOIN receivers     r ON w.receiver_id     = r.id
                    INNER JOIN polarizations u ON w.polarization_id = u.id
                    ;

--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS queries
(
    observation_id    INTEGER     NOT NULL REFERENCES observations(id)
  , exposure          INTEGER     NOT NULL
  , filesize          INTEGER     NOT NULL
  , project_id        INTEGER     NOT NULL REFERENCES projects(id)
  , project_name      VARCHAR(18) NOT NULL
  , lastdate          DATETIME    NOT NULL
  , source_id         INTEGER     NOT NULL REFERENCES sources(id)
  , source_name       VARCHAR(30) NOT NULL
  , observer_id       INTEGER     NOT NULL REFERENCES observers(id)
  , observer_name     VARCHAR(30) NOT NULL
  , telescope_id      INTEGER     NOT NULL REFERENCES telescopes(id)
  , telescope_name    VARCHAR(6)  NOT NULL
  , scan              INTEGER     NOT NULL
  , ra2000            REAL        NOT NULL
  , dec2000           REAL        NOT NULL
  , datetime          DATETIME    NOT NULL
  , starttime         DATETIME    NOT NULL
  , frequency         REAL        NOT NULL
  , bandwidth         REAL        NOT NULL
  , detector_id       INTEGER     NOT NULL REFERENCES detectors(id)
  , detector_name     VARCHAR(30) NOT NULL
  , receiver_id       INTEGER     NOT NULL REFERENCES receivers(id)
  , receiver_name     VARCHAR(12) NOT NULL
  , band              VARCHAR(2)  NULL
  , polarization_id   INTEGER     NOT NULL REFERENCES polarizations(id)
  , polarization_name VARCHAR(2)  NOT NULL
  , year              INTEGER     NOT NULL
  , month             INTEGER     NOT NULL

  , INDEX(project_id)
  , INDEX(project_name)
  , INDEX(source_id)
  , INDEX(source_name)
  , INDEX(observer_id)
  , INDEX(observer_name)
  , INDEX(ra2000)
  , INDEX(dec2000)
  , INDEX(frequency)
  , INDEX(bandwidth)
  , INDEX(detector_id)
  , INDEX(detector_name)
  , INDEX(receiver_id)
  , INDEX(receiver_name)
  , INDEX(band)
  , INDEX(polarization_id)
  , INDEX(polarization_name)
  , INDEX(year)
  , INDEX(month)
) ENGINE=MyISAM;

mysql> describe lastdates;
+--------------+-------------+------+-----+---------+-------+
| Field        | Type        | Null | Key | Default | Extra |
+--------------+-------------+------+-----+---------+-------+
| project_name | varchar(18) | NO   | PRI |         |       |
| lastdate     | datetime    | NO   |     |         |       |
+--------------+-------------+------+-----+---------+-------+

mysql> describe vlaobservers;
+-------+-------------+------+-----+---------+----------------+
| Field | Type        | Null | Key | Default | Extra          |
+-------+-------------+------+-----+---------+----------------+
| id    | int(11)     | NO   | PRI | NULL    | auto_increment | 
| name  | varchar(30) | NO   | UNI |         |                | 
+-------+-------------+------+-----+---------+----------------+

mysql> describe vlaprojects;
+-------+-------------+------+-----+---------+----------------+
| Field | Type        | Null | Key | Default | Extra          |
+-------+-------------+------+-----+---------+----------------+
| id    | int(11)     | NO   | PRI | NULL    | auto_increment | 
| name  | varchar(18) | NO   | UNI |         |                | 
+-------+-------------+------+-----+---------+----------------+

mysql> describe vlasources;
+-------+-------------+------+-----+---------+----------------+
| Field | Type        | Null | Key | Default | Extra          |
+-------+-------------+------+-----+---------+----------------+
| id    | int(11)     | NO   | PRI | NULL    | auto_increment | 
| name  | varchar(30) | NO   | UNI |         |                | 
+-------+-------------+------+-----+---------+----------------+

