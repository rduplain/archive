--------------------------------------------------------------------------------
-- DROP TABLE queries;
-- DROP TABLE windows;
-- DROP TABLE observations;
-- DROP TABLE projects;
-- DROP TABLE detectors;
-- DROP TABLE observers;
-- DROP TABLE polarizations;
-- DROP TABLE polarization_types;
-- DROP TABLE receivers;
-- DROP TABLE sources;
-- DROP TABLE telescopes;

--------------------------------------------------------------------------------
CREATE TABLE detectors
(
    id   SERIAL      PRIMARY KEY
  , name VARCHAR(30) NOT NULL UNIQUE

  -- , INDEX(name)
);

--------------------------------------------------------------------------------
INSERT INTO detectors(id, name) VALUES  ( 1, 'CCB');
INSERT INTO detectors(id, name) VALUES  ( 2, 'DCR');
INSERT INTO detectors(id, name) VALUES  ( 3, 'SpectralProcessor');
INSERT INTO detectors(id, name) VALUES  ( 4, 'Spectrometer');
INSERT INTO detectors(id, name) VALUES  ( 5, 'Spigot');
INSERT INTO detectors(id, name) VALUES  ( 6, 'BCPM1');
INSERT INTO detectors(id, name) VALUES  ( 7, 'CGSR2');
INSERT INTO detectors(id, name) VALUES  ( 8, 'CCB26_40');
INSERT INTO detectors(id, name) VALUES  ( 9, 'RadarPfs');
INSERT INTO detectors(id, name) VALUES  (10, 'BCPM2');
INSERT INTO detectors(id, name) VALUES  (11, 'LLRADAR');
INSERT INTO detectors(id, name) VALUES  (12, 'VLBA_DAR');
INSERT INTO detectors(id, name) VALUES  (12, 'Zpectrometer');

--------------------------------------------------------------------------------
CREATE TABLE observers
(
    id   SERIAL      PRIMARY KEY
  , name VARCHAR(30) NOT NULL UNIQUE

  -- , INDEX(name)
);

--------------------------------------------------------------------------------
CREATE TABLE vlaobservers
(
    id   SERIAL      PRIMARY KEY
  , name VARCHAR(30) NOT NULL UNIQUE
);

--------------------------------------------------------------------------------
CREATE TABLE polarization_types
(
    id   SERIAL     PRIMARY KEY
  , name VARCHAR(8) NOT NULL

  -- , INDEX(name)
);

--------------------------------------------------------------------------------
INSERT INTO polarization_types(id, name) VALUES  (1, 'Circular');
INSERT INTO polarization_types(id, name) VALUES  (2, 'Linear');

--------------------------------------------------------------------------------
CREATE TABLE polarizations
(
    id                   SERIAL     PRIMARY KEY
  , polarization_type_id INTEGER    NOT NULL REFERENCES polarization_types(id)
  , name                 VARCHAR(2) NOT NULL UNIQUE

  -- , INDEX(name)
);

--------------------------------------------------------------------------------
INSERT INTO polarizations(id, polarization_type_id, name) VALUES  ( 3, 1, 'LL');
INSERT INTO polarizations(id, polarization_type_id, name) VALUES  ( 4, 1, 'LR');
INSERT INTO polarizations(id, polarization_type_id, name) VALUES  ( 5, 1, 'RL');
INSERT INTO polarizations(id, polarization_type_id, name) VALUES  ( 6, 1, 'RR');
INSERT INTO polarizations(id, polarization_type_id, name) VALUES  ( 9, 2, 'XX');
INSERT INTO polarizations(id, polarization_type_id, name) VALUES  (10, 2, 'XY');
INSERT INTO polarizations(id, polarization_type_id, name) VALUES  (11, 2, 'YX');
INSERT INTO polarizations(id, polarization_type_id, name) VALUES  (12, 2, 'YY');

--------------------------------------------------------------------------------
CREATE TABLE receivers
(
    id       SERIAL      PRIMARY KEY
  , name     VARCHAR(12) NOT NULL UNIQUE
  , min_freq REAL        NOT NULL
  , max_freq REAL        NOT NULL
  , band     VARCHAR(2)  NULL

  -- , CHECK(min_freq < max_freq)
  -- , INDEX(name)
);

--------------------------------------------------------------------------------
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES ( 1, 'PF1.1',       290000000,   395000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES ( 2, 'PF1.2',       385000000,   520000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES ( 3, 'PF1.3',       510000000,   690000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES ( 4, 'PF1.4',       680000000,   920000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES ( 5, 'PF2',         910000000,  1230000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES ( 6, 'Rcvr1_2',    1150000000,  1730000000, 'L');
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES ( 7, 'Rcvr2_3',    1730000000,  2600000000, 'S');
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES ( 8, 'Rcvr4_6',    3950000000,  5850000000, 'C');
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES ( 9, 'Rcvr8_10',   8000000000, 10000000000, 'X');
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (10, 'Rcvr12_18', 12000000000, 15400000000, 'Ku');
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (11, 'Rcvr18_26', 18000000000, 26500000000, 'K');
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (12, 'Rcvr26_40', 26000000000, 40000000000, 'Ka');
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (13, 'Rcvr40_52', 41000000000, 48000000000, 'Q');
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (14, 'Rcvr_342',    290000000,   395000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (15, 'Rcvr_450',    385000000,   520000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (16, 'Rcvr_600',    510000000,   690000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (17, 'Rcvr_800',    680000000,   920000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (18, 'Rcvr_1070',   910000000,  1230000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (19, 'Rcvr_PAR',  86000000000, 94000000000, NULL);
INSERT INTO receivers(id, name, min_freq, max_freq, band) VALUES (20, 'RcvrPF_1',    290000000,   920000000, NULL);

--------------------------------------------------------------------------------
CREATE TABLE sources
(
    id   SERIAL      PRIMARY KEY
  , name VARCHAR(30) NOT NULL UNIQUE

  -- , INDEX(name)
);

--------------------------------------------------------------------------------
CREATE TABLE vlasources
(
    id   SERIAL      PRIMARY KEY
  , name VARCHAR(30) NOT NULL UNIQUE
);

--------------------------------------------------------------------------------
CREATE TABLE telescopes
(
    id   SERIAL     PRIMARY KEY
  , name VARCHAR(6) NOT NULL UNIQUE

  -- , INDEX(name)
);

--------------------------------------------------------------------------------
INSERT INTO telescopes(id, name) VALUES (1, 'ALMA');
INSERT INTO telescopes(id, name) VALUES (2, 'GBT');
INSERT INTO telescopes(id, name) VALUES (3, 'VLA');
INSERT INTO telescopes(id, name) VALUES (4, 'VLBA');

--------------------------------------------------------------------------------
CREATE TABLE projects
(
    id           SERIAL      PRIMARY KEY
  , observer_id  INTEGER     NOT NULL REFERENCES observers(id)
  , telescope_id INTEGER     NOT NULL REFERENCES telescopes(id)
  , name         VARCHAR(18) NOT NULL UNIQUE
  , lastdate     TIMESTAMP   NULL

  -- , INDEX(name)
);

--------------------------------------------------------------------------------
CREATE TABLE vlaprojects
(
    id   SERIAL      PRIMARY KEY
  , name VARCHAR(30) NOT NULL UNIQUE
);

--------------------------------------------------------------------------------
CREATE TABLE lastdates
(
    project_name VARCHAR(18) PRIMARY KEY
  , lastdate     TIMESTAMP   NOT NULL
);

--------------------------------------------------------------------------------
CREATE TABLE observations
(
    id          SERIAL    PRIMARY KEY
  , project_id  INTEGER   NOT NULL REFERENCES projects(id)
  , source_id   INTEGER   NOT NULL REFERENCES sources(id)
  , scan        INTEGER   NOT NULL
  , datetime    TIMESTAMP NOT NULL
  , exposure    INTEGER   NOT NULL
  , ra2000      REAL      NOT NULL
  , dec2000     REAL      NOT NULL
  , filesize    INTEGER   NOT NULL

  -- , UNIQUE(project_id, scan)
  -- , CHECK('2001-01-01' <= datetime AND datetime < NOW())
  -- , CHECK(0 <= ra2000 AND ra2000 < 360)
  -- , CHECK(-180 <= dec2000 AND dec2000 <= 180)
  -- , CHECK(5 <= elevation AND elevation <= 95)
);

--------------------------------------------------------------------------------
CREATE TABLE windows
(
    id              SERIAL  PRIMARY KEY
  , detector_id     INTEGER NOT NULL REFERENCES detectors(id)
  , observation_id  INTEGER NOT NULL REFERENCES observations(id)
  , polarization_id INTEGER NOT NULL REFERENCES polarizations(id)
  , receiver_id     INTEGER NOT NULL REFERENCES receivers(id)
  , frequency       REAL    NOT NULL
  , bandwidth       REAL    NOT NULL

  -- , CHECK(20000000 <= frequency AND frequency < 150000000000)
);

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
CREATE TABLE queries
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

  -- , INDEX(project_id)
  -- , INDEX(project_name)
  -- , INDEX(source_id)
  -- , INDEX(source_name)
  -- , INDEX(observer_id)
  -- , INDEX(observer_name)
  -- , INDEX(ra2000)
  -- , INDEX(dec2000)
  -- , INDEX(frequency)
  -- , INDEX(bandwidth)
  -- , INDEX(detector_id)
  -- , INDEX(detector_name)
  -- , INDEX(receiver_id)
  -- , INDEX(receiver_name)
  -- , INDEX(band)
  -- , INDEX(polarization_id)
  -- , INDEX(polarization_name)
  -- , INDEX(year)
  -- , INDEX(month)
);
