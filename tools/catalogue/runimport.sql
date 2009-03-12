CREATE TABLE IF NOT EXISTS lastdates
(
    project_name VARCHAR(18) NOT NULL UNIQUE
  , lastdate     datetime    NOT NULL
);

DELETE FROM lastdates;

INSERT INTO lastdates (
    SELECT p.name, max(o.datetime)
    FROM projects p, observations o
    WHERE o.project_id IN (
        SELECT q.id FROM projects q WHERE q.name LIKE CONCAT(SUBSTR(p.name, 1, LENGTH(p.name)-3), '\___'))
    GROUP BY p.name);

UPDATE projects
SET lastdate = (
    SELECT lastdate FROM lastdates WHERE project_name = name);

DELETE FROM queries;

INSERT INTO queries (SELECT * from observation_details);
