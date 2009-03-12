--------------------------------------------------------------------------------
-- DROP TABLE users;
-- DROP TABLE projects_users;
-- DROP VIEW  permissions;

--------------------------------------------------------------------------------
CREATE TABLE users
(
    id   SERIAL      PRIMARY KEY
  , name VARCHAR(30) NOT NULL UNIQUE
);

CREATE TABLE projects_users
(
    project_id INTEGER NOT NULL REFERENCES projects(id)
  , user_id    INTEGER NOT NULL REFERENCES users(id)

);

CREATE VIEW permissions AS
SELECT u.name      AS user_name
     , p.name      AS project_name
FROM   projects_users m INNER JOIN projects p on m.project_id = p.id
                        INNER JOIN users    u on m.user_id    = u.id
                        ;
