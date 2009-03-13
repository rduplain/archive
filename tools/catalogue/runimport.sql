-- CREATE LANGUAGE plpgsql;

CREATE FUNCTION fix_lastdates() RETURNS void AS $$
DECLARE
	project varchar;
BEGIN
	FOR project IN SELECT DISTINCT(SUBSTRING(name FROM 1 FOR LENGTH(name)-3)) FROM projects LOOP
		UPDATE projects
		SET lastdate = (SELECT MAX(o.datetime)
  			        FROM observations o, projects p
			        WHERE o.project_id = p.id AND p.name LIKE project || E'\___')
		WHERE name LIKE project || E'\___';
	END LOOP;
END
$$ LANGUAGE plpgsql;

SELECT fix_lastdates();
