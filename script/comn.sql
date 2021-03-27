-- %% =================================================================================
-- %% ========= SP: rover_db.id | SPC: ---
-- %% =================================================================================
DELIMITER %
DROP FUNCTION IF EXISTS `rover_db`.`id`%
CREATE FUNCTION `rover_db`.`id`(
        type ENUM ('CUSTOMER', 'APPLICATION')
)
RETURNS BIGINT
BEGIN
        INSERT INTO rover_db.ids (`type`, `create_at`)
                VALUES (type, NOW());
                SELECT LAST_INSERT_ID() INTO @id;
        RETURN @id;
END
%
DELIMITER ;

-- EXAMPLES:
SELECT rover_db.id('CUSTOMER');
SELECT rover_db.id('APPLICATION');

-- %% =================================================================================
-- %% ========= SP: rover_db.xxx | SPC: 100xx
-- %% =================================================================================
