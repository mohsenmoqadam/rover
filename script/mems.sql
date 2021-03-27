-- %% =================================================================================
-- %% ========= SP: rover_db.sp_add_user | SPC: 10001
-- %% =================================================================================
DELIMITER %
DROP PROCEDURE IF EXISTS `rover_db`.`sp_add_user`%
CREATE PROCEDURE `rover_db`.`sp_add_user`(
	IN pno VARCHAR(16)
)
BEGIN 
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
	 	ROLLBACK;
	 	SELECT 10001 AS 'SPC', 1 AS 'RC';
	END;

	SET @create_at = NOW();
	SET @is_old_user = FALSE;
	SET @return_code = 0;
	
	SELECT uid FROM rover_db.users WHERE users.pno = pno AND users.is_deleted = false INTO @is_old_user;  
	
	IF (@is_old_user = FALSE) THEN
		-- New User
		START TRANSACTION;
		SELECT rover_db.id('CUSTOMER') INTO @uid;
		INSERT INTO rover_db.users (`uid`, `created_at`, `pno`)
		VALUES (@uid, @create_at, pno);
		COMMIT;
		SET @return_code = 2; -- New User
	ELSE
		-- Old User
		SET @uid = @is_old_user;
		SET @return_code = 3; -- Old User
	END IF;
	SELECT 10001 AS 'SPC', @return_code AS 'RC', @uid AS 'UID';	
END
%
DELIMITER ;

CALL `rover_db`.`sp_add_user`("989134044938");

-- %% =================================================================================
-- %% ========= SP: rover_db.xxx | SPC: 100xx
-- %% =================================================================================
