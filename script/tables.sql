CREATE TABLE rover_db.ids ( `id` BIGINT NOT NULL AUTO_INCREMENT
       	     		  , `type` ENUM ('CUSTOMER', 'APPLICATION') NOT NULL
			  , create_at DATETIME
			  , PRIMARY KEY (id)
			  ) AUTO_INCREMENT=123456789 DEFAULT CHARSET=utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE rover_db.users ( `uid` BIGINT
       	     		    , `created_at` DATETIME
			    , `pno` VARCHAR(16) NOT NULL
			    , `nick_name` VARCHAR(128)
			    , `global_name` VARCHAR(128)
			    , `avatar` TEXT
			    , `bio` TEXT
			    , `is_deleted` BOOLEAN DEFAULT FALSE
			    , `sequence` BIGINT DEFAULT 0
			    , `kivi` MEDIUMTEXT DEFAULT ""
			    , FOREIGN KEY (uid) REFERENCES rover_db.ids(id)
			    , FOREIGN KEY (uid) REFERENCES rover_db.ids(id)
			    ) DEFAULT CHARSET=utf8mb4 COLLATE utf8mb4_unicode_ci;
