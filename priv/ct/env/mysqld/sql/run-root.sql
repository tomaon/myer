CREATE DATABASE IF NOT EXISTS test;


CREATE USER 'test'@'127.0.0.1';
SET old_passwords = 0;
SET PASSWORD FOR 'test'@'127.0.0.1' = PASSWORD('test');
GRANT ALL ON test.* TO  'test'@'127.0.0.1';
GRANT SELECT ON mysql.* TO  'test'@'127.0.0.1'; 
GRANT RELOAD ON *.* TO 'test'@'127.0.0.1';

CREATE USER 'test_nopwd'@'127.0.0.1';
GRANT SELECT ON test.* TO 'test_nopwd'@'127.0.0.1';

CREATE USER 'test_oldpwd'@'127.0.0.1' /*!50500 IDENTIFIED WITH 'mysql_old_password' */;
SET old_passwords = 1;
SET PASSWORD FOR 'test_oldpwd'@'127.0.0.1' = PASSWORD('test');
GRANT SELECT ON test.* TO 'test_oldpwd'@'127.0.0.1';

CREATE USER 'test_oldnopwd'@'127.0.0.1' /*!50500 IDENTIFIED WITH 'mysql_old_password' */;
GRANT SELECT ON test.* TO 'test_oldnopwd'@'127.0.0.1';

/*!50606 CREATE USER 'test_sha256pwd'@'127.0.0.1' IDENTIFIED WITH 'sha256_password' */;
/*!50606 SET old_passwords = 2 */;
/*!50606 SET PASSWORD FOR 'test_sha256pwd'@'127.0.0.1' = PASSWORD('test') */;
/*!50606 GRANT SELECT ON test.* TO 'test_sha256pwd'@'127.0.0.1' */;

/*!50606 CREATE USER 'test_sha256nopwd'@'127.0.0.1' IDENTIFIED WITH 'sha256_password' */;
/*!50606 GRANT SELECT ON test.* TO 'test_sha256nopwd'@'127.0.0.1' */;

CREATE USER 'test_dev'@'127.0.0.1' /*!50500 IDENTIFIED WITH 'test_plugin_server' */;
GRANT SELECT ON test.* TO 'test_dev'@'127.0.0.1';


FLUSH PRIVILEGES;


SELECT
  host
, user
/*!50500 , plugin */
FROM mysql.user
;
