CREATE DATABASE IF NOT EXISTS test
;

DROP USER
  'test'@'localhost'
, 'test'@'172.16.236.1'
, 'test_nopwd'@'172.16.236.1'
, 'test_oldpwd'@'172.16.236.1'
, 'test_oldnopwd'@'172.16.236.1'
/*!50606
, 'test_sha256pwd'@'172.16.236.1'
, 'test_sha256nopwd'@'172.16.236.1'
*/
;

/*!50002 CREATE USER 'test'@'localhost' */;
GRANT ALL ON test.* TO 'test'@'localhost';
SET PASSWORD FOR 'test'@'localhost' = PASSWORD('test');

/*!50002 CREATE USER 'test'@'172.16.236.1' */;
GRANT ALL ON test.* TO 'test'@'172.16.236.1';
GRANT SELECT ON mysql.* TO 'test'@'172.16.236.1';
GRANT RELOAD,SHUTDOWN ON *.* TO 'test'@'172.16.236.1';
SET PASSWORD FOR 'test'@'172.16.236.1' = PASSWORD('test');

/*!50002 CREATE USER 'test_nopwd'@'172.16.236.1' */;
GRANT SELECT ON test.* TO 'test_nopwd'@'172.16.236.1';

/*!50002 CREATE USER 'test_oldpwd'@'172.16.236.1' */
/*!50500   IDENTIFIED WITH 'mysql_old_password' */;
GRANT SELECT ON test.* TO 'test_oldpwd'@'172.16.236.1';
/*!40002 SET old_passwords = 1*/;
SET PASSWORD FOR 'test_oldpwd'@'172.16.236.1' = PASSWORD('test');

/*!50002 CREATE USER 'test_oldnopwd'@'172.16.236.1' */
/*!50500   IDENTIFIED WITH 'mysql_old_password' */;
GRANT SELECT ON test.* TO 'test_oldnopwd'@'172.16.236.1';

/*!50606 SET old_passwords = 2 */;

/*!50606 CREATE USER 'test_sha256pwd'@'172.16.236.1' IDENTIFIED WITH 'sha256_password' */;
/*!50606 SET PASSWORD FOR 'test_sha256pwd'@'172.16.236.1' = PASSWORD('test') */;
/*!50606 GRANT SELECT ON test.* TO 'test_sha256pwd'@'172.16.236.1' */;

/*!50606 CREATE USER 'test_sha256nopwd'@'172.16.236.1' IDENTIFIED WITH 'sha256_password' */;
/*!50606 GRANT SELECT ON test.* TO 'test_sha256nopwd'@'172.16.236.1' */;

FLUSH PRIVILEGES;

SELECT
  host
, user
/*!50500 , plugin */
, password
/*!50500 , authentication_string */
FROM mysql.user
;
