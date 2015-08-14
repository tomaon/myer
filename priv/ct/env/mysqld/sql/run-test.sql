--
-- 11.2.1. Integer Types (Exact Value)
--          - INTEGER, INT, SMALLINT, TINYINT, MEDIUMINT, BIGINT
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/integer-types.html
--

DROP TABLE IF EXISTS data_types_11_2_1
;
CREATE TABLE data_types_11_2_1 (
  k  INTEGER NOT NULL PRIMARY KEY
, ti TINYINT
, si SMALLINT
, mi MEDIUMINT
, i  INT -- = INTEGER
, bi BIGINT
, ut TINYINT UNSIGNED
, us SMALLINT UNSIGNED
, um MEDIUMINT UNSIGNED
, ui INT UNSIGNED
, ub BIGINT UNSIGNED
, ix INT DEFAULT 99
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_2_1 (k, ti) VALUES
  (101, 0)
, (102, -128)
, (103, +127)
;
INSERT INTO data_types_11_2_1 (k, si) VALUES
  (201, 0)
, (202, -32768)
, (203, +32767)
;
INSERT INTO data_types_11_2_1 (k, mi) VALUES
  (301, 0)
, (302, -8388608)
, (303, +8388607)
;
INSERT INTO data_types_11_2_1 (k, i ) VALUES
  (401, 0)
, (402, -2147483648)
, (403, +2147483647)
;
INSERT INTO data_types_11_2_1 (k, bi) VALUES
  (501, 0)
, (502, -9223372036854775808)
, (503, +9223372036854775807)
;

INSERT INTO data_types_11_2_1 (k, ut) VALUES
  (151, 0)
, (152, 255)
;
INSERT INTO data_types_11_2_1 (k, us) VALUES
  (251, 0)
, (252, 65535)
;
INSERT INTO data_types_11_2_1 (k, um) VALUES
  (351, 0)
, (352, 16777215)
;
INSERT INTO data_types_11_2_1 (k, ui) VALUES
  (451, 0)
, (452, 4294967295)
;
INSERT INTO data_types_11_2_1 (k, ub) VALUES
  (551, 0)
, (552, 18446744073709551615)
;

COMMIT
;

SELECT * FROM data_types_11_2_1 ORDER BY k
;

--
-- 11.2.2. Fixed-Point Types (Exact Value) - DECIMAL, NUMERIC
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/fixed-point-types.html
--

DROP TABLE IF EXISTS data_types_11_2_2
;
CREATE TABLE data_types_11_2_2 (
  k  INTEGER NOT NULL PRIMARY KEY
, d  DECIMAL        -- (10,0)
, ds DECIMAL(5,2)
, dm DECIMAL(65,0)
, dd DECIMAL(31,30) -- M >= D
, de DECIMAL(31,15) -- limit? 
, n  NUMERIC        -- (10,0)
, ns NUMERIC(5,2)
, nm NUMERIC(65,0)
, nd NUMERIC(31,30) -- M >= D
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_2_2 (k, d) VALUES
  (101, 0)
, (102, -9999999999)
, (103, +9999999999)
;
INSERT INTO data_types_11_2_2 (k, ds) VALUES
  (201, 0)
, (202, -999.99)
, (203, +999.99)
;
INSERT INTO data_types_11_2_2 (k, dm) VALUES
  (301, 0)
, (302, -99999999999999999999999999999999999999999999999999999999999999999)
, (303, +99999999999999999999999999999999999999999999999999999999999999999)
;
INSERT INTO data_types_11_2_2 (k, dd) VALUES
  (401, 0)
, (402, -9.999999999999999999999999999999)
, (403, +9.999999999999999999999999999999)
;
INSERT INTO data_types_11_2_2 (k, de) VALUES
  (501, 0)
, (502, -  99999999999999.9)  -- ok 
, (503, - 999999999999999.9)  -- ok
, (504, -9999999999999999.9)  -- -1.0e16
, (505, - 999999999999999.99) -- -1.0e15
, (506, -9.99999999999999)    -- ok
, (507, -9.999999999999999)   -- -9.999999999999998
, (508, -9.9999999999999999)  -- -10.0 
, (512,    99999999999999.9)  -- ok 
, (513,   999999999999999.9)  -- ok
, (514,  9999999999999999.9)  -- 1.0e16
, (515,   999999999999999.99) -- 1.0e15
, (516,  9.99999999999999)    -- ok
, (517,  9.999999999999999)   -- 9.999999999999998
, (518,  9.9999999999999999)  -- 10.0
;
INSERT INTO data_types_11_2_2 (k, n) VALUES
  (601, 0)
, (602, -9999999999)
, (603,  9999999999)
;
INSERT INTO data_types_11_2_2 (k, ns) VALUES
  (701, 0)
, (702, -999.99)
, (703,  999.99)
;
INSERT INTO data_types_11_2_2 (k, nm) VALUES
  (801, 0)
, (802, -99999999999999999999999999999999999999999999999999999999999999999)
, (803,  99999999999999999999999999999999999999999999999999999999999999999)
, (804, -99999999999999999999999999999999999999999999999999999999999999999)
;
INSERT INTO data_types_11_2_2 (k, nd) VALUES
  (901, 0)
, (902, -9.999999999999999999999999999999)
, (903,  9.999999999999999999999999999999)
;

COMMIT
;

SELECT * FROM data_types_11_2_2 ORDER BY k
;

--
-- 11.2.3. Floating-Point Types (Approximate Value) - FLOAT, DOUBLE
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/floating-point-types.html
--

DROP TABLE IF EXISTS data_types_11_2_3
;
CREATE TABLE data_types_11_2_3 (
  k  INTEGER NOT NULL PRIMARY KEY
, f  FLOAT
, d  DOUBLE
, r  REAL
, uf FLOAT UNSIGNED
, ud DOUBLE UNSIGNED
, ur REAL UNSIGNED
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_2_3 (k, f) VALUES
  (101, 0)
, (102, -3.402823466E+38)
, (103, -1.175494351E-38)
, (104, +1.175494351E-38)
, (105, +3.402823466E+38)
;
INSERT INTO data_types_11_2_3 (k, d) VALUES
  (201, 0)
, (202, -1.7976931348623157E+308)
, (203, -2.2250738585072014E-308)
, (204, +2.2250738585072014E-308)
, (205, +1.7976931348623157E+308)
;
INSERT INTO data_types_11_2_3 (k, r) VALUES
  (301, 0)
, (302, -1.7976931348623157E+308)
, (303, -2.2250738585072014E-308)
, (304, +2.2250738585072014E-308)
, (305, +1.7976931348623157E+308)
;
INSERT INTO data_types_11_2_3 (k, uf) VALUES
  (401, 0)
, (404, +1.175494351E-38)
, (405, +3.402823466E+38)
;
INSERT INTO data_types_11_2_3 (k, ud) VALUES
  (501, 0)
, (504, +2.2250738585072014E-308)
, (505, +1.7976931348623157E+308)
;
INSERT INTO data_types_11_2_3 (k, ur) VALUES
  (601, 0)
, (604, +2.2250738585072014E-308)
, (605, +1.7976931348623157E+308)
;

COMMIT
;

SELECT * FROM data_types_11_2_3 ORDER BY k
;

--
-- 11.2.4. Bit-Value Type - BIT
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/bit-type.html
--

DROP TABLE IF EXISTS data_types_11_2_4
;
CREATE TABLE data_types_11_2_4 (
  k  INTEGER NOT NULL PRIMARY KEY
, b  BIT     -- (1)
, bm BIT(64)
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_2_4 (k, b) VALUES
  (101, 0) -- b'0'
, (102, 1) -- b'1'

;
INSERT INTO data_types_11_2_4 (k, bm) VALUES
  (201, 0)                   -- b'0000000000000000000000000000000000000000000000000000000000000000'
, (202, 1)                   -- b'0000000000000000000000000000000000000000000000000000000000000001'
, (203, 8)                   -- b'0000000000000000000000000000000000000000000000000000000000001000'
, (204, 16)                  -- b'0000000000000000000000000000000000000000000000000000000000010000'
, (205, 128)                 -- b'0000000000000000000000000000000000000000000000000000000010000000'
, (206, 256)                 -- b'0000000000000000000000000000000000000000000000000000000100000000'
, (207, 4096)                -- b'0000000000000000000000000000000000000000000000000001000000000000'
, (208, 9223372036854775808) -- b'1000000000000000000000000000000000000000000000000000000000000000'
;

COMMIT
;

SELECT k, hex(b), hex(bm) FROM data_types_11_2_4 ORDER BY k
;

--
-- 11.3.1. The DATE, DATETIME, and TIMESTAMP Types
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/datetime.html
--

DROP TABLE IF EXISTS data_types_11_3_1
;
CREATE TABLE data_types_11_3_1 (
  k  INTEGER NOT NULL PRIMARY KEY
, d  DATE
, dt DATETIME
, ts TIMESTAMP NULL
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_3_1 (k, d) VALUES
  (101, '2012-05-10')
;
INSERT INTO data_types_11_3_1 (k, dt) VALUES
  (201, '2012-05-10 12:34:56')
;
INSERT INTO data_types_11_3_1 (k, ts) VALUES
  (301, '2012-05-10 12:34:56')
;

COMMIT
;

SELECT * FROM data_types_11_3_1 ORDER BY k
;

--
-- 11.3.2. The TIME Type
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/time.html
--

DROP TABLE IF EXISTS data_types_11_3_2
;
CREATE TABLE data_types_11_3_2 (
  k  INTEGER NOT NULL PRIMARY KEY
, t  TIME
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_3_2 (k, t) VALUES
  (101, '12:34:56')
;

COMMIT
;

SELECT * FROM data_types_11_3_2 ORDER BY k
;

--
-- 11.3.3. The YEAR Type
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/year.html
--

DROP TABLE IF EXISTS data_types_11_3_3
;
CREATE TABLE data_types_11_3_3 (
  k  INTEGER NOT NULL PRIMARY KEY
, y  YEAR
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_3_3 (k, y) VALUES
  (101, '2012')
;

COMMIT
;

SELECT * FROM data_types_11_3_3 ORDER BY k
;

--
-- 11.4.1. The CHAR and VARCHAR Types
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/char.html
--

DROP TABLE IF EXISTS data_types_11_4_1
;
CREATE TABLE data_types_11_4_1 (
  k  INTEGER NOT NULL PRIMARY KEY
, c  CHAR(0)
, cf CHAR(5)
, cs CHAR(6)
, cm CHAR(7)    -- 255
, v  VARCHAR(0)
, vf VARCHAR(5)
, vs VARCHAR(6)
, vm VARCHAR(7) -- 65536
)
ENGINE=InnoDB
/*!50000 DEFAULT CHARACTER SET = utf8 */
;

INSERT INTO data_types_11_4_1 (k, c) VALUES
  (101, '')
, (102, '')
;
INSERT INTO data_types_11_4_1 (k, cf) VALUES
  (201, 'abcde')
, (202, 'てすと')
;
INSERT INTO data_types_11_4_1 (k, cs) VALUES
  (301, 'abcdef')
, (302, 'てすと')
;
INSERT INTO data_types_11_4_1 (k, cm) VALUES
  (401, 'abcdef')
, (402, 'てすと')
;
INSERT INTO data_types_11_4_1 (k, v) VALUES
  (501, '')
, (502, '')
;
INSERT INTO data_types_11_4_1 (k, vf) VALUES
  (601, 'abcde')
, (602, 'てすと')
;
INSERT INTO data_types_11_4_1 (k, vs) VALUES
  (701, 'abcdef')
, (702, 'てすと')
;
INSERT INTO data_types_11_4_1 (k, vm) VALUES
  (801, 'abcdef')
, (802, 'てすと')
;

COMMIT
;

SELECT * FROM data_types_11_4_1 ORDER BY k
;

--
-- 11.4.2. The BINARY and VARBINARY Types
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/binary-varbinary.html
--

DROP TABLE IF EXISTS data_types_11_4_2
;
CREATE TABLE data_types_11_4_2 (
  k  INTEGER NOT NULL PRIMARY KEY
, b  BINARY(0)
, bf BINARY(5)
, bs BINARY(6)
, bm BINARY(7)    -- 255
, v  VARBINARY(0)
, vf VARBINARY(5)
, vs VARBINARY(6)
, vm VARBINARY(7) -- 65536
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_4_2 (k, b) VALUES
  (101, '')
;
INSERT INTO data_types_11_4_2 (k, bf) VALUES
  (201, 'abcde')
;
INSERT INTO data_types_11_4_2 (k, bs) VALUES
  (301, 'abcdef')
;
INSERT INTO data_types_11_4_2 (k, bm) VALUES
  (401, 'abcdef')
;
INSERT INTO data_types_11_4_2 (k, v) VALUES
  (501, '')
;
INSERT INTO data_types_11_4_2 (k, vf) VALUES
  (601, 'abcde')
;
INSERT INTO data_types_11_4_2 (k, vs) VALUES
  (701, 'abcdef')
;
INSERT INTO data_types_11_4_2 (k, vm) VALUES
  (801, 'abcdef')
;

COMMIT
;

SELECT * FROM data_types_11_4_2 ORDER BY k
;

--
-- 11.4.3. The BLOB and TEXT Types
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/blob.html
--

DROP TABLE IF EXISTS data_types_11_4_3
;
CREATE TABLE data_types_11_4_3 (
  k  INTEGER NOT NULL PRIMARY KEY
, tb TINYBLOB
, b  BLOB
, mb MEDIUMBLOB
, lb LONGBLOB
, tt TINYTEXT
, t  TEXT
, mt MEDIUMTEXT
, lt LONGTEXT
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_4_3 (k, tb) VALUES
  (101, 'ABCDEF')
;
INSERT INTO data_types_11_4_3 (k, b) VALUES
  (201, 'ABCDEF')
;
INSERT INTO data_types_11_4_3 (k, mb) VALUES
  (301, 'ABCDEF')
;
INSERT INTO data_types_11_4_3 (k, lb) VALUES
  (401, 'ABCDEF')
;
INSERT INTO data_types_11_4_3 (k, tt) VALUES
  (501, 'ABCDEF')
;
INSERT INTO data_types_11_4_3 (k, t) VALUES
  (601, 'ABCDEF')
;
INSERT INTO data_types_11_4_3 (k, mt) VALUES
  (701, 'ABCDEF')
;
INSERT INTO data_types_11_4_3 (k, lt) VALUES
  (801, 'ABCDEF')
;

COMMIT
;

SELECT * FROM data_types_11_4_3 ORDER BY k
;

--
-- 11.4.4. The ENUM Type
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/enum.html
--

DROP TABLE IF EXISTS data_types_11_4_4
;
CREATE TABLE data_types_11_4_4 (
  k  INTEGER NOT NULL PRIMARY KEY
, e  ENUM('x-small', 'small', 'medium', 'large', 'x-large')
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_4_4 (k, e) VALUES
  (101, 'small')
, (102, 'large')
;

COMMIT
;

SELECT * FROM data_types_11_4_4 ORDER BY k
;

--
-- 11.4.5. The SET Type
--
--  @see http://dev.mysql.com/doc/refman/5.6/en/set.html
--

DROP TABLE IF EXISTS data_types_11_4_5
;
CREATE TABLE data_types_11_4_5 (
  k  INTEGER NOT NULL PRIMARY KEY
, s  SET('S', 'M', 'L')
)
ENGINE=InnoDB
;

INSERT INTO data_types_11_4_5 (k, s) VALUES
  (101, 'S')
, (102, 'L')
;

COMMIT
;

SELECT * FROM data_types_11_4_5 ORDER BY k
;
