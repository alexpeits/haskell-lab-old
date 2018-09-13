{-# LANGUAGE OverloadedStrings #-}
module TestDB where

import Database.PostgreSQL.Simple

main :: IO ()
main = do
  conn <- connectPostgreSQL "host=postgres user=postgres port=5432 dbname=credit"
  x <- query_ conn "select avg(id) from credits"
  print (x :: [Only (Maybe Double)])

-- *** Exception: Incompatible {errSQLType = "int4", errSQLTableOid = Just (Oid 16395), errSQLField = "id", errHaskellType = "Text", errMessage = "types incompatible"
-- *** Exception: UnexpectedNull {errSQLType = "numeric", errSQLTableOid = Nothing, errSQLField = "avg", errHaskellType = "Double", errMessage = ""}
