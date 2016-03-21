module Imply where

import Text.Regex

-- get named data and type:
-- ^(type|data) ([A-Z][a-z]*).*-- ([a-z][a-z]?)$
-- get explicit constructors:
-- ([A-Z][a-z]*[0-9])(( [a-z][a-z]?[0-9]?)+
-- get manual converters:
-- ^([a-z]+[0-9])((_[a-z][a-z]?)*)(_[a-z]+[0-9]?)((_[a-z][a-z]?)*)
-- get required converters:
-- ([a-z]+[0-9])((_[a-z][a-z]?)*)(_[a-z]+[0-9?])((_[a-z][a-z]?)*)
