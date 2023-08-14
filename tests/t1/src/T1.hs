module T1 where

-- | Compute the list of integers consisting of all (positive) multiples
-- of 13 which are less than 10000.
-- It is up to you whether you use 'map' and 'takeWhile' (from the @Prelude@),
-- list comprehension or another technique.
--
-- >>> take 3 multiplesOf13
-- [13,26,39]
--
-- >>> length multiplesOf13
-- 769
--
multiplesOf13 :: [Int]
multiplesOf13 = error "implement multiplesOf13!"

-- | Implement a function that /intertwines/ two lists, taking elements from
-- both lists in turn until one list runs out of elements.
-- (If both arguments are infinite lists, then the result will also be an infitie
-- list.)
--
-- >>> intertwine [1, 2, 3] [10, 11]
-- [1,10,2,11,3]
--
-- >>> intertwine "abcdef" "XY"
-- "aXbYc"
--
-- >>> intertwine [] [False, True]
-- []
--
-- >>> intertwine [True] [False, False, False, False]
-- [True,False]
--
intertwine :: [a] -> [a] -> [a]
intertwine = error "implement intertwine!"

-- | Define a new polymorphic data type 'Employee' for representing an employee
-- using /record syntax/. The record should have four fields, 'firstName' (a
-- 'String'), 'lastName' (also a 'String'), 'salary' (a 'Double') and 'info'
-- (of polymorphic type @a@).
-- Derive 'Show', 'Eq', 'Ord' and 'Read' instances for this type.
--
data Employee a = Employee
    { -- add appropriate fields to this record type!
    }

-- | Write a function that computes the /full name/ of an 'Employee'
-- (first name and last name, separated by a space).
--
-- >>> fullName $ Employee "Charles" "Hoskinson" 1000 True
-- "Charles Hoskinson"
--
fullName :: Employee a -> String
fullName = error "implement fullName!"

-- | Write a function that doubles the salary of an 'Employee' (and doesn't
-- change any of the other three fields). Use record update syntax!
--
-- >>> doubleSalary $ Employee "Charles" "Hoskinson" 1000 "info"
-- Employee {firstName = "Charles", lastName = "Hoskinson", salary = 2000.0, info = "info"}
--
doubleSalary :: Employee a -> Employee a
doubleSalary = error "implement doubleSalary!"

-- | Make 'Employee' an instance of class 'Functor'.
--
-- >>> not <$> Employee "Charles" "Hoskinson" 1000 True
-- Employee {firstName = "Charles", lastName = "Hoskinson", salary = 1000.0, info = False}
--
instance Functor Employee where
    fmap = error "implement fmap!"

-- | Write a function that "forgets" the info of an 'Employee', but keeps
-- the other three fields untouched. Use `fmap`!
--
-- >>> forgetInfo $ Employee "Charles" "Hoskinson" 1000 True
-- Employee {firstName = "Charles", lastName = "Hoskinson", salary = 1000.0, info = ()}
--
forgetInfo :: Employee a -> Employee ()
forgetInfo = error "Implement forget info!"

-- | Write a function that takes a list of employees and extracts their 'info'.
-- (The resulting list should contain the info of each employee in the argument
-- list in the same order.)
--
-- >>> :{
--  gatherInfo [ Employee "Charles" "Hoskinson" 1000 True
--             , Employee "Alejandro" "Garcia" 900 False
--             ]
-- :}
-- [True,False]
--
gatherInfo :: [Employee a] -> [a]
gatherInfo = error "implement gatherInfo!"

-- | A type for /rose trees/.
data Rose a = Fork a [Rose a] deriving Show

-- | Collect all data from a rose tree in a list (in /pre-order/).
--
-- >>> roseToList $ Fork 'x' [Fork 'y' [], Fork 'z' []]
-- "xyz"
--
-- >>> roseToList $ Fork True [Fork False [], Fork False [Fork True []]]
-- [True,False,False,True]
--
roseToList :: Rose a -> [a]
roseToList = error "implement roseToList!"

-- | Sum all data in a /rose tree/. Use 'sum' for lists and 'roseToList'.
--
-- >>> sumRose $ Fork 1 [Fork 2 [], Fork 3 []]
-- 6
--
sumRose :: Num a => Rose a -> a
sumRose = error "implement sumRose!"
