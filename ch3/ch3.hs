
data BookInfo = Book Int String [String] deriving (Show)

data MagazineInfo = Magazine Int String [String] deriving (Show)

myInfo = Book 12345 "Programming" ["Richard Bird", "Oega de Moor"]

data BookReview = BookReview BookInfo CustomerID String

-- Type Synonyms

type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

-- Algebraic data types

-- data Bool = False | True

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                   | CashOnDelivery
                   | Invoice CustomerID
                   deriving (Show)

a = ("Porpoise","Grey")
b = ("Table","Oak")

data Cetacean = Cetacean String String
data Furniture = Furniture String String

c = Cetacean "Porpoise" "Grey"
d = Furniture "Table" "Oak"

-- x and y coordinates or length
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

-- Angle and distance (magnitude).
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)

-- Enumeration Types

data Roygbiv = Red
               | Orange
               | Yellow
               | Green
               | Blue
               | Indigo
               | Violet
               deriving (Eq, Show)

-- file: ch03/ShapeUnion.hs

type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector]

-- Pattern Matching

myNot :: Bool -> Bool
myNot True  = False
myNot False = True

sumList :: (Num a) => [a] -> a
sumList (x:xs) = x + sumList xs
sumList [] = 0

complicated (True, a, x:xs, 5) = (a, xs)

bookID (Book id title authors) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors

niceID (Book id _ _) = id
niceTitle (Book _ title _) = title
niceAuthors (Book _ _ authors) = authors


-- Record Syntax

data Customer = Customer {
       customerID      :: CustomerID,
       customerName    :: String,
       customerAddress :: Address
       } deriving (Eq, Show)


customer1 = Customer 123 "Bob" ["CYC", "HKU"]

customer2 = Customer {
             customerID = 123,
             customerName = "Bob",
             customerAddress = ["CYC", "HKU"]
             }


-- Parameterised Types

-- data Maybe a = Just a | Nothing


-- Recursive Types

data List a = Cons a (List a) | Nil deriving (Show)

-- file: ch03/BadIndent.hs
-- This is the leftmost column.

    -- Our first declaration is in column 4.
firstBadIndentation = 1

  -- Our second is left of the first, which is illegal!
secondBadIndentation = 2


-- Exercise


isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = case (x == (last xs)) of
                       False -> False
                       True  -> isPalindrome $ init xs
