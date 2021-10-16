
-- Chapter 2
-- Types and Functions

add a b = a + b

x = 10
-- x = 11

mydrop n xs = if n <= 0 || null xs
              then xs
              else mydrop (n-1) (tail xs)


lastbutOne xs = last $ take (length xs - 1) xs
