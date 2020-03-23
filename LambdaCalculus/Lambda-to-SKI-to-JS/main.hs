{-# LANGUAGE OverloadedStrings #-}
import Data.List(concat)
import Data.Text(pack, unpack, replace)
import System.Process(readProcess)


(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>


data Expr = Var String | Apply Expr Expr | Lambda String Expr -- lambda calculus expression
            deriving (Eq, Show)

data Comb = I | K | S | Comb :$ Comb | Free [String] -- combinatory expression
            deriving (Eq, Show)

bindee :: String -> Comb -> Comb
bindee p (Free [p']) | p == p' = I
bindee p (f :$ g) = S :$ bindee p f :$ bindee p g
bindee _ e = K :$ e

-- Convert lambda expression to SKI basis expression
convert :: Expr -> Comb
convert (Apply f x) = convert f :$ convert x
convert (Lambda p e) = bindee p $ convert e
convert (Var i) = Free [i]

-- Normalize combinatory expression to executable form
normalize :: Comb -> String
normalize combExpression =
    combExpression
        |> show
        |> pack
        |> replace ":$ " "" 
        |> replace "S" "(S)"
        |> replace "K" "(K)"
        |> replace "I" "(I)" 
        |> replace "Free" "((Free)"
        |> replace "]" "))"
        |> replace "[" "("
        |> unpack


execute :: Expr -> [String] -> IO ()
execute lambdaExpression argList = 
    lambdaExpression
        |> convert
        |> normalize
        |> (++) "const S = f => g => x => f(x)(g(x));\nconst K = x => y => x;\nconst I = x => x;\nconst Free = x => eval(x);const z = 5;\n\nconst fn = "
        |> flip (++) (";\n\nconsole.log(fn" ++ (concat $ map (\x -> "(" ++ x ++ ")") argList) ++ ");\n") 
        |> \e -> (readProcess "node" ["-e", e] "" >>= putStrLn)

export :: Expr -> IO ()
export lambdaExpression =
    lambdaExpression
        |> convert
        |> normalize
        |> (++) "const S = f => g => x => f(x)(g(x));\nconst K = x => y => x;\nconst I = x => x;\nconst Free = x => eval(x);\n\nconst fn = "
        |> flip (++) (";\nmodule.exports = fn;\n") 
        |> writeFile "index.js"


test1 :: IO ()
test1 =
    "This is an algorithm validation test.\n"
        |> flip (++) "Go to https://tarao.orezdnu.org/LambdaJS/ and compare (\\f -> \\x -> f x) with ("
        |> flip (++) (unpack $ replace ":$ " "" $ pack $ show $ convert $ (Lambda "f" (Lambda "x" (Apply (Var "f") (Var "x")))))
        |> flip (++) ").\nDon't forget to define SKI combinators!\n"
        |> putStrLn

test2 :: IO ()
test2 =
    putStrLn "Unbound variable test. Running (\\x -> z) converted to SKI in Node.js with args: [100]. Note: z variable is predefined as z = 5" >>
    putStrLn "Expected value is: 5" >>
    execute (Lambda "x" (Var "z")) ["100"]

test3 :: IO ()
test3 =
    putStrLn "Eval mechanics test. Running (\\x -> 5) converted to SKI in Node.js with args: [100]" >>
    putStrLn "Expected value is: 5" >>
    execute (Lambda "x" (Var "5")) ["100"]

test4 :: IO ()
test4 =
    putStrLn "Apply test. Running ((\\x -> \\y -> x)(\\x -> x)) converted to SKI in Node.js with args: [0, 5]" >>
    putStrLn "Expected value is: 5" >>
    execute (Apply (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x"))) ["0", "5"]

test5 :: IO ()
test5 =
    putStrLn "Just variable test. Running (z) converted to SKI in Node.js with args: []. Note: z variable is predefined as z = 5" >>
    putStrLn "Expected value is: 5" >>
    execute (Var "z") []

test6 :: IO ()
test6 = 
    putStrLn "Reproduction of A combinator test. Running (\\f -> \\x -> f x) converted to SKI in Node.js with args: [x => x + 5, 5]" >>
    putStrLn "Expected value is: 10" >>
    execute (Lambda "f" (Lambda "x" (Apply (Var "f") (Var "x")))) ["x => x + 5", "5"]

test7 :: IO ()
test7 = 
    putStrLn "Reproduction of W combinator test. Running (\\f -> \\x -> f x x) converted to SKI in Node.js with args: [x => y => x * y, 5]" >>
    putStrLn "Expected value is: 25" >>
    execute (Lambda "f" (Lambda "x" (Apply (Apply (Var "f") (Var "x")) (Var "x")))) ["x => y => x * y", "5"]

test8 :: IO ()
test8 =
    putStrLn "Reproduction of C combinator test. Running (\\f -> \\x -> \\y -> f y x) converted to SKI in Node.js with args: [a => b => a - b, 35, 10]" >>
    putStrLn "Expected value is: -25" >>
    execute (Lambda "f" (Lambda "x" (Lambda "y" (Apply (Apply (Var "f") (Var "y")) (Var "x"))))) ["a => b => a - b", "35", "10"]

test9 :: IO ()
test9 = 
    putStrLn "Reproduction of B combinator test. Running (\\f -> \\g -> \\x -> f (g x)) converted to SKI in Node.js with args: [x => x * 4, x => x * 3, 3]" >>
    putStrLn "Expected value is: 36" >>
    execute (Lambda "f" (Lambda "g" (Lambda "x" (Apply (Var "f") (Apply (Var "g") (Var "x")))))) ["x => x * 4", "x => x * 3", "3"]

test10 :: IO ()
test10 =
    putStrLn "Manual code generation test." >>
    export (Lambda "f" (Lambda "g" (Lambda "x" (Apply (Var "f") (Apply (Var "g") (Var "x")))))) >>
    putStrLn "Code for B combinator has been written to index.js file. You may want to test it with https://tarao.orezdnu.org/LambdaJS/."


main :: IO ()
main = 
    test1 >> 
    test2 >>
    test3 >>
    test4 >>
    test5 >>
    test6 >>
    test7 >>
    test8 >>
    test9 >>
    test10

