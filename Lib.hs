{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant return" #-}
module Lib(start) where

import Data.Char
import System.IO
import Control.Applicative
import Data.List



data Variable = Variable {
    name :: String,
    vtype :: String,
    value :: Int
} deriving Show

type Env = [Variable]

--environment management
updateEnv :: Variable -> Parser String
updateEnv var = P (\env input -> case input of
                     xs -> [((modifyEnv env var),"",xs)])

modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = if (name x) == (name newVar) then
                            [newVar] ++ xs
                          else
                            [x] ++ modifyEnv xs newVar

readVariable :: String -> Parser Int
readVariable name = P (\env input -> case searchVariable env name of
    [] -> []
    [value] -> [(env, value , input)])

searchVariable :: Env -> String -> [Int]
searchVariable [] queryname = []
searchVariable (x:xs) queryname = if (name x) == queryname
                                       then [(value x)]
                                       else
                                            searchVariable xs queryname



newtype Parser a = P (Env -> String -> [(Env, a, String)])

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) env inp = p env inp

instance Functor Parser where
    fmap g p = P (\env inp -> case parse p env inp of
        [] -> []
        [(env, v, out)] -> [(env, g v, out)])

instance Applicative Parser where
    pure v = P (\env inp -> [(env,v,inp)])

    pg <*> px = P (\env inp -> case parse pg env inp of
        [] -> []
        [(env,g,out)] -> parse (fmap g px) env out)

instance Monad Parser where
    return v = P (\env inp -> [(env,v,inp)])

    p >>= f = P (\env inp -> case parse p env inp of
        [] -> []
        [(env,v,out)] -> parse (f v) env out)

instance Alternative Parser where
    empty = P (\env inp -> [])

    p <|> q = P (\env inp -> case parse p env inp of
        [] -> parse q env inp
        [(env,v,out)] -> [(env,v,out)])

--Parser function
item :: Parser Char
item = P (\env inp -> case inp of
    [] -> []
    (x:xs) -> [(env, x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do {
    x <- item;
    if p x then return x else empty;
}


digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

ident :: Parser String
ident = do {
    removeSpaces;
    x <- lower;
    xs <- many alphaNum;
    removeSpaces;
    return (x:xs);
    }

removeSpaces :: Parser ()
removeSpaces =
    do {
    many (sat isSpace);
    return ();
    }

natural :: Parser Int
natural = do {
    xs <- some digit;
    return (read xs);

}

integer :: Parser Int
integer = do { char '-';
              n <- natural;
              return (-n);}
          <|>
              natural

space :: Parser ()
space = do {
    many (sat isSpace);
    return ();
    }

symbol :: String -> Parser String
symbol [] = return ""
symbol (x:xs) = do {sat (x ==);
                    symbol xs;
                    return (x:xs);}

--Defining parser for arithmetic expression
aExp :: Parser Int
aExp = do{
        t <- aTerm;
        symbol "+";
        a <- aExp;
        return (t + a);
    } <|>
    do {
        t <- aTerm;
        symbol "-";
        a <- aExp;
        return (t - a);
    } <|>
    do {aTerm}

aTerm :: Parser Int
aTerm = 
    do{
        f <- aFactor;
        symbol "*";
        a <- aTerm;
        return (f * a);
    } <|>
    do{
        f <- aFactor;
        symbol "/";
        a <- aTerm;
        return (f `div` a);
    } <|>
    do{aFactor}


aFactor :: Parser Int
aFactor = 
    do {
        symbol "(";
        a <- aExp;
        symbol ")";
        return a
    } <|>
    do{
        i <- ident;
        readVariable i;
    }<|>
    do{
        x <- ident;
        symbol "#";
        i <- aExp;
        symbol "#";
        readVariable ( x ++ "#" ++ (show i) ++ "#" );
    }<|>
    do{
        integer;
    }

--Consume aExp
consumeAexp :: Parser String
consumeAexp = 
    do{
        t <- consumeTerm;
        symbol "+";
        a <- consumeAexp;
        return (t ++ "+" ++ a);
    } <|>
    do{
        t <- consumeTerm;
        symbol "-";
        a <- consumeAexp;
        return (t ++ "-" ++ a)
    } <|>
    do {
        consumeTerm;
    }


consumeTerm :: Parser String
consumeTerm =
    do{
        f <- consumeFactor;
        symbol "*";
        t <- consumeTerm;
        return (f ++ "*" ++ t);
    } <|>
    do{
        f <- consumeFactor;
        symbol "/";
        t <- consumeTerm;
        return (f ++ "/" ++ t);
    } <|>
    do {
        consumeFactor;
    }


consumeFactor :: Parser String
consumeFactor =
    do{
        symbol "(";
        a <- consumeAexp;
        symbol ")";
        return ("(" ++ a ++ ")");
    }
    <|>
    do{
        x <- ident;
        symbol "#";
        i <- consumeAexp;
        symbol "#";
        return (x ++ "#" ++ i ++ "#");
    }<|>
    do {
        i <- integer;
        return (show i);
       }
    <|>
       do{
           i <- ident;
           return i;
       }

--Boolean Expression
bExp :: Parser Bool
bExp = do{
        b1 <- bTerm;
        symbol "oo";
        b2 <- bExp;
        return (b1 || b2);
    }<|>
    do{bTerm;}

bTerm :: Parser Bool
bTerm =
    do {
        b1 <- bFactor;
        symbol "aa";
        b2 <- bTerm;
        return (b1 && b2)
    }<|>
    do{bFactor;}

bFactor :: Parser Bool
bFactor = 
    do{
        symbol "(";
        b <- bExp;
        symbol ")";
        return b;
    }<|>
    do{
        symbol "V";
        return True;
    }<|>
    do{
        symbol "F";
        return False;
    }<|>
    do{
        symbol "nn";
        b <- bFactor;
        return (not b);
    } <|>
    do{bComparison;}


bComparison :: Parser Bool
bComparison = 
    do {
        a <- aExp;
        symbol "=";
        b <- aExp;
        return (a == b);
    }<|>
    do{
        a <- aExp;
        symbol "<=";
        b <- aExp;
        return (a <= b);
    }<|>
    do{
        a <- aExp;
        symbol ">=";
        b <- aExp;
        return (a >= b);
    }<|>
    do{
        a <- aExp;
        symbol ">";
        b <- aExp;
        return (a > b);
    }<|>
    do{
        a <- aExp;
        symbol "<";
        b <- aExp;
        return (a < b);
    }<|>
    do{
        a <- aExp;
        symbol "!!";
        b <- aExp;
        return (a /= b)
    }


--Consume bExp
consumeBExp :: Parser String
consumeBExp = 
    do{
        b1 <- consumeBTerm;
        symbol "oo";
        b2 <- consumeBExp;
        return (b1 ++ "oo" ++ b2);
    }<|>
    do{consumeBTerm;}


consumeBTerm :: Parser String
consumeBTerm = 
    do {
        b1 <- consumeBFactor;
        symbol "aa";
        b2 <- consumeBTerm;
        return (b1 ++ "aa" ++ b2)
    }<|>
    do{consumeBFactor;}


consumeBFactor :: Parser String
consumeBFactor = 
    do{
        symbol "(";
        b <- consumeBExp;
        symbol ")";
        return ("(" ++ b ++ ")");
    }<|>
    do{
        symbol "V";
        return "V";
    }<|>
    do{
        symbol "F";
        return "F";
    }<|>
    do{
        symbol "nn";
        b <- consumeBFactor;
        return ("nn" ++ b);
    } <|>
    do{consumeBComparsion;}


consumeBComparsion :: Parser String
consumeBComparsion = 
    do {
        a <- consumeAexp;
        symbol "=";
        b <- consumeAexp;
        return (a ++ "=" ++ b);
    }<|>
    do{
        a <- consumeAexp;
        symbol "<=";
        b <- consumeAexp;
        return (a ++ "<=" ++ b);
    }<|>
    do{
        a <- consumeAexp;
        symbol ">=";
        b <- consumeAexp;
        return (a ++ ">=" ++ b);
    }<|>
    do{
        a <- consumeAexp;
        symbol ">";
        b <- consumeAexp;
        return (a ++ ">" ++ b);
    }<|>
    do{
        a <- consumeAexp;
        symbol "<";
        b <- consumeAexp;
        return (a ++ "<" ++ b);
    }<|>
    do{
        a <- consumeAexp;
        symbol "!!";
        b <- consumeAexp;
        return (a ++ "!!" ++ b)
    }
  
--Commands
program :: Parser String
program = 
    do {
        command;
        program;
    }<|>
    do {
        command;
    }


consumeProgram :: Parser String
consumeProgram = 
    do{
        c <- consumeCommand;
        p <- consumeProgram;
        return (c ++ p)
    }<|>
    do{
        c <- consumeCommand;
        return c}
  

command :: Parser String
command = do{
    removeSpaces;
    assignment;}
    <|>
    do{
    removeSpaces;
    ifElse;}
    <|>do{
    removeSpaces;
    whileST;}
    <|>
    do{
        removeSpaces;
        symbol "skip";
        symbol ".";}


consumeCommand :: Parser String
consumeCommand = do{removeSpaces; consumeAssignment;} <|> do{removeSpaces; consumeIfElse} <|> do{removeSpaces; consumeWhileST}  <|>do{removeSpaces; symbol "skip."} <|> do{removeSpaces; symbol "."}

executeWhile :: String -> Parser String
executeWhile c = P(\env input -> [(env, "", c ++ input)])

whileST :: Parser String
whileST = do{
    removeSpaces;
    w <- consumeWhileST;
    executeWhile w;
    symbol "while";
    removeSpaces; symbol "(";
    removeSpaces; b <- bExp;
    removeSpaces; symbol ")";
    removeSpaces; symbol "{";
    if b then
            do {
            program;
            symbol "}";
            executeWhile w;
            whileST;}
    else
            do{
                consumeProgram;
                symbol "}";
                return "";}
    }<|>
    do{
        w <- consumeWhileST;
        executeWhile w;
        symbol "do";
        removeSpaces; symbol "{";
        program;
        removeSpaces; symbol "}";
        removeSpaces; symbol "while";
        removeSpaces; symbol "(";
        removeSpaces; b <- bExp;
        removeSpaces; symbol ").";
        if b then
            do{
                executeWhile w;
                whileST;
            }
        else
            do{
                return "";
            }
    }

consumeWhileST :: Parser String
consumeWhileST = do {symbol "while";
                removeSpaces; symbol "(";
                removeSpaces; b <- consumeBExp;
                removeSpaces; symbol ")";
                removeSpaces; symbol "{";
                removeSpaces; x <- consumeProgram;
                removeSpaces; symbol "}";
                return ("while(" ++ b ++ "){" ++ x ++ "}");}
            <|>
            do {
                symbol "do";
                removeSpaces; symbol "{";
                removeSpaces; p <- consumeProgram;
                removeSpaces; symbol "}";
                removeSpaces; symbol "while";
                removeSpaces; symbol "(";
                removeSpaces; b <- consumeBExp;
                removeSpaces; symbol ").";
                return ("do{" ++ p ++ "}while(" ++ b ++ ").")
            }


ifElse :: Parser String
ifElse = do{
    symbol "if";
    removeSpaces; symbol "(";
    removeSpaces; b <- bExp;
    removeSpaces; symbol ")";
    if b then
        do{
            removeSpaces; symbol "{";
            removeSpaces; program;
            removeSpaces; symbol "}else{";
            removeSpaces; consumeProgram;
            removeSpaces; symbol "}";
            removeSpaces; return ""
        }
    else
        do{
            symbol "{";
            removeSpaces; consumeProgram;
            symbol "}";
            removeSpaces; symbol "else{";
            removeSpaces; program;
            removeSpaces; symbol "}";
            removeSpaces; return "";
        }
    }<|>
    do {
        symbol "if";
        removeSpaces; symbol "(";
        removeSpaces; b <- bExp;
        removeSpaces; symbol ")";
        if b then
            do{
                removeSpaces; symbol "{";
                removeSpaces; program;
                removeSpaces; symbol "}";
                removeSpaces; return "";
            }
        else
            do{
                symbol "{";
                removeSpaces; consumeProgram;
                symbol "}"; 
                return "";
            }
    }
    
consumeIfElse :: Parser String
consumeIfElse = do{
    symbol "if";
    removeSpaces; symbol "(";
    removeSpaces; b <- consumeBExp;
    removeSpaces; symbol ")";
    removeSpaces; symbol "{";
    removeSpaces; p1 <- consumeProgram;
    removeSpaces; symbol "}else{";
    removeSpaces; p2 <- consumeProgram;
    removeSpaces; symbol "}";
    return ("if(" ++ b ++ "){" ++ p1 ++ "}else{" ++ p2 ++ "}")
    }<|>
    do{
        symbol "if";
        removeSpaces; symbol "(";
        removeSpaces; b <- consumeBExp;
        removeSpaces; symbol ")";
        removeSpaces; symbol "{";
        removeSpaces; p <- consumeProgram;
        removeSpaces; symbol "}";
        return ("if(" ++ b ++ "){" ++ p ++ "}");
    }




assignment :: Parser String
assignment =
    do{
        x <- ident;
        symbol ":=";
        v <- aExp;
        symbol ".";
        updateEnv Variable{name = x, vtype = "Integer", value = v};}
    <|> do{ -- x = array
        x <- ident;
        symbol ":=";
        a <- array;
        symbol ".";
        saveArray x a;
    }<|> --x = y#1# x = elemento in posizione 1
    do{
        i1 <- ident;
        symbol ":=";
        i2 <- ident;
        symbol "#";
        a <- aExp; 
        symbol "#";
        symbol ".";
        val <- readVariable (i2 ++ "#" ++ (show a) ++ "#" );
        updateEnv Variable {name = i1, vtype ="Integer", value = val};
    }<|> -- y#1# = x elemento in posizione 1 uguale a x
    do{
        i1 <- ident;
        symbol "#";
        a <- aExp;
        symbol "#";
        symbol ":=";
        val <- aExp;
        symbol ".";
        array <- readArray i1;
        (if length array <= a then empty else updateEnv Variable{name = (i1 ++ "#" ++ (show a) ++ "#"), vtype = "array", value = val});
        }
    <|> -- array concatenated to another array
    do{
        i <- ident;
        symbol ":=";
        a1 <- array;
        removeSpaces; 
        symbol "conc";
        removeSpaces;
        a2 <- array;
        symbol ".";
        saveArray i (a1 ++ a2);
    }



consumeAssignment :: Parser String
consumeAssignment = 
    do{
        x<-ident;
        symbol ":=";
        a <-consumeArray;
        symbol ".";
        return (x ++ ":=" ++ a ++ ".")
    }<|> do{
        i1 <- ident;
        symbol ":=";
        i2 <- ident;
        symbol "#";
        a <- consumeAexp;
        symbol "#";
        symbol ".";
        return (i1 ++ ":=" ++ i2 ++ "#" ++ a ++ "#" ++ ".");
    }<|>
    do{
        id <- ident;
        symbol "#";
        a <- consumeAexp;
        symbol "#";
        symbol ":=";
        val <- consumeAexp;
        array <- readArray id;
        symbol ".";
        return (id ++ "#" ++ a ++ "#" ++ ":=" ++ val ++ ".")
    }<|>
    do{
        i <- ident;
        symbol ":=";
        a1 <- consumeArray;
        removeSpaces;
        symbol "conc";
        removeSpaces;
        a2 <- consumeArray;
        return (i ++ ":=" ++ a1 ++ "conc" ++ a2 ++ ".");
    }<|>do {
        x <- ident;
        symbol ":=";
        a <- consumeAexp;
        symbol ".";
        return (x ++ ":=" ++ a ++ ".")
        }



--array management
saveArray :: String -> [Int] -> Parser String
saveArray var val = P(\env input -> [(updateArray env var val, "", input)])

updateArray :: Env -> String -> [Int] -> Env
updateArray env var val = foldl (modifyEnv) env l 
                            where l = zipWith (\a i ->  Variable {name = var ++ "#" ++ (show i) ++ "#", vtype = "array", value = a}) val [0..]

searchArray :: Env -> String -> [Int]
searchArray env array = case searchVariable env x of
    [] -> []
    value -> concat([value] ++ map (\var -> searchVariable env var) xs )
    where  
        (x:xs) = map (\i -> (array ++ "#" ++ (show i) ++ "#")) [0..l]
        l = countElem env
        countElem [] = 0
        countElem (x:xs) = if (array ++ "#") `isPrefixOf` (name x)
            then 1 + countElem xs
            else countElem xs
                                                    
readArray :: String -> Parser [Int]
readArray name = P(\env input -> case searchArray env name of
                    [] -> []
                    value -> [(env, value, input)])
                    
--array
array :: Parser [Int]
array = do {
    symbol "#";
    a <- aItem;
    symbol "#";
    return a;
    }<|>
    do{
        i <- ident;
        readArray i; 
    }

consumeArray :: Parser String
consumeArray = do{
    symbol "#";
    a <- consumeArrayTerm;
    symbol "#";
    return ("#" ++ a ++ "#")
    }<|> ident

aItem :: Parser [Int]
aItem = do{
    a <- integer;
    symbol ",";
    as <- aItem;
    return ([a] ++ as);
    }<|>
    do{
        a <- integer;
        return [a];
    }

consumeArrayTerm :: Parser String
consumeArrayTerm = do{
    a <- consumeAexp;
    symbol ",";
    as <- consumeArrayTerm;
    return (a ++ "," ++ as)
    }<|> consumeAexp

eval :: String -> Env
eval c = case parse program [] c of
             []          -> error "Invalid Input"
             [(e, _, [])]  -> e
             [(e, _, out)] -> error $ "Invalid input: unused '" ++ out ++ "'"


start :: IO String
start = do {
    putStr "SIMP> ";
    hFlush stdout;
    input <- getLine;

    if (input == "exit") then return "See you!"
    else do {
        print (eval input);
        start;
        }
    }
    