#!/usr/bin/env stack
{- stack --resolver lts-15.2 script  
 --package split
 -}

module Pretty where

import Data.List
import Data.List.Split (splitOn)

main = putStrLn "Hello from 'pretty.hs'"

fun_def :: String -> String -> [(String,String)] -> Doc -> Doc
fun_def ret_type fun_name pars body =
  text ret_type <++> text fun_name <++> (parens . comma_sep_pars) pars <+|+>
    body
  where comma_sep_pars :: [(String,String)] -> Doc 
        comma_sep_pars = 
          intercalated (text ", ") . map (\(typ, var) -> text typ <++> text var)

fun_call :: String -> [String] -> Doc
fun_call fun_name args =
  text fun_name <++> (parens . intercalated (text ", ") . map text) args

asm :: [String] -> String -> Doc
asm quals s = stmt $ text "asm" <++> qualifiers <+> (parens . text) s
  where qualifiers = (intercalated (text " ") . map text) quals

stmt :: Doc -> Doc
stmt s = s <+> text ";"

stmts :: [Doc] -> Doc
stmts = intercalated Line . map stmt

code_block :: [Doc] -> Doc
code_block = braces . Nest 2 . stmts

lines :: [Doc] -> Doc
lines = intercalated Line


data Doc = 
    Text String
  | Concat Doc Doc  -- no space concat
  | Nil
  | Line
  | Nest Int Doc

layout :: Doc -> String
layout (Text s) = s
layout (Concat d1 d2) = layout d1 ++ layout d2
layout Nil = ""
layout Line = "\n"
layout (Nest n d) = indent n d
  where indent _ (Text s) = s
        indent n (Concat d1 d2) = indent n d1 ++ indent n d2
        indent _ Nil = layout Nil
        indent n Line = spaces n ++ layout Line
          where spaces n = intercalate "" $ replicate n " "
        indent n (Nest m d) = layout (Nest (n+m) d)
  
-- helper Doc functions
text :: String -> Doc
text = intercalated Line . map Text . splitOn "\n"

nest :: Int -> Doc -> Doc
nest = Nest

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = Concat d1 d2

-- single space concat
(<++>) :: Doc -> Doc -> Doc
d1 <++> d2 = Concat (Concat d1 (Text " ")) d2

-- newline concat
(<+|+>) :: Doc -> Doc -> Doc
d1 <+|+> d2 = Concat (Concat d1 Line) d2

sepBy :: Doc -> Doc -> Doc -> Doc
sepBy sep d1 d2 = d1 <+> sep <+> d2

intercalated :: Doc -> [Doc] -> Doc
intercalated sep ds = foldl (sepBy sep) Nil ds
  
parens d = text "(" <+> d <+> text ")"
braces d = text "{" <+> d <+> text "}"
brackets d = text "[" <+> d <+> text "]"


-- (<.>) :: Doc -> Doc -> Doc
-- (Doc s1) <.> (Doc s2) = Doc (s1++s2)

-- nil :: Doc
-- nil = Text ""

-- text :: String -> Doc
-- text t = Doc t

-- line :: Doc
-- line = Doc "\n"

-- nest :: Int -> Doc -> Doc
-- nest n d = Doc (foldl (++) "" (replicate n " ")) <.> d


-- type Doc = String
--
-- (<..>) :: Doc -> Doc -> Doc
-- (<..>) = (++)
--
-- nil :: Doc
-- nil = ""
--
-- text :: String -> Doc
-- text = id
--
-- line :: Doc
-- line = "\n"
--
-- nest :: Int -> Doc -> Doc
-- nest n = unlines . map (indentation ++) . lines
--   where indentation = (foldl (++) "" (replicate n " "))
--
-- layout :: Doc -> String
-- layout = id
--
--
-- (<.|.>) :: Doc -> Doc -> Doc
-- d1 <.|.> d2 = d1 <..> " " <..> d2
--
-- (<.||.>) :: Doc -> Doc -> Doc
-- d1 <.||.> d2 = d1 <..> line <..> d2

