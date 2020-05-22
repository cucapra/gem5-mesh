#!/usr/bin/env stack
{- stack --resolver lts-15.2 script 
 --package interpolatedstring-perl6
 --package shakespeare
 --package neat-interpolation
 --package interpolate
 --package text
 --package containers
 --package regex-tdfa
 --package mtl
 --package transformers
 --package split
 --package prettyprinter
-}

--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}
import Text.InterpolatedString.Perl6 (qq)
-- import Data.String.Here (i, iTrim, here)
import NeatInterpolation (text)
-- import Text.Shakespeare.Text (st, sbt, text)
-- import Data.String.Interpolate (i)
-- import Data.String.Interpolate.Util (unindent)
import qualified Data.Text as T 
import qualified Data.Text.IO as T_IO
import qualified Data.Map.Strict as M
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Text.Printf (printf)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import Text.Printf (printf)
import Data.Text.Prettyprint.Doc

data ProgAST = 
  VecSimcV1 {
    kernel_name :: String,
    kernel_body :: [String],
    vissue_blocks :: M.Map Int [String]
  }

emptyProg = 
  VecSimcV1 {
    kernel_name = "",
    kernel_body = [],
    vissue_blocks = M.empty
  }

main :: IO ()
main = splitter "vvadd.c"
-- main = tokenize "vvadd.c"

tokenize :: FilePath -> IO()
tokenize filename = do
  vec_simcv2_src <- readFile filename
  let vec_simcv1_toks = map (show . linewise_tokenize) . lines $ vec_simcv2_src
  sequence_ . map putStrLn $ vec_simcv1_toks

splitter :: FilePath -> IO ()
splitter filename = do
  vec_simcv2_src <- readFile filename
  let vec_simcv1_src = compile vec_simcv2_src
  putStrLn vec_simcv1_src
  where compile = emit_intermingled_vec_simcv1 . linewise_split

emit_intermingled_vec_simcv1 :: ProgAST -> String
emit_intermingled_vec_simcv1 prog = show $
    fun_def "void" (kernel_name prog) ([("int", "trillium_codegen_mask")]++default_args)
      $ vsep
      [(codegen_wrapper "Kernel Start Boilerplate" beginning_boilerplate),
      (codegen_wrapper "Kernel Body" (vsep . map pretty . kernel_body $ prog)),
      (codegen_wrapper "Kernel End Boilerplate" end_boilerplate)]
    where
      default_args = []
  
      fun_def ret_type fun_name params body =
        pretty ret_type <+> pretty fun_name <> pretty_params params <> (nest 2 . braces) (line <> body)
        where pretty_params = tupled . map (\(typ, x) -> pretty typ <+> pretty x)

      fun_call fun_name args = pretty fun_name <> (tupled . map pretty) args

      stmt d = d <> pretty ";"
      stmts = vsep . map stmt

      asm :: [String] -> String -> Doc ann
      asm quals ass = pretty "asm" <+> (hsep . map pretty) quals <> (parens $ pretty ass)

      intermingle scalar vector = vsep
        [pretty "#ifdef SCALAR",
         scalar,
         pretty "#elif defined VECTOR",
         vector,
         pretty "#endif"]

      beginning_boilerplate = 
        intermingle
          (stmt $ fun_call "VECTOR_EPOCH" ["mask"])
          (pretty "//nothing")

      end_boilerplate = 
        intermingle
          (stmts [fun_call "DEVEC" ["devec_0"],
                  asm ["volatile"] "fence",
                  asm [] "scalar return", 
                  pretty "return"])

          (stmts [asm [] "vector return",
                  pretty "return"])

      codegen_wrapper :: String -> Doc ann -> Doc ann
      codegen_wrapper blk_name code_blk = vsep
        [pretty $ ([qq|//***** TRILLIUM CODEGEN: {blk_name} *****|] :: String),
        code_blk,
        pretty [text|//********* TRILLIUM CODEGEN *********|]]

       

    



data VecSimcV2Parser = State VecSimcV2ParserState ProgAST
data VecSimcV2ParserState =
  V2 {fsm_state :: FSMState,
      line_no :: Int,
      prog :: ProgAST,
      curr_vissue_key :: Int}

-- input: Vector-SIMCv2 source text
-- output: Vector-SIMCv1 ast
linewise_split :: String -> ProgAST
linewise_split vec_simcv2_src =
  let final_parser_state = execState (parser vec_simcv2_src) init
  in prog final_parser_state
  where parser :: String -> State VecSimcV2ParserState ()
        parser = foldl (>>) (pure ()) . map linewise_parse . map linewise_tokenize . lines
        init = V2 { fsm_state = FSMKernelBegin,
                    prog = emptyProg,
                    curr_vissue_key = 0,
                    line_no = 0}


-- source formatted as list of code lines, each with a line num.
linewise_parse :: VecSimcV2Line -> State VecSimcV2ParserState ()
linewise_parse line = do
  curr_state <- gets fsm_state
  curr_line_no <- inc_line_no
  case transition curr_state line of
    (FSMError,_) -> return . error . show . vsep . map pretty $
                                ["state transition failure:",
                                printf "attempted transition from state %s" (show curr_state),
                                printf "on line $curr_line_no:",
                                show line]

    (new_state, side_effect) -> do modify (\s -> s {fsm_state = new_state}); side_effect
  where inc_line_no = mk_fresh line_no (\n ps -> ps {line_no= n+1})


  

type ParserEffect = State VecSimcV2ParserState ()
data FSMState = FSMKernelBegin | FSMKernelBody | FSMScalarLoop | FSMVector | FSMKernelEnd | FSMError deriving (Show)
data VecSimcV2Line = KernelBeginPragma String | KernelEndPragma | VectorBeginPragma | VectorEndPragma
                     | ScalarLoopPragma | KernelCode String deriving (Show, Eq)

transition :: FSMState -> VecSimcV2Line -> (FSMState, ParserEffect)
-- copy-paste lines before begin pragma
transition FSMKernelBegin (KernelCode line) =
  (FSMKernelBegin, modify $ insertIntoKernelBody line)
  
transition FSMKernelBegin (KernelBeginPragma name) =
  (FSMKernelBody, modify $ \s -> s {prog = (prog s) { kernel_name=name} } )
transition FSMKernelBody (KernelCode line) = 
  (FSMKernelBegin, modify $ insertIntoKernelBody line)

-- vissue block parse states
transition FSMKernelBody VectorBeginPragma = 
  (FSMVector, 
    do  vissue_key <- mk_fresh_vissue_key
        modify $ \s -> s {  curr_vissue_key = vissue_key })
  where mk_fresh_vissue_key = mk_fresh curr_vissue_key (\n ps -> ps {curr_vissue_key= n+1} )

transition FSMVector (KernelCode line) =
  (FSMVector, modify $ insertIntoVissueBlk line)
        
transition FSMVector VectorEndPragma = 
  (FSMKernelBody, pure ())


-- scalar loop parse states
transition FSMKernelBody ScalarLoopPragma = (FSMScalarLoop, pure ())
transition FSMScalarLoop (KernelCode _) = (FSMKernelBody, pure ())

-- kernel end
transition FSMKernelBody KernelEndPragma = (FSMKernelEnd, pure ())

-- otherwise
transition _ _ = (FSMError, pure ())




linewise_tokenize :: String -> VecSimcV2Line
linewise_tokenize line
  | [name] <- regex_parse line [qq|#pragma trillium vec_simd begin ([{alpha}]+[{alpha_num}|_]*)|]
    = KernelBeginPragma name
  | line =~ "#pragma trillium vec_simd end"
    = KernelEndPragma
  | line =~ "#pragma trillium vector begin"
    = VectorBeginPragma
  | line =~ "#pragma trillium vector end"
    = VectorEndPragma
  | line =~ "#pragma trillium scalar loop"
    = ScalarLoopPragma
  | otherwise
    = KernelCode line
  where 
    alpha = "a-z|A-Z"
    num = "0-9"
    alpha_num = alpha ++ "|" ++ num



regex_parse :: String -> String -> [String]
regex_parse line regex =  
  let (_,_,_,matches) = line =~ regex :: (String,String,String,[String])
  in matches


-- (+++) :: T.Text -> T.Text -> T.Text
-- t1 +++ t2 = T.intercalate "" [t1, t2]


mk_fresh :: (VecSimcV2ParserState -> a)
          -> (a -> VecSimcV2ParserState -> VecSimcV2ParserState)
          -> State VecSimcV2ParserState a
mk_fresh field_getter field_setter = do
  field <- gets field_getter
  modify (field_setter field)
  gets field_getter

-- TODO: inefficient to append to end of list
--        an alternative uses Sequence instead, which allows strict, both-ended appends
-- TODO: Haskell's record field update syntax is too verbose. Learn about and implement a version using Lenses.
insertIntoVissueBlk :: String -> VecSimcV2ParserState -> VecSimcV2ParserState
insertIntoVissueBlk line ps = 
  ps {prog = (prog ps) {vissue_blocks = M.insertWith (flip (++)) (curr_vissue_key ps) [line] (vissue_blocks $ prog ps)}}

insertIntoKernelBody :: String -> VecSimcV2ParserState -> VecSimcV2ParserState
insertIntoKernelBody line s = s {prog = (prog s) { kernel_body= (kernel_body $ prog s) ++ [line]}}
