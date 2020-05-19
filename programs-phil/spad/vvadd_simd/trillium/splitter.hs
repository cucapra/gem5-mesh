#!/usr/bin/env stack
{- stack --resolver lts-15.2 script 
 --package interpolatedstring-perl6
 --package neat-interpolation
 --package text
 --package containers
 --package regex-tdfa
 --package mtl
 --package transformers
-}

{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
import Text.InterpolatedString.Perl6 (qq)
-- import Data.String.Here (i, iTrim, here)
import NeatInterpolation (text)
import qualified Data.Text as T 
import qualified Data.Text.IO as T_IO
import qualified Data.Map.Strict as M
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Text.Printf (printf)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class

data ProgAST = 
  VecSimcV1 {
    kernel_name :: T.Text,
    kernel_body :: T.Text,
    vissue_blocks :: M.Map Int T.Text
  }

-- emptyProg = 
--   VecSimcV1 {
--       kernel_name="", 
--       kernel_body="",
--       vissue_blocks=M.empty }


main :: IO ()
main = splitter "vvadd.c"
-- main = tokenize "vvadd.c"

tokenize :: FilePath -> IO()
tokenize filename = do
  vec_simcv2_src <- T_IO.readFile filename
  let vec_simcv1_toks = map (T.pack . show . linewise_tokenize) . T.lines $ vec_simcv2_src
  sequence_ . map T_IO.putStrLn $ vec_simcv1_toks

splitter :: FilePath -> IO ()
splitter filename = do
  vec_simcv2_src <- T_IO.readFile filename
  let vec_simcv1_src = compile vec_simcv2_src
  T_IO.putStrLn vec_simcv1_src
  where compile = emit_intermingled_vec_simcv1 . linewise_split

emit_intermingled_vec_simcv1 :: ProgAST -> T.Text
emit_intermingled_vec_simcv1 prog =
    [text|
      void $fun_name(int $mask) {
        /* ----- boilerplate ----- */
        $beginning_boilerplate
        /* ----- boilerplate ----- */


        // vissue blocks found:
        /*
        $blocks
        */
        /* ----- kernel start -----*/
        $kernel_body_text        
        /* ----- kernel end -----*/


        /* ----- boilerplate ----- */
        ${end_boilerplate}
        /* ----- boilerplate ----- */
      }|]
  where fun_name = kernel_name prog
        kernel_body_text = kernel_body prog
        blocks = M.foldMapWithKey
          (\blk_key blk -> 
            "vissue block #" +++ (T.pack. show) blk_key +++ ":\n" +++ blk) 
          (vissue_blocks prog)
        mask = "trillium_codegen_mask"
        vector_epoch_arg = "trillium_codegen_vearg"
        beginning_boilerplate = 
          intermingle 
            [text|VECTOR_EPOCH(${vector_epoch_arg});|]
            [text|//nothing|]
        end_boilerplate = 
          intermingle
            [text|
                DEVEC(devec_0);
                asm volatile(fence);
                asm(scalar return);
                return;|]
            [text|
                asm(vector return);
                return;|]
       

intermingle :: T.Text -> T.Text -> T.Text
intermingle scalar vector =
  [text|
    #ifdef SCALAR
      $scalar 
    #elif defined VECTOR
      $vector
    #endif|]
    



data VecSimcV2Parser = State VecSimcV2ParserState ProgAST
data VecSimcV2ParserState =
  V2 {fsm_state :: FSMState,
      parsed_kernel_name :: T.Text,
      parsed_vissue_blocks :: M.Map Int T.Text,
      parsed_kernel_body :: T.Text,
      fresh_bh :: Int,
      line_no :: Int}

-- input: Vector-SIMCv2 source text
-- output: Vector-SIMCv1 ast
linewise_split :: T.Text -> ProgAST
linewise_split vec_simcv2_src =
  let final_parser_state = execState (parser vec_simcv2_src) init
  in assemble_program final_parser_state
  where parser :: T.Text -> State VecSimcV2ParserState VecSimcV2ParserState
        parser = foldl (>>) get . map linewise_parse . map linewise_tokenize . T.lines
        init = V2 { fsm_state = FSMKernelBegin,
                    parsed_kernel_name = "",
                    parsed_vissue_blocks = M.empty,
                    parsed_kernel_body = "",
                    fresh_bh = 0,
                    line_no = 0}
        assemble_program ps = 
                VecSimcV1 { kernel_name = parsed_kernel_name ps,
                            kernel_body = parsed_kernel_body ps,
                            vissue_blocks = parsed_vissue_blocks ps }



-- source formatted as list of code lines, each with a line num.
linewise_parse :: VecSimcV2Line -> State VecSimcV2ParserState VecSimcV2ParserState
linewise_parse line = do
  curr_state <- gets fsm_state
  curr_line_no <- inc_line_no
  case transition curr_state line of
    FSMError -> return $ error [qq|state transition failure:
                                attempted transition from state $curr_state
                                                     on line ${curr_line_no}: 
                                $line|]

    new_state -> modify (\s -> s {fsm_state = new_state}) 
  get 
  where mk_fresh_bh = mk_fresh (fresh_bh) (\n ps -> ps {fresh_bh = n+1} )
        inc_line_no = mk_fresh (line_no) (\n ps -> ps {line_no= n+1} )


  

data FSMState = FSMKernelBegin | FSMKernelBody | FSMScalarLoop | FSMVector | FSMKernelEnd | FSMError deriving (Show)
data VecSimcV2Line = KernelBeginPragma T.Text | KernelEndPragma | VectorBeginPragma | VectorEndPragma
                     | ScalarLoopPragma | KernelCode T.Text deriving (Show, Eq)

transition :: FSMState -> VecSimcV2Line -> FSMState
transition FSMKernelBegin (KernelBeginPragma _) = FSMKernelBody
transition FSMKernelBody (KernelCode _) = FSMKernelBody

-- vissue block parse states
transition FSMKernelBody VectorBeginPragma = FSMVector
transition FSMVector VectorEndPragma = FSMKernelBody

-- scalar loop parse states
transition FSMKernelBody ScalarLoopPragma = FSMScalarLoop
transition FSMScalarLoop (KernelCode _) = FSMKernelBody

-- kernel end
transition FSMKernelBody KernelEndPragma = FSMKernelEnd

-- otherwise
transition _ _ = FSMError




linewise_tokenize :: T.Text -> VecSimcV2Line
linewise_tokenize line
  | [name] <- regex_parse line [qq|#pragma trillium vec_simd begin ([$alpha]+[alpha_num|_]*)|]
    = KernelBeginPragma name
  | line =~ [qq|#pragma trillium vec_simd end|]
    = KernelEndPragma
  | line =~ [qq|#pragma trillium vector begin|]
    = VectorBeginPragma
  | line =~ [qq|#pragma trillium vector end|]
    = VectorEndPragma
  | line =~ [qq|#pragma trillium scalar loop|]
    = ScalarLoopPragma
  | otherwise
    = KernelCode line
  where 
    alpha = "a-z|A-Z"
    num = "0-9"
    alpha_num = [text|$alpha|$num|]



regex_parse :: T.Text -> T.Text -> [T.Text]
regex_parse line regex =  
  let (_,_,_,matches) = line =~ regex :: (T.Text,T.Text,T.Text,[T.Text])
  in matches


(+++) :: T.Text -> T.Text -> T.Text
t1 +++ t2 = T.intercalate "" [t1, t2]


mk_fresh :: (VecSimcV2ParserState -> a)
          -> (a -> VecSimcV2ParserState -> VecSimcV2ParserState)
          -> State VecSimcV2ParserState a
mk_fresh field_getter field_setter = do
  field <- gets field_getter
  modify (field_setter field)
  gets field_getter

