#!/usr/bin/env stack
{- stack --resolver lts-15.2 script 
 --package interpolatedstring-perl6
 --package neat-interpolation
 --package text
 --package containers
 --package regex-tdfa
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

data ProgAST = 
  VecSimcV1 {
    kernel_name :: T.Text,
    body :: [T.Text],
    vissue_blocks :: M.Map Int BasicBlock
  }
type BasicBlock = [T.Text]
emptyProg = 
  VecSimcV1 {
      kernel_name="", 
      body=[],
      vissue_blocks=M.empty }


main :: IO ()
main = compile KernelStart "empty_kernel.c"

compile component filename = do
  vec_simcv2_src <- T_IO.readFile filename
  let vec_simcv1_ast = split component emptyProg $ T.lines vec_simcv2_src
  T_IO.putStrLn $ intermingled_vvadd_simcv1 vec_simcv1_ast


intermingled_vvadd_simcv1 :: ProgAST -> T.Text
intermingled_vvadd_simcv1 prog =
  let fun_name = kernel_name prog
      kernel_body = T.intercalate "\n" (body prog)
  in
    [text|
      void $fun_name(int $mask) {
        /* ----- boilerplate ----- */
        ${beginning_boilerplate}
        /* ----- boilerplate ----- */


        /* ----- kernel start -----*/
        $kernel_body        
        /* ----- kernel end -----*/


        /* ----- boilerplate ----- */
        ${end_boilerplate}
        /* ----- boilerplate ----- */
      }|]
  where mask = "trillium_codegen_mask"
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
    




data SplitterState = KernelStart | KernelBody | KernelEnd

split :: SplitterState -> ProgAST -> [T.Text] -> ProgAST
split KernelStart accum_ast (line:rest)
  | [name] <- decompose line start_pragma_regex
      = split KernelBody (accum_ast { kernel_name=name }) rest
  | otherwise
      = error [qq|expected kernel start pragma matching regex: 
                  $start_pragma_regex|]
  where start_pragma_regex = [qq|#pragma trillium vec_simd start ([$alpha]+[alpha_num|_]*)|]

split KernelBody accum_ast (line:rest)
  | line =~ "for(.+;.+;.+) {"
      = split KernelBody (accum_ast { body = reflected_for:(body accum_ast) }) rest
  | line =~ end_pragma_regex 
      = accum_ast
  | otherwise
      = error "error while parsing kernel body"

  where end_pragma_regex = [qq|#pragma trillium vec_simd end|]
        reflected_for = intermingle line [qq|while($blackhole) \{|]
        blackhole = "bh"

split _ _ [] = error "reached end of file unexpectedly"

kernel_start_pragma = "#pragma trillium vec_simd start"
alpha = "a-z|A-Z"
num = "0-9"
alpha_num = [text|$alpha|$num|]
iden = [text|[$alpha]+|]
decompose :: T.Text -> T.Text -> [T.Text]
decompose line regex =  
  let (_,_,_,matches) = line =~ regex :: (T.Text,T.Text,T.Text,[T.Text])
  in matches


