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
import Text.Printf (printf)

data ProgAST = 
  VecSimcV1 {
    kernel_name :: T.Text,
    body :: T.Text,
    vissue_blocks :: M.Map Int T.Text
  }

emptyProg = 
  VecSimcV1 {
      kernel_name="", 
      body="",
      vissue_blocks=M.empty }


main :: IO ()
main = compile KernelStart "vvadd.c"

compile component filename = do
  vec_simcv2_src <- T_IO.readFile filename
  let vec_simcv1_ast = split component emptyProg $ T.lines vec_simcv2_src
  T_IO.putStrLn $ emit_intermingled_simcv1 vec_simcv1_ast


emit_intermingled_simcv1 :: ProgAST -> T.Text
emit_intermingled_simcv1 prog =
  let fun_name = kernel_name prog
      kernel_body = body prog
      blocks = M.foldMapWithKey
        (\blk_key blk -> 
          "vissue block #" +++ (T.pack. show) blk_key +++ ":\n" +++ blk) 
        (vissue_blocks prog)
  in
    [text|
      void $fun_name(int $mask) {
        /* ----- boilerplate ----- */
        ${beginning_boilerplate}
        /* ----- boilerplate ----- */


        // vissue blocks found:
        /*
        $blocks
        */
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
    




data SplitterState = KernelStart | KernelBody | KernelEnd | Vissue T.Text

split :: SplitterState -> ProgAST -> [T.Text] -> ProgAST
split KernelStart accum_ast (line:rest)
  | [name] <- decompose line start_pragma_regex
      = split KernelBody (accum_ast { kernel_name=name }) rest
  | otherwise
      = error [qq|expected kernel start pragma matching regex: 
                  $start_pragma_regex|]
  where start_pragma_regex = [qq|#pragma trillium vec_simd begin ([$alpha]+[alpha_num|_]*)|]

split KernelBody accum_ast (line:rest)
  | line =~ "for(.+;.+;.+) {"
      = split KernelBody (accum_ast { body = body accum_ast +++ reflected_for } ) rest
  | line =~ vissue_begin_regex
      = split (Vissue "") accum_ast rest
  | line =~ end_pragma_regex 
      = accum_ast
  | otherwise
      = error "error while parsing kernel body"
  where end_pragma_regex = [qq|#pragma trillium vec_simd end|]
        vissue_begin_regex = [qq|#pragma trillium vector begin|]
        reflected_for = intermingle line [qq|while($blackhole) \{|]
        blackhole = "bh"

split (Vissue block) accum_ast (line:rest)
  | line =~ end_vissue_pragma_regex 
      = split KernelBody (insert_block 0 block accum_ast) rest
  | otherwise
      = split (Vissue (block +++ line)) accum_ast rest
  where end_vissue_pragma_regex = [qq|#pragma trillium vector end|] 
        insert_block key bb prog =
          accum_ast {vissue_blocks= M.insert key bb $ vissue_blocks prog}

split _ _ [] = error "reached end of file unexpectedly"

(+++) :: T.Text -> T.Text -> T.Text
t1 +++ t2 = T.intercalate "" [t1, t2]

alpha = "a-z|A-Z"
num = "0-9"
alpha_num = [text|$alpha|$num|]
iden = [text|[$alpha]+|]
decompose :: T.Text -> T.Text -> [T.Text]
decompose line regex =  
  let (_,_,_,matches) = line =~ regex :: (T.Text,T.Text,T.Text,[T.Text])
  in matches
