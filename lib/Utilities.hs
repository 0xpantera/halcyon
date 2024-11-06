{-# LANGUAGE OverloadedStrings #-}
module Utilities where

import Filepath
import qualified Text as T




--(* main driver *)

--let compile stage optimizations preprocessed_src =
--  let _ = Compile.compile stage optimizations preprocessed_src in
--  (* remove preprocessed src *)
--  run_command "rm" [ preprocessed_src ];
--  replace_extension preprocessed_src ".s"

--let driver target debug libs stage optimizations src =
--  let _ =
--    Settings.platform := target;
--    Settings.debug := debug
--  in
--  let preprocessed_name = preprocess src in
--  let assembly_name = compile stage optimizations preprocessed_name in
--  match stage with
--  | Settings.Executable ->
--      assemble_and_link ~link:true ~cleanup:(not debug) ~libs assembly_name
--  | Settings.Obj ->
--      assemble_and_link ~link:false ~cleanup:(not debug) assembly_name
--  | _ -> ()
