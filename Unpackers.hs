{-# LANGUAGE ExistentialQuantification #-}
module Unpackers where

import Control.Monad.Error
import Datatypes

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return (show s)
unpackStr (Bool s) = return (show s)
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = case reads n of
    [(n, _)] -> return n
    _ -> throwError $ TypeMismatch "number" $ String n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

allUnpackers = [ AnyUnpacker unpackNum
               , AnyUnpacker unpackStr
               , AnyUnpacker unpackBool ]
