module IOPrimitives (ioPrimitives) where
import System.IO
import Datatypes
import Eval (apply, load)
import Control.Monad (liftM)
import Parser (parseScheme)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProcAndParse),
                ("read-line", readProc),
                ("print", writeProcPrint),
                ("write", writeProc),
                ("write-line", writeProcLine),
                ("read-contents", readContents),
                ("read-all", readAll)]

-- Helper functions
-- Many just shuffle monads around on top of Haskell IO functions

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ String `liftM` hGetLine port) >>= return

readProcAndParse :: [LispVal] -> IOThrowsError LispVal
readProcAndParse x = readProc x >>= \(String x) -> liftThrows (parseScheme x)

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [String obj, Port port] = do
    liftIO $ hPutStr port obj
    return $ Bool True

writeProcLine :: [LispVal] -> IOThrowsError LispVal
writeProcLine (String x:xs) = writeProc ((String $ x ++ "\n") : xs)

writeProcPrint :: [LispVal] -> IOThrowsError LispVal
writeProcPrint (x:xs) = writeProc ((String $ show x) : xs)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
