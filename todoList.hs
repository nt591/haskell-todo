import Data.List
import System.IO
import Control.Monad
import System.Directory

todoFileName :: String
todoFileName = "todo.txt"

openTodoListFile :: String -> IO Handle
openTodoListFile fileName = openFile fileName ReadMode

view :: String -> IO ()
view fileName = do
  handle <- openTodoListFile fileName
  todoFileContents <- hGetContents handle
  let presentationalContents = zipWith (\ix line -> (show ix) ++ " - " ++ line) [1..] (lines todoFileContents)
  putStrLn "Your todos are:"
  putStr $ unlines presentationalContents
  hClose handle

dispatch :: String -> String -> IO ()
dispatch "add"    = add
dispatch "view"   = viewFile
dispatch "delete" = deleteItem
dispatch "help"   = outputHelp
dispatch cmd      = unmatchedDispatch cmd

outputHelp :: String -> IO ()
outputHelp _ = do
  putStrLn "Commands are:"
  putStrLn "ADD [item]: adds an item to the todo list"
  putStrLn "VIEW: reads out all the items on the list"
  putStrLn "DELETE [index]: deletes a todo list item by index"
  putStrLn "HELP: Outputs all commands"
  putStrLn "CMD + C to exit"

viewFile :: String -> IO ()
viewFile _ = view "todo.txt"

unmatchedDispatch :: String -> String -> IO ()
unmatchedDispatch cmd _ = putStrLn $ "Unknown command: " ++ cmd ++ " is not a valid command."

add :: String -> IO ()
add todoItem = appendFile todoFileName $ todoItem ++ "\n"

deleteItem :: String -> IO ()
deleteItem idx = do
  -- read file, load in elements, delete index, copy to temp, rename temp to original
  handle <- openTodoListFile todoFileName
  contents <- hGetContents handle

  let todoFileContents = lines contents
      number = read idx
      -- Why doesn't this do what I think it does
      -- newContents = delete (number - 1) todoFileContents
      newContents = unlines $ (take (number - 1) todoFileContents) ++ (drop number todoFileContents)

  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle newContents
  hClose handle
  hClose tempHandle
  removeFile todoFileName
  renameFile tempName todoFileName

main = do
  putStrLn "Commands are:"
  putStrLn "ADD [item]: adds an item to the todo list"
  putStrLn "VIEW: reads out all the items on the list"
  putStrLn "DELETE [index]: deletes a todo list item by index"
  putStrLn "HELP: Outputs all commands"
  putStrLn "CMD + C to exit"
  view todoFileName
  forever $ do
    str <- getLine
    let command = head . words $ str
        args    = unwords . tail . words $ str
    dispatch command args
    putStrLn ""