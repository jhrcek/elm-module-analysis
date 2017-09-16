module Main where
import Control.Monad (when)
import Data.List
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ error "Expecting one file containing list of elm files as argument"
  elmFiles <- fmap lines $ readFile $ head args
  moduleInfos <- getModuleInfos True elmFiles
  let dotSource = renderModuleDependenciesDotFile moduleInfos
  writeFile "dependencies.dot" dotSource

renderModuleDependenciesDotFile :: [ModuleInfo] ->  String
renderModuleDependenciesDotFile mis =
  unlines $ "digraph ModuleDependencies {\nrankdir=LR" : map renderGraphvizLine mis ++ ["}"]

getModuleInfos :: Bool -> [FilePath] -> IO [ModuleInfo]
getModuleInfos internalOnly elmFiles = do
  mis <- mapM getModuleInfo elmFiles
  return $ if internalOnly
    then removeExternalModules mis
    else mis

removeExternalModules :: [ModuleInfo] -> [ModuleInfo]
removeExternalModules mis =
  let internalModules = map (\(ModuleInfo m _) -> m) mis
  in map (\(ModuleInfo m deps) -> ModuleInfo m (filter (`elem` internalModules) deps)) mis

getModuleInfo :: FilePath -> IO ModuleInfo
getModuleInfo f =
   getInfo f <$> readFile f


data ModuleInfo = ModuleInfo String [String] deriving Show

renderGraphvizLine ::ModuleInfo -> String
renderGraphvizLine (ModuleInfo modName dependsOnModules) =
  show modName ++ "-> {" ++ intercalate "," (map show dependsOnModules) ++ "}"

getInfo :: FilePath -> String -> ModuleInfo
getInfo file elmSource =
  let ls = lines elmSource
  in ModuleInfo (getModuleName file ls) (getImportedModuleNames ls)

getModuleName :: FilePath -> [String] -> String
getModuleName file sourceFileLines =
  case filter (\line -> "module" `isPrefixOf` line || "port module" `isPrefixOf` line) sourceFileLines of
    []             -> error $ "Failed to find module declaration in " ++ file
    (moduleLine:_) -> extractModuleName moduleLine

getImportedModuleNames :: [String] -> [String]
getImportedModuleNames sourceFileLines =
  map extractModuleName $ filter (\line -> "import" `isPrefixOf` line) sourceFileLines

extractModuleName :: String -> String
extractModuleName str = case words str of
  ("module":name:_) -> name
  ("port":"module":name:_) -> name
  ("import":name:_) -> name
  _ -> error $ "Line '" ++ str ++ "' doesn't seem to have module name"
