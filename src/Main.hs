module Main where
import Data.List
import System.Environment (getArgs)
import Control.Monad (when)

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
   getInfo <$> readFile f


data ModuleInfo = ModuleInfo String [String] deriving Show

renderGraphvizLine ::ModuleInfo -> String
renderGraphvizLine (ModuleInfo modName dependsOnModules) =
  show modName ++ "-> {" ++ intercalate "," (map show dependsOnModules) ++ "}"

getInfo :: String -> ModuleInfo
getInfo elmSource =
  let ls = lines elmSource
  in ModuleInfo (getModuleName ls) (getImportedModuleNames ls)

getModuleName :: [String] -> String
getModuleName sourceFileLines =
  case filter (\line -> "module" `isPrefixOf` line) sourceFileLines of
    [] -> error "Failed to find module declaration"
    (moduleLine:_) -> extractSecondWord moduleLine

extractSecondWord :: String -> String
extractSecondWord str = case words str of
   (_: secondWord:_) -> secondWord
   _ -> error $ "String " ++ str ++ " doesn't have second word"

getImportedModuleNames :: [String] -> [String]
getImportedModuleNames sourceFileLines =
  map extractSecondWord $ filter (\line -> "import" `isPrefixOf` line) sourceFileLines
