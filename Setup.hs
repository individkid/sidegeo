import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

fluidPP :: PreProcessor
fluidPP = PreProcessor {
 platformIndependent = True,
 runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
   putStrLn ("preprocess " ++ inFile ++ " to " ++ outFile)
   stuff <- readFile inFile
   writeFile outFile ("# preprocessed as a test\n\n" ++ stuff)}

fluidPPHandler :: BuildInfo -> LocalBuildInfo -> PreProcessor
fluidPPHandler bi lbi = fluidPP

main = defaultMainWithHooks simpleUserHooks {
 hookedPreProcessors = [(".fl",fluidPPHandler)]}
