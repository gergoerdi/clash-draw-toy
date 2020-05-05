import Clash.Clashilator.Setup (clashilate)

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription

import Data.Maybe

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { buildHook = myBuildHook
    }

myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook pkg localInfo userHooks buildFlags = do
    let flags = configConfigurationsFlags . configFlags $ localInfo
        verilatorFlag = fromMaybe False $ lookupFlagAssignment (mkFlagName "verilator") flags

    pkg <- if verilatorFlag then clashilate pkg localInfo buildFlags "DrawToy" else return pkg
    buildHook simpleUserHooks pkg localInfo userHooks buildFlags
