{- LambdaToolbox - Version and Copyright information below -}

import Toolbox 
import Types
import System.IO
import System.Info

license = "LambdaToolbox v1.0.1"
    ++ "\n"
    ++ "    by Julian Fleischer <julian dot fleischer at fu-berlin dot de>\n"
    ++ "\n"
    ++ "    A tiny toolbox for working with lambda expressions.\n"
    ++ "    Includes alpha and beta reduction, reducing to normal form,\n"
    ++ "    identifying free variables, recognizing and expanding macros.\n"
    ++ "\n"
    ++ "    Redistribution and use in source and binary forms,\n"
    ++ "    with or without modification, are permitted provided that\n"
    ++ "    the following conditions are met:\n"
    ++ "\n"
    ++ "    1. Redistributions of source code must retain the above\n"
    ++ "       copyright notice, this list of conditions and the\n"
    ++ "       following disclaimer.\n"
    ++ "\n"
    ++ "    2. Redistributions in binary form must reproduce the above\n"
    ++ "       copyright notice, this list of conditions and the following\n"
    ++ "       disclaimer in the documentation and/or other materials\n"
    ++ "       provided with the distribution.\n"
    ++ "\n"
    ++ "    THIS SOFTWARE IS PROVIDED \"AS IS\" AND ANY EXPRESS OR IMPLIED\n"
    ++ "    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\n"
    ++ "    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\n"
    ++ "    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR\n"
    ++ "    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,\n"
    ++ "    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT\n"
    ++ "    NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\n"
    ++ "    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)\n"
    ++ "    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"
    ++ "    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR\n"
    ++ "    OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,\n"
    ++ "    EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"



showMacros = unlines [n ++ " ≡ " ++ m ++ "\n  " ++ d | (n, m, d) <- macros]

showMacroList [] = "No macros defined."
showMacroList m  = "\n" ++ unlines [n ++ " ≡ " ++ show m | (n, m) <- m]

showHelp = "\n"
        ++ " Type something. Anything not a command\n"
        ++ " will be interpreted as λ-expression.\n"
        ++ "\n"
        ++ " Type “help <command>” for further details, like “help macros”.\n"
        ++ "\n"
        ++ " The commands are:\n"
        ++ "   a  alpha    Performs alpha conversion on the current λ-expression.\n"
        ++ "   b  beta     Performs beta reduction on the current λ-expression.\n"
        ++ "   e  eta      Performs eta reduction on the current λ-expression.\n"
        ++ "   .  trace    Trace complete reduction to normal form.\n"
        ++ "   ,  eval     Expand macros and trace, also only show substitued λ-expr.\n"
        ++ "   x  expand   Expands macros in the current λ-expression.\n"
        ++ "   s  subst    Identifies and substitutes known macros.\n"
        ++ "   f  free     Show free variables in the current λ-expression.\n"
        ++ "   ff allfree  Show *all* free variables, including sub-expressions.\n"
        ++ "   r  readable Shows a readable representation of the current expression.\n"
        ++ "   d  debug    Shows an accurate representation of the internal store.\n"
        ++ "   m  macros   Shows a list of all built-in macros.\n"
        ++ "   cc clear    Clears the list of macros.\n"
        ++ "   h  help     Show this help text.\n"
        ++ "   l  license  Show copyright / version information.\n"
        ++ "   q  exit     Exits the interpreter.\n"
        ++ "\n"
        ++ " let X = <macro>\n"
        ++ "   defines the macro with the name X.\n"
        ++ "\n"
        ++ " λ-functions can be declared using λ,\n"
        ++ " forward- or backward slash (/, \\),\n"
        ++ " or ^, like so: λx.x, /x.x, \\x.x, ^x.x\n"

aliasHelp short long = " “" ++ short ++ "” is the short alias for “"
                    ++ long ++ "”:\n" ++ detailedHelp long

detailedHelp "m"      = aliasHelp "m" "macros"
detailedHelp "macros" = showMacros ++ "\n"
                     ++ " This is a list of built-in macros. The list of current macros\n"
                     ++ " can be obtained using “macros”.\n"
                     ++ " You can define new macros using “let X = ...”\n"
detailedHelp "e"      = aliasHelp "e" "eta"
detailedHelp "eta"    = " Performs a top-level eta reduction on the current expression.\n"
                     ++ " Eta reduction is only applied on top level expressions.\n"
detailedHelp "b"      = aliasHelp "b" "beta"
detailedHelp "beta"   = " Performs beta reduction on the current expression, if possible.\n"
                     ++ " Reduction is performed in normal order, that is, the left-most\n"
                     ++ " expression is evaluated first.\n"
detailedHelp "a"      = aliasHelp "a" "alpha"
detailedHelp "alpha"  = " Performs alpha reduction on the current string.\n"
detailedHelp "."      = aliasHelp "." "solve"
detailedHelp "solve"  = " Solves the current expression by applying beta reduction until\n"
                     ++ " no further reduction is possible. If necessary, alpha conversion\n"
                     ++ " is applied. This might result in a non-terminating computation!\n"
detailedHelp "x"      = aliasHelp "x" "expand"
detailedHelp "expand" = " Expands the macros in the current expression.\n"
                     ++ " For example, this will expand all T in “pt” into\n"
                     ++ " their definition: pT -> p(λxy.x)\n"

detailedHelp cmd      = " No detailed help available for the command “" ++ cmd ++ "”.\n"

data Action = Message String | NewState Lambda | NewMacros [(String, Lambda)] | Exit

basicAction macros ('l':'e':'t':' ':name:' ':'=':' ':def)
    | not $ isSpace name = Just $ NewMacros (([name], read def) : macros)
basicAction macros cmd
    | "l" >< "license" = Just $ Message license
    | "h" >< "help"   = Just $ Message $ showHelp
    | "m" >< "macros" = Just $ Message $ showMacroList macros
    | "cc" >< "clear" = Just $ NewMacros []
    | take 5 cmd == "help " = Just $ Message ('\n' : (detailedHelp $ drop 5 cmd))
    | take 2 cmd == "h "    = Just $ Message ('\n' : (detailedHelp $ drop 2 cmd))
    | cmd `elem` ["q", "quit", "exit", ":q"] = Just Exit
    | otherwise       = Nothing
        where a >< b = cmd == a || cmd == b

adv = [ "a", "alpha", "b", "beta", "e", "eta", "x", "expand", "s", "subst",
        "f", "free", "ff", "allfree", "d", "debug", ".", "trace", ",", "eval",
        "r", "readable", "" ]

isWindows = take 5 os == "mingw"

sanitize x
    | isWindows = maskUnicode x
    | otherwise = x
        where
            maskUnicode ('λ':xs) = '\\' : maskUnicode xs
            maskUnicode ('“':xs) = '"'  : maskUnicode xs
            maskUnicode ('”':xs) = '"'  : maskUnicode xs
            maskUnicode ('≡':xs) = '='  : maskUnicode xs
            maskUnicode ('α':xs) = 'a'  : maskUnicode xs
            maskUnicode ('β':xs) = 'b'  : maskUnicode xs
            maskUnicode ('η':xs) = 'n'  : maskUnicode xs
            maskUnicode ('∧':xs) = '^'  : maskUnicode xs
            maskUnicode ('∨':xs) = 'v'  : maskUnicode xs
            maskUnicode (x:xs)   = x    : maskUnicode xs
            maskUnicode []       = []

repl :: [(String, Lambda)] -> Maybe Lambda -> IO ()
repl macros state = do
    line <- getLine
    hSetEncoding stdout (if isWindows then latin1 else utf8)
    let putStrLn x = hPutStrLn stdout $ sanitize x
    let res = case basicAction macros line of
                Just action -> action
                Nothing -> case state of
                    Nothing -> if line `elem` adv
                        then Message  "No lambda expression loaded!"
                        else NewState $ read line
                    Just l  -> eval macros line l
    case res of
        Message message -> putStrLn message       >> repl macros state
        NewState state' -> putStrLn (show state') >> repl macros (Just state')
        NewMacros []    -> putStrLn "Macros cleared." >> repl [] state
        NewMacros m'    -> putStrLn "Macros changed." >> repl m' state
        Exit            -> putStrLn "Goodbye."

eval :: [(String, Lambda)] -> String -> Lambda -> Trace Action
eval _      ""  expr  = Message $ show expr
eval macros cmd expr
    | "a" >< "alpha"  = NewState $ alpha expr
    | "b" >< "beta"   = let (result, expr') = beta expr in
        case result of
            Reduced              -> NewState expr'
            Impossible           -> Message "No further reduction possible!"
            NeedsAlphaConversion -> Message "Needs alpha conversion first!"
    | "e" >< "eta"    = let (result, expr') = eta expr in
        if result then NewState expr' else Message "No top-level eta reduction applicable!"
    | "." >< "trace"  = Message  $ unlines (("     " ++ show expr) : (showTrace show $ trace expr))
    | "," >< "eval"   = let show' = show . subst macros in
            Message  $ unlines (("     " ++ show' expr) : (showTrace show' $ trace $ expand macros expr))
    | "x" >< "expand" = NewState $ expand macros expr
    | "s" >< "subst"  = NewState $ subst macros expr
    | "f" >< "free"   = let freeVars = free expr in
        if freeVars == []
            then Message "No free variables!"
            else Message $ "Free variables: " ++ formatFree freeVars
    | "ff" >< "allfree" = let freeVars = allfree expr in
        if freeVars == []
            then Message "No free variables!"
            else Message $ '\n' : (unlines $ map (\(l, f) -> formatFree f ++ " in " ++ show l) freeVars)
    | "r" >< "readable" = Message $ show $ subst macros $ expr
    | "d" >< "debug"  = Message  $ debug expr
    | otherwise       = NewState $ read cmd
        where a >< b = cmd == a || cmd == b
              formatFree free = foldl1 (\l r -> l ++ ", " ++ r) free

main = do
    putStrLn "Welcome to lambda calculus! Type \"help\" for help."
    repl macros Nothing
