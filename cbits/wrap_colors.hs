import Language.C
import Language.C.Analysis
import Language.C.Analysis.Export
import Language.C.Data.Ident
import Language.C.System.GCC (newGCC)
import System.Environment (getArgs,getProgName)
import System.FilePath
import System.IO(hPutStrLn,stderr)
import qualified Data.Map as Map
import Text.PrettyPrint hiding (float)
import Control.Monad(when)
import qualified Data.Set as Set
import Data.Set (Set)


files = [ "allegro5.h"
        , "allegro_font.h"
        , "allegro_ttf.h"
        , "allegro_primitives.h"
        ]

colorStruct :: String
colorStruct = "ALLEGRO_COLOR"

fields :: [String]
fields = ["r","g","b","a"]

data Done = Done { names :: Set Ident, decls :: [CFunDef] }

main :: IO ()
main = do as <- getArgs
          case as of
            [d] -> go [ d </> "allegro5" </> f | f <- files ]
                      Done { names = Set.empty, decls = [] }
            _ -> do p <- getProgName
                    hPutStrLn stderr $ "Usage: " ++ p ++ " INCLUDE_DIR"
  where
  addInclude f     = "#include <allegro5/" ++ f ++ ">"

  go [] done       = do putStrLn $ unlines $ map addInclude files
                        mapM_ (print . pretty) (decls done)
  go (f : fs) done = go fs =<< processFile f done

processFile :: FilePath -> Done -> IO Done
processFile file sofar =
  do res <- parseCFile (newGCC "gcc") Nothing [] file
     case res of
       Left err -> printErr err >> return sofar
       Right tu ->
         case runTrav_ (analyseAST tu) of
           Left errs    -> mapM_ printErr errs >> return sofar
           Right (ds,_) -> return (processDecls ds sofar)
  where
  printErr msg = hPutStrLn stderr (file ++ " " ++ show msg)


processDecls :: GlobalDecls -> Done -> Done
processDecls ds sofar =
  go sofar [ getVarDecl d | Declaration d <- Map.elems (gObjs ds) ]
  where
  go d []       = d
  go d (x : xs) = case processDecl x d of
                    Nothing -> go d xs
                    Just d1 -> go d1 xs

processDecl :: VarDecl -> Done -> Maybe Done
processDecl (VarDecl f _ (FunctionType (FunType res args False) _)) done
  | fi `Set.notMember` names done && (isColor res || any (isColor . snd) ps)
    = Just Done { names = Set.insert fi (names done)
                , decls = newFun : decls done }
  where
  ps = [ (identOfVarName x,tp) | VarDecl x _ tp <- map getVarDecl args ]
  (ps1,ds1,ss1)      = unzip3 $ map expandParam ps
  (ps2,ds2,ss2,res') = expandResult res
                     $ call (identOfVarName f) (map (var.fst) ps)

  fi = identOfVarName f
  f' = newName (\x -> "sh" ++ x) fi
  newFun = mkFun f' (concat ps1 ++ ps2) res'
                    (concat ds1 ++ ds2)
                    (concat ss1 ++ ss2)


processDecl _ _ = Nothing

newName :: (String -> String) -> Ident -> Ident
newName f (Ident s _ _) = ident (f s)

type D = (Ident, Type)



expandParam :: D -> ([D],[D],[CStat])
expandParam d@(x,t)
  | isColor t     = (params,[d],stats)
    where
    rename a  = newName (\s -> s ++ "_" ++ a) x
    params    = [ (rename f, float) | f <- fields ]
    stats     = [ assign (field (var x) f) (var (rename f)) | f <- fields ]

expandParam p     = ([p], [], [])


expandResult :: Type -> CExpr -> ([D], [D], [CStat], Type)
expandResult t e
  | isColor t = (params,decls,stats, void)
    where
    name    = "out"
    res     = ident name
    param f = ident (name ++ "_" ++ f)
    params  = [ (param f, ptr float) | f <- fields ]
    decls   = [ (res, namedType (ident colorStruct)) ]
    stats   = assign (var res) e
            : [ assign (deref (var (param f)))
                       (field (var res) f) | f <- fields ]


expandResult t e = ([], [], [ret e], t)


float :: Type
float = DirectType (TyFloating TyFloat) noTypeQuals []

namedType :: Ident -> Type
namedType x = TypeDefType (TypeDefRef x Nothing undefined) noTypeQuals []

ptr :: Type -> Type
ptr t = PtrType t noTypeQuals []

void :: Type
void = DirectType TyVoid noTypeQuals []

var :: Ident -> CExpr
var x = CVar x undefined

deref :: CExpr -> CExpr
deref e = CUnary CIndOp e undefined

assign :: CExpr -> CExpr -> CStat
assign e1 e2 = CExpr (Just (CAssign CAssignOp e1 e2 undefined)) undefined

ret :: CExpr -> CStat
ret e = CReturn (Just e) undefined

call :: Ident -> [CExpr] -> CExpr
call f xs = CCall (var f) xs undefined

field :: CExpr -> String -> CExpr
field e f = CMember e (ident f) False undefined

ident :: String -> Ident
ident x = Ident x 0 undefined

mkDecl :: (Ident,Type) -> CDecl
mkDecl (x,t) = CDecl spec [(Just d, Nothing, Nothing)] (nodeInfo d)
  where (spec,d) = exportDeclr [] t [] (VarName x Nothing)

mkFun :: Ident -> [D] -> Type -> [D] -> [CStat] -> CFunDef
mkFun f ps res ds ss = CFunDef spec declr [] (CCompound [] body i) i
  where
  (spec,der)  = exportType res
  pds         = map mkDecl ps
  fd          = CFunDeclr (Right (pds, False)) [] i
  declr       = CDeclr (Just f) (fd:der) Nothing [] i
  body        = map (CBlockDecl . mkDecl) ds ++ map CBlockStmt ss
  i           = undefined



isColor :: Type -> Bool
isColor ty = show (pretty ty) == colorStruct







