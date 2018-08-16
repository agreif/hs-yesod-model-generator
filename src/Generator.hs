{-# LANGUAGE OverloadedStrings #-}
module Generator where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Casing as TC
import Data.Aeson
import qualified Data.Maybe as M
import qualified Data.List as L
import Data.Semigroup ((<>))
import qualified Text.Ginger as G
import qualified System.IO as SIO (IOMode(ReadMode), hSetEncoding, withFile, utf8_bom)
import qualified System.IO.Strict as SIOS
import qualified System.IO.Error as SIOE
import qualified Options.Applicative as O
import qualified GHC.IO.Encoding as E

generate :: Value -> IO ()
generate context = do
  E.setLocaleEncoding E.utf8
  templFile <- O.execParser argInfo
  template <- loadTemplate templFile
  putStrLn . Text.unpack $ G.easyRender context template

-- command line parser

argParser :: O.Parser String
argParser = O.argument O.str (O.metavar "TEMPLATE" <> O.help "Ginger template file")

argInfo :: O.ParserInfo String
argInfo = O.info (argParser O.<**> O.helper)
  ( O.fullDesc
  <> O.progDesc "Generate code from TEMPLATE"
  <> O.header "hs-generator - yesod partioal code generator" )

-- ginger

loadTemplate :: String -> IO (G.Template G.SourcePos)
loadTemplate templFile = do
  eitherTemplate <- G.parseGingerFile' opts templFile
  return $ case eitherTemplate of
             Left err -> error . show $ err
             Right template' -> template'

opts :: G.ParserOptions IO
opts = (G.mkParserOptions fileResolver) { G.poSourceName = Nothing
                                        , G.poKeepTrailingNewline = True }

fileResolver :: G.IncludeResolver IO
fileResolver filename = do
  content <- loadFile filename
  return $ Just content

loadFile :: FilePath -> IO String
loadFile fn =
  SIOE.tryIOError (loadFile' $ "ginger/" ++ fn) >>= \e ->
    case e of
      Right contents -> return contents
      Left err -> return $ show err
  where
    loadFile' :: FilePath -> IO String
    loadFile' fn' = do
      SIO.withFile fn' SIO.ReadMode $ \h -> do
        SIO.hSetEncoding h SIO.utf8_bom
        contents <- SIOS.hGetContents h
        return contents

-- helpers

upperFirst :: Text -> Text
upperFirst t = Text.append (Text.toUpper $ Text.take 1 t) (Text.drop 1 t)

lowerFirst :: Text -> Text
lowerFirst t = Text.append (Text.toLower $ Text.take 1 t) (Text.drop 1 t)

-- types

data BContext = BContext
  { bContextModels :: [BModel]
  , bContextTranslations :: [BTranslation]
  }

instance ToJSON BContext where
  toJSON o = object $
    [ "models" .= bContextModels o
    , "translations" .= bContextTranslations o
    ] ++ (map (\bModel@(BModel {bModelName = modelName}) -> (modelName <> "Model") .= bModel) $ bContextModels o)

data BModel = BModel
  { bModelName :: Text
  , bModelLabel :: Text
  , bModelIsJson :: Bool
  , bModelDbUniquenesses :: [Text]
  , bModelDbHasHistoryTable :: Bool
  , bModelHsDerivings :: [Text]
  , bModelFields :: [BField]
  , bModelAddFormEntityLoader :: Maybe Text
  , bModelEditFormEntityLoader :: Maybe Text
  , bModelDeleteFormEntityLoader :: Maybe Text
  , bModelAddFormDataJsonUrl :: Maybe Text
  , bModelEditFormDataJsonUrl :: Maybe Text
  , bModelDeleteFormDataJsonUrl :: Maybe Text
  , bModelAddFormHasDefaultModel :: Bool
  , bModelEditPostLoadsModel :: Bool
  , bModelDeletePostLoadsModel :: Bool
  , bModelAddFormTitleMsg :: Maybe Text
  , bModelEditFormTitleMsg :: Maybe Text
  , bModelDeleteFormTitleMsg :: Maybe Text
  , bModelParentHsType :: Maybe Text
  , bModelFormRouteHsType :: Text
  }

instance ToJSON BModel where
  toJSON o = object
    [ "name" .= bModelName o
    , "nameCap" .= (upperFirst $ bModelName o)
    , "label" .= bModelLabel o
    , "isJson" .= bModelIsJson o
    , "dbUniquenesses" .= bModelDbUniquenesses o
    , "dbHasHistoryTable" .= bModelDbHasHistoryTable o
    , "dbTableName" .= (TC.toQuietSnake $ TC.fromAny (Text.unpack $ bModelName o))
    , "dbHistoryTableName" .= ((TC.toQuietSnake $ TC.fromAny (Text.unpack $ bModelName o)) ++ "_history")
    , "dbFields" .= getDbFields o
    , "dbUpdatableFields" .= (filter (\field -> case bFieldDb field of
                                                  Just BFieldDb {bFieldDbCanUpdate = canUpdate} -> canUpdate && (M.isJust $ bFieldEditView field)
                                                  Nothing -> False
                                     ) $ bModelFields o)
    , "hsAddAssignmentLines" .= getAddAssignmentLines o
    , "hsDerivings" .= bModelHsDerivings o
    , "fields" .= bModelFields o
    , "addViewFields" .= (filter (\field -> M.isJust $ bFieldAddView field) $ bModelFields o)
    , "editViewFields" .= (filter (\field -> M.isJust $ bFieldEditView field) $ bModelFields o)
    , "isInDb" .= (L.any M.isJust $ L.map bFieldDb $ bModelFields o)
    , "addFormEntityLoader" .= bModelAddFormEntityLoader o
    , "editFormEntityLoader" .= bModelEditFormEntityLoader o
    , "deleteFormEntityLoader" .= bModelDeleteFormEntityLoader o
    , "addFormDataJsonUrl" .= bModelAddFormDataJsonUrl o
    , "editFormDataJsonUrl" .= bModelEditFormDataJsonUrl o
    , "deleteFormDataJsonUrl" .= bModelDeleteFormDataJsonUrl o
    , "addFormHasDefaultModel" .= bModelAddFormHasDefaultModel o
    , "editPostLoadsModel" .= bModelEditPostLoadsModel o
    , "deletePostLoadsModel" .= bModelDeletePostLoadsModel o
    , "addFormTitleMsg" .= bModelAddFormTitleMsg o
    , "editFormTitleMsg" .= bModelEditFormTitleMsg o
    , "deleteFormTitleMsg" .= bModelDeleteFormTitleMsg o
    , "parentHsType" .= bModelParentHsType o
    , "formRouteHsType" .= bModelFormRouteHsType o
    , "parentHsParamId" .= getParentHsParamId o
    , "formHasProgressBar" .= (any (\field -> bFieldHsType field == "FileInfo") $ bModelFields o)
    ]

getDbFields :: BModel -> [BField]
getDbFields m = filter (M.isJust . bFieldDb) $ bModelFields m

getAddAssignmentLines :: BModel -> [Text]
getAddAssignmentLines m =
  (if M.isJust $ bModelParentHsType m then [ Text.concat [bModelName m, M.fromJust $ bModelParentHsType m, "Id", " = ", getParentHsParamId m]] else [])
  ++
  ( map (\f -> Text.concat [bModelName m, upperFirst $ bFieldName f, " = ",
                            case bFieldAddView f of
                              Just _ -> Text.concat ["vAdd", upperFirst $ bModelName m, upperFirst $ bFieldName f, " vAdd", upperFirst $ bModelName m]
                              _ -> "Nothing"
                           ]
        )
    $ filter (\f -> case bModelParentHsType m of
                 Just hsType -> Text.concat [hsType, "Id"] /= bFieldHsType f
                 _ -> True
             )
    $ getDbFields m
  )


getParentHsParamId :: BModel -> Text
getParentHsParamId m = case bModelParentHsType m of
                         Just parentHsType -> lowerFirst $ Text.append parentHsType "Id"
                         _ -> ""


data BTranslation = BTranslation
  { bTranslationKey :: Text
  , bTranslationDe :: Text
  , bTranslationEn :: Text
  }

instance ToJSON BTranslation where
  toJSON o = object
    [ "key" .= bTranslationKey o
    , "keyCap" .= (upperFirst $ bTranslationKey o)
    , "de" .= bTranslationDe o
    , "en" .= bTranslationEn o
    ]


data BFieldDb = BFieldDb
  { bFieldDbIsNullable :: Bool
  , bFieldDbDefault :: Maybe Text
  , bFieldDbCanUpdate :: Bool
  }

instance ToJSON BFieldDb where
  toJSON o = object
    [ "isNullable" .= bFieldDbIsNullable o
    , "isNotNullable" .= (not $ bFieldDbIsNullable o)
    , "default" .= bFieldDbDefault o
    , "canUpdate" .= bFieldDbCanUpdate o
    ]

data BFieldAddView = BFieldAddView
  { bFieldAddViewIsRequired :: Bool
  , bFieldAddViewIsDisabled :: Bool
  , bFieldAddViewAttrs :: [BFieldAttr]
  , bFieldAddViewDefault :: Maybe Text
  }

instance ToJSON BFieldAddView where
  toJSON o = object
    [ "isRequired" .= bFieldAddViewIsRequired o
    , "isOptional" .= (not $ bFieldAddViewIsRequired o)
    , "isDisabled" .= bFieldAddViewIsDisabled o
    , "isEnabled" .= (not $ bFieldAddViewIsDisabled o)
    , "attrs" .= ((if bFieldAddViewIsDisabled o then [BFieldAttr "disabled" ""] else [])
                   ++ bFieldAddViewAttrs o)
    , "default" .= bFieldAddViewDefault o
    ]

data BFieldEditView = BFieldEditView
  { bFieldEditViewIsRequired :: Bool
  , bFieldEditViewIsDisabled :: Bool
  , bFieldEditViewAttrs :: [BFieldAttr]
  , bFieldEditViewDefault :: Maybe Text
  }

instance ToJSON BFieldEditView where
  toJSON o = object
    [ "isRequired" .= bFieldEditViewIsRequired o
    , "isOptional" .= (not $ bFieldEditViewIsRequired o)
    , "isDisabled" .= bFieldEditViewIsDisabled o
    , "isEnabled" .= (not $ bFieldEditViewIsDisabled o)
    , "attrs" .= ((if bFieldEditViewIsDisabled o then [BFieldAttr "disabled" ""] else [])
                   ++ bFieldEditViewAttrs o)
    , "default" .= bFieldEditViewDefault o
    ]

data BField = BField
  { bFieldName :: Text
  , bFieldLabelDe :: Maybe Text
  , bFieldLabelEn :: Maybe Text
  , bFieldHsType :: Text
  , bFieldDb :: Maybe BFieldDb
  , bFieldFormFieldType :: Maybe Text
  , bFieldAddView :: Maybe BFieldAddView
  , bFieldEditView :: Maybe BFieldEditView
  }

instance ToJSON BField where
  toJSON o = object
    [ "name" .= bFieldName o
    , "nameCap" .= (upperFirst $ bFieldName o)
    , "dbColumnName" .= (TC.toQuietSnake $ TC.fromAny (Text.unpack $ bFieldName o))
    , "labelDe" .= bFieldLabelDe o
    , "labelEn" .= bFieldLabelEn o
    , "hsType" .= bFieldHsType o
    , "db" .= bFieldDb o
    , "formFieldType" .= bFieldFormFieldType o
    , "addView" .= bFieldAddView o
    , "editView" .= bFieldEditView o
    , "isHsTypeBool" .= (bFieldHsType o == "Bool")
    , "isForeignKey" .= ((Text.takeEnd 2 $ bFieldName o) == "Id")
    ]

data BFieldAttr = BFieldAttr
  { bFieldAttrKey :: Text
  , bFieldAttrValue :: Text
  }

instance ToJSON BFieldAttr where
  toJSON o = object
    [ "key" .= bFieldAttrKey o
    , "value" .= bFieldAttrValue o
    ]
