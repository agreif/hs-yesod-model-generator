{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Generator where

import Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.UTF8 as UTF8
import Data.FileEmbed
import qualified Data.List as L
import qualified Data.Maybe as M
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GHC.IO.Encoding as E
import qualified Options.Applicative as O
import qualified System.IO as SIO (IOMode (ReadMode), hSetEncoding, utf8_bom, withFile)
import qualified System.IO.Error as SIOE
import qualified System.IO.Strict as SIOS
import qualified Text.Casing as TC
import qualified Text.Ginger as G

generate :: Value -> IO ()
generate context = do
  E.setLocaleEncoding E.utf8
  args <- O.execParser argInfo
  let (templFile, maybeModelName) = case args of
        [a1] -> (a1, Nothing)
        [a1, a2] -> (a1, Just a2)
  template <- loadTemplate templFile maybeModelName
  putStrLn . Text.unpack $ G.easyRender context template

-- command line parser

argParser :: O.Parser [String]
argParser = O.many (O.argument O.str O.showDefault)

argInfo :: O.ParserInfo [String]
argInfo =
  O.info
    (argParser O.<**> O.helper)
    ( O.fullDesc
        <> O.progDesc "Generate code from TEMPLATE"
        <> O.header "hs-generator - yesod partial code generator"
    )

-- ginger

loadTemplate :: String -> Maybe String -> IO (G.Template G.SourcePos)
loadTemplate templFile maybeModelName = do
  eitherTemplate <- G.parseGingerFile' (opts maybeModelName) templFile
  return $ case eitherTemplate of
    Left err -> error . show $ err
    Right template' -> template'

opts :: Maybe String -> G.ParserOptions IO
opts maybeModelName =
  (G.mkParserOptions (fileResolver maybeModelName))
    { G.poSourceName = Nothing,
      G.poKeepTrailingNewline = True
    }

fileResolver :: Maybe String -> G.IncludeResolver IO
fileResolver maybeModelName filename = do
  content <- loadFile filename
  let content' = case maybeModelName of
        Just modelName -> "{%- set model = " ++ modelName ++ "Model -%}\n" ++ content
        _ -> content
  return $ Just content'

loadFile :: FilePath -> IO String
loadFile fn = do
  cachedFileContents <- getCachedFileContents
  SIOE.tryIOError (loadFile' $ "ginger/" ++ fn) >>= \e ->
    case e of
      Right contents -> return contents
      Left err -> do
        case L.lookup fn cachedFileContents of
          Just content -> return $ UTF8.toString content
          _ -> return $ show err
  where
    loadFile' :: FilePath -> IO String
    loadFile' fn' = do
      SIO.withFile fn' SIO.ReadMode $ \h -> do
        SIO.hSetEncoding h SIO.utf8_bom
        contents <- SIOS.hGetContents h
        return contents

getCachedFileContents :: IO [(FilePath, Data.ByteString.ByteString)]
getCachedFileContents = do
  let tuples1 = $(embedDir "ginger")
  -- create also filenames wir leading './'
  let tuples2 = L.map (\(fn, bs) -> ("./" ++ fn, bs)) tuples1
  return $ tuples1 ++ tuples2

-- helpers

upperFirst :: Text -> Text
upperFirst t = Text.append (Text.toUpper $ Text.take 1 t) (Text.drop 1 t)

lowerFirst :: Text -> Text
lowerFirst t = Text.append (Text.toLower $ Text.take 1 t) (Text.drop 1 t)

-- types

data BContext = BContext
  { bContextCrudModels :: [BCrudModel],
    bContextActionModels :: [BActionModel],
    bContextGlobalTranslations :: [BTranslation]
  }

instance ToJSON BContext where
  toJSON o =
    object $
      [ "crudModels" .= bContextCrudModels o,
        "actionModels" .= bContextActionModels o,
        "translations"
          .= ( ( map
                   ( \translation@(BTranslation {bTranslationKey = tk}) ->
                       translation {bTranslationKey = Text.concat ["global", upperFirst tk]}
                   )
                   $ bContextGlobalTranslations o
               )
                 ++ ( foldl (\acc i -> acc ++ i) []
                        $ map
                          ( \( BCrudModel
                                 { bCrudModelName = modelName,
                                   bCrudModelFields = fields
                                 }
                               ) ->
                                map
                                  ( \BCrudField
                                       { bCrudFieldName = fieldName,
                                         bCrudFieldLabelDe = labelDe,
                                         bCrudFieldLabelEn = labelEn
                                       } ->
                                        BTranslation
                                          { bTranslationKey =
                                              Text.concat
                                                [modelName, upperFirst fieldName],
                                            bTranslationDe = case labelDe of
                                              Just label -> label
                                              _ -> "",
                                            bTranslationEn = case labelEn of
                                              Just label -> label
                                              _ -> ""
                                          }
                                  )
                                  fields
                          )
                        $ bContextCrudModels o
                    )
                 ++ ( foldl (\acc i -> acc ++ i) []
                        $ map
                          ( \BCrudModel {bCrudModelTranslations = maybeTranslations, bCrudModelName = modelName} ->
                              case maybeTranslations of
                                Just translations ->
                                  map
                                    ( \translation ->
                                        BTranslation
                                          { bTranslationKey = Text.concat [modelName, upperFirst $ bTranslationKey translation],
                                            bTranslationDe = bTranslationDe translation,
                                            bTranslationEn = bTranslationEn translation
                                          }
                                    )
                                    translations
                                _ -> []
                          )
                        $ bContextCrudModels o
                    )
                 ++ ( foldl (\acc i -> acc ++ i) []
                        $ map
                          ( \( BActionModel
                                 { bActionModelName = modelName,
                                   bActionModelFields = fields
                                 }
                               ) ->
                                map
                                  ( \BActionField
                                       { bActionFieldName = fieldName,
                                         bActionFieldLabelDe = labelDe,
                                         bActionFieldLabelEn = labelEn
                                       } ->
                                        BTranslation
                                          { bTranslationKey =
                                              Text.concat
                                                [modelName, upperFirst fieldName],
                                            bTranslationDe = case labelDe of
                                              Just label -> label
                                              _ -> "",
                                            bTranslationEn = case labelEn of
                                              Just label -> label
                                              _ -> ""
                                          }
                                  )
                                  fields
                          )
                        $ bContextActionModels o
                    )
                 ++ ( foldl (\acc i -> acc ++ i) []
                        $ map
                          ( \BActionModel {bActionModelTranslations = maybeTranslations, bActionModelName = modelName} ->
                              case maybeTranslations of
                                Just translations ->
                                  map
                                    ( \translation ->
                                        BTranslation
                                          { bTranslationKey = Text.concat [modelName, upperFirst $ bTranslationKey translation],
                                            bTranslationDe = bTranslationDe translation,
                                            bTranslationEn = bTranslationEn translation
                                          }
                                    )
                                    translations
                                _ -> []
                          )
                        $ bContextActionModels o
                    )
             )
      ]
        ++ ( map
               (\bCrudModel@(BCrudModel {bCrudModelName = modelName}) -> (modelName <> "Model") .= bCrudModel)
               $ bContextCrudModels o
           )
        ++ ( map
               (\bActionModel@(BActionModel {bActionModelName = modelName}) -> (modelName <> "Model") .= bActionModel)
               $ bContextActionModels o
           )

data BCrudModel = BCrudModel
  { bCrudModelName :: Text,
    bCrudModelIsJson :: Bool,
    bCrudModelDbUniquenesses :: [Text],
    bCrudModelDbHasHistoryTable :: Bool,
    bCrudModelHsDerivings :: [Text],
    bCrudModelAddFormArgs :: Maybe [BFuncArg],
    bCrudModelEditFormArgs :: Maybe [BFuncArg],
    bCrudModelAddHandlerArgs :: Maybe [BFuncArg],
    bCrudModelEditHandlerArgs :: Maybe [BFuncArg],
    bCrudModelAddFormEntityLoader :: Maybe Text,
    bCrudModelEditFormEntityLoader :: Maybe Text,
    bCrudModelDeleteFormEntityLoader :: Maybe Text,
    bCrudModelAddFormDataJsonUrl :: Maybe Text,
    bCrudModelEditFormDataJsonUrl :: Maybe Text,
    bCrudModelDeleteFormDataJsonUrl :: Maybe Text,
    bCrudModelAddFormHasDefaultModel :: Bool,
    bCrudModelEditPostLoadsModel :: Bool,
    bCrudModelDeletePostLoadsModel :: Bool,
    bCrudModelAddPostExtraStoreFunc :: Maybe Text,
    bCrudModelEditPostExtraStoreFunc :: Maybe Text,
    bCrudModelAddFormTitleMsg :: Maybe Text,
    bCrudModelEditFormTitleMsg :: Maybe Text,
    bCrudModelDeleteFormTitleMsg :: Maybe Text,
    bCrudModelParentHsType :: Maybe Text,
    bCrudModelFormRouteHsType :: Text,
    bCrudModelFields :: [BCrudField],
    bCrudModelTranslations :: Maybe [BTranslation]
  }

instance ToJSON BCrudModel where
  toJSON o =
    object
      [ "name" .= bCrudModelName o,
        "nameCap" .= (upperFirst $ bCrudModelName o),
        "isJson" .= bCrudModelIsJson o,
        "dbUniquenesses" .= bCrudModelDbUniquenesses o,
        "dbHasHistoryTable" .= bCrudModelDbHasHistoryTable o,
        "dbTableName" .= (TC.toQuietSnake $ TC.fromAny (Text.unpack $ bCrudModelName o)),
        "dbHistoryTableName" .= ((TC.toQuietSnake $ TC.fromAny (Text.unpack $ bCrudModelName o)) ++ "_history"),
        "hsAddAssignmentLines" .= getAddAssignmentLines o,
        "hsDerivings" .= bCrudModelHsDerivings o,
        "fields" .= bCrudModelFields o,
        "addViewFields" .= (filter (\field -> M.isJust $ bCrudFieldAddView field) $ bCrudModelFields o),
        "editViewFields" .= (filter (\field -> M.isJust $ bCrudFieldEditView field) $ bCrudModelFields o),
        "isInDb" .= (L.any M.isJust $ L.map bCrudFieldDb $ bCrudModelFields o),
        "addFormArgs" .= bCrudModelAddFormArgs o,
        "editFormArgs" .= bCrudModelEditFormArgs o,
        "addHandlerArgs" .= bCrudModelAddHandlerArgs o,
        "editHandlerArgs" .= bCrudModelEditHandlerArgs o,
        "addFormEntityLoader" .= bCrudModelAddFormEntityLoader o,
        "editFormEntityLoader" .= bCrudModelEditFormEntityLoader o,
        "deleteFormEntityLoader" .= bCrudModelDeleteFormEntityLoader o,
        "addFormDataJsonUrl" .= bCrudModelAddFormDataJsonUrl o,
        "editFormDataJsonUrl" .= bCrudModelEditFormDataJsonUrl o,
        "deleteFormDataJsonUrl" .= bCrudModelDeleteFormDataJsonUrl o,
        "addFormHasDefaultModel" .= bCrudModelAddFormHasDefaultModel o,
        "editPostLoadsModel" .= bCrudModelEditPostLoadsModel o,
        "deletePostLoadsModel" .= bCrudModelDeletePostLoadsModel o,
        "addPostExtraStoreFunc" .= bCrudModelAddPostExtraStoreFunc o,
        "editPostExtraStoreFunc" .= bCrudModelEditPostExtraStoreFunc o,
        "addFormTitleMsg" .= bCrudModelAddFormTitleMsg o,
        "editFormTitleMsg" .= bCrudModelEditFormTitleMsg o,
        "deleteFormTitleMsg" .= bCrudModelDeleteFormTitleMsg o,
        "parentHsType" .= bCrudModelParentHsType o,
        "parentHsParamId" .= getCrudParentHsParamId o,
        "dbFields" .= getDbFields o,
        "dbUpdatableFields"
          .= ( filter
                 ( \field -> case bCrudFieldDb field of
                     Just BCrudFieldDb {bCrudFieldDbCanUpdate = canUpdate} -> canUpdate && (M.isJust $ bCrudFieldEditView field)
                     Nothing -> False
                 )
                 $ bCrudModelFields o
             ),
        "formRouteHsType" .= bCrudModelFormRouteHsType o,
        "formHasProgressBar" .= (any (\field -> bCrudFieldHsType field == "FileInfo") $ bCrudModelFields o)
      ]

data BActionModel = BActionModel
  { bActionModelName :: Text,
    bActionModelAction :: Text,
    bActionModelFields :: [BActionField],
    bActionModelFormArgs :: Maybe [BFuncArg],
    bActionModelFormEntityLoader :: Maybe Text,
    bActionModelFormDataJsonUrl :: Maybe Text,
    bActionModelFormHasDefaultModel :: Bool,
    bActionModelFormTitleMsg :: Maybe Text,
    bActionModelParentHsType :: Maybe Text,
    bActionModelFormRouteHsType :: Text,
    bActionModelTranslations :: Maybe [BTranslation]
  }

instance ToJSON BActionModel where
  toJSON o =
    object
      [ "name" .= bActionModelName o,
        "nameCap" .= (upperFirst $ bActionModelName o),
        "action" .= bActionModelAction o,
        "actionCap" .= (upperFirst $ bActionModelAction o),
        "fields" .= bActionModelFields o,
        "viewFields" .= (filter (\field -> M.isJust $ bActionFieldView field) $ bActionModelFields o),
        "formArgs" .= bActionModelFormArgs o,
        "formEntityLoader" .= bActionModelFormEntityLoader o,
        "formDataJsonUrl" .= bActionModelFormDataJsonUrl o,
        "formHasDefaultModel" .= bActionModelFormHasDefaultModel o,
        "formTitleMsg" .= bActionModelFormTitleMsg o,
        "parentHsType" .= bActionModelParentHsType o,
        "parentHsParamId" .= getActionParentHsParamId o,
        "formRouteHsType" .= bActionModelFormRouteHsType o,
        "formHasProgressBar" .= (any (\field -> bActionFieldHsType field == "FileInfo") $ bActionModelFields o)
      ]

getDbFields :: BCrudModel -> [BCrudField]
getDbFields m = filter (M.isJust . bCrudFieldDb) $ bCrudModelFields m

getAddAssignmentLines :: BCrudModel -> [Text]
getAddAssignmentLines m =
  (if M.isJust $ bCrudModelParentHsType m then [Text.concat [bCrudModelName m, M.fromJust $ bCrudModelParentHsType m, "Id", " = ", getCrudParentHsParamId m]] else [])
    ++ ( map
           ( \f ->
               Text.concat
                 [ bCrudModelName m,
                   upperFirst $ bCrudFieldName f,
                   " = ",
                   case bCrudFieldAddView f of
                     Just _ -> Text.concat ["vAdd", upperFirst $ bCrudModelName m, upperFirst $ bCrudFieldName f, " vAdd", upperFirst $ bCrudModelName m]
                     _ -> "Nothing"
                 ]
           )
           $ filter
             ( \f -> case bCrudModelParentHsType m of
                 Just hsType -> Text.concat [hsType, "Id"] /= bCrudFieldHsType f
                 _ -> True
             )
           $ getDbFields m
       )

getCrudParentHsParamId :: BCrudModel -> Text
getCrudParentHsParamId m = case bCrudModelParentHsType m of
  Just parentHsType -> lowerFirst $ Text.append parentHsType "Id"
  _ -> ""

getActionParentHsParamId :: BActionModel -> Text
getActionParentHsParamId m = case bActionModelParentHsType m of
  Just parentHsType -> lowerFirst $ Text.append parentHsType "Id"
  _ -> ""

data BTranslation = BTranslation
  { bTranslationKey :: Text,
    bTranslationDe :: Text,
    bTranslationEn :: Text
  }

instance ToJSON BTranslation where
  toJSON o =
    object
      [ "key" .= bTranslationKey o,
        "keyCap" .= (upperFirst $ bTranslationKey o),
        "de" .= bTranslationDe o,
        "en" .= bTranslationEn o
      ]

data BFuncArg = BFuncArg
  { bFuncArgType :: Text,
    bFuncArgName :: Text
  }

instance ToJSON BFuncArg where
  toJSON o =
    object
      [ "type" .= bFuncArgType o,
        "name" .= bFuncArgName o
      ]

data BCrudFieldDb = BCrudFieldDb
  { bCrudFieldDbIsNullable :: Bool,
    bCrudFieldDbDefault :: Maybe Text,
    bCrudFieldDbCanUpdate :: Bool
  }

instance ToJSON BCrudFieldDb where
  toJSON o =
    object
      [ "isNullable" .= bCrudFieldDbIsNullable o,
        "isNotNullable" .= (not $ bCrudFieldDbIsNullable o),
        "default" .= bCrudFieldDbDefault o,
        "canUpdate" .= bCrudFieldDbCanUpdate o
      ]

data BFieldView = BFieldView
  { bFieldViewIsRequired :: Bool,
    bFieldViewIsDisabled :: Bool,
    bFieldViewAttrs :: [BFieldAttr],
    bFieldViewDefault :: Maybe Text
  }

instance ToJSON BFieldView where
  toJSON o =
    object
      [ "isRequired" .= bFieldViewIsRequired o,
        "isOptional" .= (not $ bFieldViewIsRequired o),
        "isDisabled" .= bFieldViewIsDisabled o,
        "isEnabled" .= (not $ bFieldViewIsDisabled o),
        "attrs"
          .= ( (if bFieldViewIsDisabled o then [BFieldAttr "disabled" ""] else [])
                 ++ bFieldViewAttrs o
             ),
        "default" .= bFieldViewDefault o
      ]

data BCrudField = BCrudField
  { bCrudFieldName :: Text,
    bCrudFieldLabelDe :: Maybe Text,
    bCrudFieldLabelEn :: Maybe Text,
    bCrudFieldHsType :: Text,
    bCrudFieldDb :: Maybe BCrudFieldDb,
    bCrudFieldFormFieldType :: Maybe Text,
    bCrudFieldAddView :: Maybe BFieldView,
    bCrudFieldEditView :: Maybe BFieldView
  }

instance ToJSON BCrudField where
  toJSON o =
    object
      [ "name" .= bCrudFieldName o,
        "nameCap" .= (upperFirst $ bCrudFieldName o),
        "dbColumnName" .= (TC.toQuietSnake $ TC.fromAny (Text.unpack $ bCrudFieldName o)),
        "labelDe" .= bCrudFieldLabelDe o,
        "labelEn" .= bCrudFieldLabelEn o,
        "hsType" .= bCrudFieldHsType o,
        "db" .= bCrudFieldDb o,
        "formFieldType" .= bCrudFieldFormFieldType o,
        "addView" .= bCrudFieldAddView o,
        "editView" .= bCrudFieldEditView o,
        "isHsTypeBool" .= (bCrudFieldHsType o == "Bool"),
        "isForeignKey" .= ((Text.takeEnd 2 $ bCrudFieldName o) == "Id")
      ]

data BActionField = BActionField
  { bActionFieldName :: Text,
    bActionFieldLabelDe :: Maybe Text,
    bActionFieldLabelEn :: Maybe Text,
    bActionFieldHsType :: Text,
    bActionFieldFormFieldType :: Maybe Text,
    bActionFieldView :: Maybe BFieldView
  }

instance ToJSON BActionField where
  toJSON o =
    object
      [ "name" .= bActionFieldName o,
        "nameCap" .= (upperFirst $ bActionFieldName o),
        "labelDe" .= bActionFieldLabelDe o,
        "labelEn" .= bActionFieldLabelEn o,
        "hsType" .= bActionFieldHsType o,
        "formFieldType" .= bActionFieldFormFieldType o,
        "view" .= bActionFieldView o,
        "isHsTypeBool" .= (bActionFieldHsType o == "Bool")
      ]

data BFieldAttr = BFieldAttr
  { bFieldAttrKey :: Text,
    bFieldAttrValue :: Text
  }

instance ToJSON BFieldAttr where
  toJSON o =
    object
      [ "key" .= bFieldAttrKey o,
        "value" .= bFieldAttrValue o
      ]
