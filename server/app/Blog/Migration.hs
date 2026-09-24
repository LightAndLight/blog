{-# LANGUAGE FlexibleContexts #-}

module Blog.Migration (migrate) where

import Blog (Name, ResourceId (..), renderName, renderResourceId, unsafeName, utctimeMetadataValue)
import Blog.Diagnostic (DiagnosticReports, renderDiagnosticReports)
import Blog.Log (MonadLog)
import qualified Blog.Log as Log
import Blog.Store (Store)
import qualified Blog.Store as Store
import Control.Monad.Catch (MonadMask)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import System.Exit (exitFailure)

migrate :: (MonadLog m, MonadIO m, MonadMask m) => Store (ExceptT DiagnosticReports m) -> m ()
migrate store =
  Log.scope (fromString "migration") $ do
    result <- runExceptT . Store.withTransaction store False $ \xactId -> do
      mMigrationTy <- Store.lookupResourceType store (Just xactId) (unsafeName "migration")
      migrationTy <-
        case mMigrationTy of
          Just x -> pure x
          Nothing -> do
            resourceTy <- Store.getResourceType store (Just xactId) (unsafeName "resource")
            _changed <-
              Store.writeResource resourceTy (unsafeName "migration") . ByteString.Lazy.Char8.unlines $
                [ fromString "content-type = \"text/plain\""
                , fromString ""
                , fromString "[metadata]"
                ]
            Store.getResourceType store (Just xactId) (unsafeName "migration")
      for_ (zip [0 :: Int ..] migrations) $ \(ix, migration) ->
        Log.scope (fromString . renderName $ migrationName migration) $ do
          Log.attach (fromString "index") ix
          exists <- Store.doesResourceExist migrationTy (migrationName migration)
          if exists
            then Log.attach (fromString "status") "skipped"
            else do
              migrationAction migration store xactId
              _changed <-
                Store.writeResource migrationTy (migrationName migration) $
                  fromString (migrationDescription migration)
              Log.attach (fromString "status") "ran"

    case result of
      Left err -> liftIO $ do
        ByteString.Lazy.Char8.putStrLn $ renderDiagnosticReports err
        exitFailure
      Right () -> pure ()

data Migration m
  = Migration
  { migrationName :: !Name
  , migrationDescription :: !String
  , migrationAction :: Store m -> Store.TransactionId -> m ()
  }

migrations :: (MonadError DiagnosticReports m, MonadIO m) => [Migration m]
migrations =
  [ Migration
      (unsafeName "20260924T02:27:00Z-add-create-update-properties")
      "Add `created` and `updated` properties (see <git:commit:fbcea6c7c5a027ed817f27658abbbb8648f610ec>) to existing resources."
      ( \store xactId -> do
          resourceTy <- Store.getResourceType store (Just xactId) (unsafeName "resource")
          resIds <- Store.listResource resourceTy
          for_ resIds $ \resId -> do
            resTy <- Store.getResourceType store (Just xactId) (resourceName resId)
            resIds' <- Store.listResource resTy
            for_ resIds' $ \resId' -> do
              mCreated <- Store.lookupProperty resTy (resourceName resId') (unsafeName "created")
              created <-
                case mCreated of
                  Just created -> pure created
                  Nothing -> do
                    metadata <- Store.getMetadata resTy (resourceName resId')
                    created <-
                      case Map.lookup (fromString "published") metadata of
                        Nothing -> do
                          modificationTime <-
                            fromMaybe (error $ "no modification time for " ++ renderResourceId resId')
                              <$> Store.readResourceModificationTime resTy (resourceName resId')
                          pure $ utctimeMetadataValue modificationTime
                        Just published -> pure published
                    _changed <- Store.setProperty resTy (resourceName resId') (unsafeName "created") created
                    pure created

              mUpdated <- Store.lookupProperty resTy (resourceName resId') (unsafeName "updated")
              case mUpdated of
                Just{} ->
                  pure ()
                Nothing -> do
                  _changed <- Store.setProperty resTy (resourceName resId') (unsafeName "updated") created
                  pure ()
      )
  ]
