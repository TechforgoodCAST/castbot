{-# LANGUAGE OverloadedStrings #-}

module GoogleDrive where

import Snap.Core
import Snap.Snaplet

data GoogleDrive = GoogleDrive

gDriveInit :: SnapletInit b GoogleDrive
gDriveInit = makeSnaplet "google-drive" "google drive snaplet" Nothing $ return GoogleDrive
