{-# LANGUAGE OverloadedStrings #-}
module GeoInstancing where

import LambdaDesigner.Op
import LambdaDesigner.Lib

import Prelude hiding (floor, mod)

import Control.Lens
import Data.IORef
import Data.Matrix

import qualified Data.ByteString.Char8 as BS

go =
  let
    ain = math' (mathMult ?~ float 10) [audioIn]
    -- Use instanceGeo to create a Geo node with instancing turned on.
    sgeo = instanceGeo' ((geoMat ?~ wireframeM)) poses (outS $ lineS)
    -- Number of instances we want
    instances = casti (volc !* float 30) !+ int 2
    -- How far along the x axis the instances should stretch
    width = float 20
    -- The CHOP that has all of our instancing data. Note that we have to stretch to align it with our tx.
    poses = mergeC' (mergeCAlign ?~ int 7) [tx, ty, rz, sx]
    -- tx is a linear function based on instances and width
    tx = waveC' (waveCNames ?~ str "tx") instances $ ((castf (sampleIndex !* casti width !* int 2)) !/ castf instances) !- width
    -- ty is the audio in wave resampled to the number of instances we have
    ty = ain & resampleC' ((resampleEnd ?~ instances) . (resampleRate ?~ instances)) False & renameC (str "ty")
    -- rz is a simple sin wave
    rz = waveC' (waveCNames ?~ str "rz") instances $ osin (castf sampleIndex) !* float 360
    -- sx increases the scale of the lines according to instance number
    sx = waveC' (waveCNames ?~ str "sx") instances $ castf sampleIndex !* float 0.1
    -- utility function to center the camera on the origin
    centerCam t r = cam' ((camTranslate .~ t) . (camPivot .~ v3mult (float (-1)) t) . (camRotate .~ r))
    -- move the camera back 50
    grender = render sgeo (centerCam (v3 (float 0) (float 0) (float 50)) emptyV3)
    volume = analyze (int 6) ain
    volc = chan0f volume
  in
    do r <- newIORef mempty
       run r [outT $ grender & fade (float 0.95)]

fade' f l o t = feedbackT t (\t' -> l $ compT 0 [t, levelT' (levelOpacity ?~ o) t']) f
fade = fade' id id
