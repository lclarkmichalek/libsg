{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Picture.SG
       ( SGFile
       , SGBitmap
       , SGImage
       , SGImageData
         ( imgDataWidth
         , imgDataHeight
         , imgDataMask
         , imgDataRaw
         )
       , readSGFile
       , sgFileVersion
       , sgFileTotalFilesize
       , sgFile555Filesize
       , sgFileExternalFilesize
       , sgFileBitmaps
       , sgFileImages
       , sgBitmapFilename
       , sgBitmapComment
       , sgBitmapWidth
       , sgBitmapHeight
       , sgBitmapID
       , sgBitmapImages
       , sgImageLength
       , sgImageWidth
       , sgImageHeight
       , sgImageType
       , sgImageExtern
       , sgImageParent
       , sgImageID
       , loadSGImageData
       ) where

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

#include <sg/sgimage.h>

import qualified Data.ByteString as B

import Data.Int
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad (ap)
import Foreign hiding (unsafeLocalState)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Foreign.C.Types
import Foreign.C.String

data SGFile = SGFile (ForeignPtr ())

data SGBitmap = SGBitmap (Ptr ()) SGFile

data SGImage = SGImage (Ptr ()) SGFile

data SGImageData = SGImageData
                   { imgDataPtr :: ForeignPtr ()
                   , imgDataWidth :: Int
                   , imgDataHeight :: Int
                     -- RGBA mask
                   , imgDataMask :: (Int, Int, Int, Int)
                   , imgDataRaw :: Ptr ()
                   } deriving (Show, Eq)

-- File
foreign import ccall "sg/sgfile.h sg_read_file"
  c_sg_read_file :: CString -> IO (Ptr ())

foreign import ccall "sg/sgfile.h sg_get_file_version"
  c_sg_get_file_version :: Ptr () -> IO CUInt

foreign import ccall "sg/sgfile.h sg_get_file_total_filesize"
  c_sg_get_file_total_filesize :: Ptr () -> IO CUInt

foreign import ccall "sg/sgfile.h sg_get_file_555_filesize"
  c_sg_get_file_555_filesize :: Ptr () -> IO CUInt

foreign import ccall "sg/sgfile.h sg_get_file_external_filesize"
  c_sg_get_file_external_filesize :: Ptr () -> IO CUInt

foreign import ccall "sg/sgfile.h sg_get_file_bitmap_count"
  c_sg_get_file_bitmap_count :: Ptr () -> IO CInt

foreign import ccall "sg/sgfile.h sg_get_file_bitmap"
  c_sg_get_file_bitmap :: Ptr () -> CInt -> IO (Ptr ())

foreign import ccall "sg/sgfile.h sg_get_file_image_count"
  c_sg_get_file_image_count :: Ptr () -> IO CInt

foreign import ccall "sg/sgfile.h sg_get_file_image"
  c_sg_get_file_image :: Ptr () -> CInt -> IO (Ptr ())

-- Bitmap
foreign import ccall "sg/sgbitmap.h sg_get_bitmap_filename"
  c_sg_get_bitmap_filename :: Ptr () -> IO (Ptr CChar)

foreign import ccall "sg/sgbitmap.h sg_get_bitmap_comment"
  c_sg_get_bitmap_comment :: Ptr () -> IO (Ptr CChar)

foreign import ccall "sg/sgbitmap.h sg_get_bitmap_width"
  c_sg_get_bitmap_width :: Ptr () -> IO CUInt

foreign import ccall "sg/sgbitmap.h sg_get_bitmap_height"
  c_sg_get_bitmap_height :: Ptr () -> IO CUInt

foreign import ccall "sg/sgbitmap.h sg_get_bitmap_id"
  c_sg_get_bitmap_id :: Ptr () -> IO CInt

foreign import ccall "sg/sgbitmap.h sg_get_bitmap_image"
  c_sg_get_bitmap_image :: Ptr () -> CInt -> IO (Ptr ())

foreign import ccall "sg/sgbitmap.h sg_get_bitmap_image_count"
  c_sg_get_bitmap_image_count :: Ptr () -> IO CInt

-- Image
foreign import ccall "sg/sgimage.h sg_get_image_length"
  c_sg_get_image_length :: Ptr () -> IO CInt

foreign import ccall "sg/sgimage.h sg_get_image_width"
  c_sg_get_image_width :: Ptr () -> IO CInt

foreign import ccall "sg/sgimage.h sg_get_image_height"
  c_sg_get_image_height :: Ptr () -> IO CInt

foreign import ccall "sg/sgimage.h sg_get_image_type"
  c_sg_get_image_type :: Ptr () -> IO CInt

foreign import ccall "sg/sgimage.h sg_get_image_extern"
  c_sg_get_image_extern :: Ptr () -> IO Bool

foreign import ccall "sg/sgimage.h sg_get_image_parent"
  c_sg_get_image_parent :: Ptr () -> IO (Ptr ())

foreign import ccall "sg/sgimage.h sg_get_image_id"
  c_sg_get_image_id :: Ptr () -> IO CInt

foreign import ccall "sg/sgimage.h sg_get_image_error"
  c_sg_get_image_error :: Ptr () -> IO (Ptr CChar)

-- Image data
foreign import ccall "sg/sgimage.h sg_load_image_data"
  c_sg_load_image_data :: Ptr () -> CString -> IO (Ptr ())

-- DTORS
foreign import ccall "sg/sgfile.h &sg_delete_file"
  c_sg_delete_file :: FunPtr (Ptr () -> IO ())

foreign import ccall "sg/sgimage.h &sg_delete_image_data"
  c_sg_delete_image_data :: FunPtr(Ptr () -> IO ())

mZip :: Monad m => m a -> m b -> m c -> m d -> m (a, b, c, d)
mZip a b c d =
  a >>= \a' ->
  b >>= \b' ->
  c >>= \c' ->
  d >>= \d' ->
  return (a', b', c', d')

-- File

createSGFile :: Ptr () -> IO SGFile
createSGFile ptr =
  return SGFile `ap` newForeignPtr c_sg_delete_file ptr

wrapSGFile :: (Ptr () -> IO a) -> SGFile -> IO a
wrapSGFile f (SGFile fptr) = withForeignPtr fptr f

readSGFile :: String -> IO (Maybe SGFile)
readSGFile fname =
  withCString fname $ \fname' -> do
    filePtr <- c_sg_read_file fname'
    if filePtr == nullPtr
      then return Nothing
      else fmap Just $ createSGFile filePtr

sgFileBitmaps :: SGFile -> [SGBitmap]
sgFileBitmaps file@(SGFile fptr) =
  unsafeLocalState $
  withForeignPtr fptr $ \ptr -> do
    n <- c_sg_get_file_bitmap_count ptr
    mapM (fmap ((flip SGBitmap) file) . c_sg_get_file_bitmap ptr) [0..n-1]

sgFileImages :: SGFile -> [SGImage]
sgFileImages file@(SGFile fptr) =
  unsafeLocalState $
  withForeignPtr fptr $ \ptr -> do
    n <- c_sg_get_file_image_count ptr
    mapM (fmap ((flip SGImage) file) . c_sg_get_file_image ptr) [0..n-1]

sgFileVersion :: SGFile -> Int
sgFileVersion = fromIntegral .
                unsafeLocalState .
                wrapSGFile c_sg_get_file_version

sgFileTotalFilesize :: SGFile -> Int
sgFileTotalFilesize = fromIntegral .
                      unsafeLocalState .
                      wrapSGFile c_sg_get_file_total_filesize

sgFile555Filesize :: SGFile -> Int
sgFile555Filesize = fromIntegral .
                    unsafeLocalState .
                    wrapSGFile c_sg_get_file_555_filesize

sgFileExternalFilesize :: SGFile -> Int
sgFileExternalFilesize = fromIntegral .
                         unsafeLocalState .
                         wrapSGFile c_sg_get_file_external_filesize

-- Bitmap
sgBitmapFilename :: SGBitmap -> String
sgBitmapFilename (SGBitmap ptr _) =
  unsafeLocalState $
  peekCString =<<
  c_sg_get_bitmap_filename ptr

sgBitmapComment :: SGBitmap -> String
sgBitmapComment (SGBitmap ptr _) =
  unsafeLocalState $
  peekCString =<<
  c_sg_get_bitmap_filename ptr

sgBitmapWidth :: SGBitmap -> Int
sgBitmapWidth (SGBitmap ptr _) =
  fromIntegral .
  unsafeLocalState $
  c_sg_get_bitmap_width ptr

sgBitmapHeight :: SGBitmap -> Int
sgBitmapHeight (SGBitmap ptr _) =
  fromIntegral .
  unsafeLocalState $
  c_sg_get_bitmap_height ptr

sgBitmapID :: SGBitmap -> Int
sgBitmapID (SGBitmap ptr _) =
  fromIntegral .
  unsafeLocalState $
  c_sg_get_bitmap_id ptr

sgBitmapImages :: SGBitmap -> [SGImage]
sgBitmapImages (SGBitmap ptr file) =
  unsafeLocalState $ do
    n <- c_sg_get_bitmap_image_count ptr
    mapM (fmap ((flip SGImage) file) . c_sg_get_bitmap_image ptr) [0..n-1]

-- Image
sgImageLength :: SGImage -> Int
sgImageLength (SGImage ptr _) =
  fromIntegral .
  unsafeLocalState $
  c_sg_get_image_length ptr

sgImageWidth :: SGImage -> Int
sgImageWidth (SGImage ptr _) =
  fromIntegral .
  unsafeLocalState $
  c_sg_get_image_width ptr

sgImageHeight :: SGImage -> Int
sgImageHeight (SGImage ptr _) =
  fromIntegral .
  unsafeLocalState $
  c_sg_get_image_height ptr

sgImageType :: SGImage -> Int
sgImageType (SGImage ptr _) =
  fromIntegral .
  unsafeLocalState $
  c_sg_get_image_type ptr

sgImageExtern :: SGImage -> Bool
sgImageExtern (SGImage ptr _) =
  unsafeLocalState $
  c_sg_get_image_extern ptr

sgImageParent :: SGImage -> SGBitmap
sgImageParent (SGImage ptr file) =
  unsafeLocalState $
  c_sg_get_image_parent ptr >>= \parent ->
  return (SGBitmap parent file)

sgImageID :: SGImage -> Int
sgImageID (SGImage ptr _) =
  fromIntegral .
  unsafeLocalState $
  c_sg_get_image_id ptr

-- Image data
createSGImageData :: Ptr () -> IO SGImageData
createSGImageData ptr =
  return SGImageData
  `ap` newForeignPtr c_sg_delete_image_data ptr
  `ap` #{peek struct SgImageData, width} ptr
  `ap` #{peek struct SgImageData, height} ptr
  `ap` (mZip
        (#{peek struct SgImageData, rMask} ptr)
        (#{peek struct SgImageData, gMask} ptr)
        (#{peek struct SgImageData, bMask} ptr)
        (#{peek struct SgImageData, aMask} ptr))
  `ap` #{peek struct SgImageData, data} ptr

loadSGImageData :: SGImage -> String -> IO (Either String SGImageData)
loadSGImageData (SGImage imgPtr _) file555 =
  withCString file555 $ \cFile555 -> do
    cImgData <- c_sg_load_image_data imgPtr cFile555
    if cImgData == nullPtr
      then c_sg_get_image_error imgPtr >>=
           peekCString >>=
           return . Left
      else fmap Right $ createSGImageData cImgData
