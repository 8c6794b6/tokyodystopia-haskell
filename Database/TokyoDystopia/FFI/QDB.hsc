{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}
------------------------------------------------------------------------------
-- |
-- Inner guts of haskell binding for tcqdb.h.
--
module Database.TokyoDystopia.FFI.QDB
    (
    -- * Types and constants

    -- ** Database
      TCQDB

    -- ** OpenMode
    , OpenMode(..)
    , omReader
    , omWriter
    , omCreat
    , omTrunc
    , omNolck
    , omLcknb

    -- ** GetMode
    , GetMode(..)
    , gmSubstr
    , gmPrefix
    , gmSuffix
    , gmFull

    -- ** Tuning options
    , TuningOption(..)
    , toLarge
    , toDeflate
    , toBzip
    , toTcbs

    -- ** Text normalize options
    , TNOption(..)
    , tnLower
    , tnNoAcc
    , tnSpace

    -- ** Set
    , QDBRSET
    , TCIDSET

    -- * C Functions

    -- ** Basic functions
    , c_close
    , c_copy
    , c_del
    , c_ecode
    , c_errmsg
    , c_fsiz
    , c_new
    , c_open
    , c_optimize
    , c_out
    , c_path
    , c_put
    , c_tnum
    , c_search
    , c_setcache
    , c_setfwmmax
    , c_sync
    , c_tune
    , c_vanish

    -- ** Advanced functions
    , c_setdbgfd
    , c_dbgfd
    , c_memsync
    , c_cacheclear
    , c_inode
    , c_mtime
    , c_opts
    , c_fwmmax
    , c_cnum
    , c_setsynccb
    , c_setsynccb_wrapper
    , c_resunion
    , c_resisect
    , c_resdiff
    , c_textnormalize
    , c_tcidsetnew
    , c_tcidsetdel
    , c_tcidsetmark
    , c_tcidsetcheck
    , c_tcidsetclear
    ) where

import Data.Int
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <tcqdb.h>

------------------------------------------------------------------------------
--
-- Haskell Side Datatype definitions
--
------------------------------------------------------------------------------

data OpenMode = OpenMode { unOpenMode :: CInt }
              deriving (Eq, Show)

data GetMode = GetMode { unGetMode :: CInt }
             deriving (Eq, Show)

data TuningOption = TuningOption { unTuningOption :: CInt }
               deriving (Eq, Show)

data TNOption = TNOption { unTNOption :: CInt }
              deriving (Eq, Show)

data TCQDB

data QDBRSET

data TCIDSET

------------------------------------------------------------------------------
--
-- Enums
--
------------------------------------------------------------------------------

#{enum OpenMode, OpenMode
 , omReader = QDBOREADER
 , omWriter = QDBOWRITER
 , omCreat  = QDBOCREAT
 , omTrunc  = QDBOTRUNC
 , omNolck  = QDBONOLCK
 , omLcknb  = QDBOLCKNB }

#{enum GetMode, GetMode
 , gmSubstr = QDBSSUBSTR
 , gmPrefix = QDBSPREFIX
 , gmSuffix = QDBSSUFFIX
 , gmFull   = QDBSFULL }

#{enum TuningOption, TuningOption
 , toLarge   = QDBTLARGE
 , toDeflate = QDBTDEFLATE
 , toBzip    = QDBTBZIP
 , toTcbs    = QDBTTCBS }

#{enum TNOption, TNOption
 , tnLower = TCTNLOWER
 , tnNoAcc = TCTNNOACC
 , tnSpace = TCTNSPACE }

------------------------------------------------------------------------------
--
-- Function calls
--
------------------------------------------------------------------------------

foreign import ccall "tcqdb.h tcqdberrmsg"
  c_errmsg :: CInt -> CString

foreign import ccall "tcqdb.h tcqdbnew"
  c_new :: IO (Ptr TCQDB)

foreign import ccall "tcqdb.h tcqdbdel"
  c_del :: Ptr TCQDB -> IO ()

foreign import ccall "tcqdb.h tcqdbecode"
  c_ecode :: Ptr TCQDB -> IO CInt

foreign import ccall "tcqdb.h tcqdbtune"
  c_tune :: Ptr TCQDB -> Int64 -> CUInt -> IO Bool

foreign import ccall "tcqdb.h tcqdbsetcache"
  c_setcache :: Ptr TCQDB -> Int64 -> Int32 -> IO Bool

foreign import ccall "tcqdb.h tcqdbsetfwmmax"
  c_setfwmmax :: Ptr TCQDB -> Int32 -> IO Bool

foreign import ccall "tcqdb.h tcqdbopen"
  c_open :: Ptr TCQDB -> CString -> CInt -> IO Bool

foreign import ccall "tcqdb.h tcqdbclose"
  c_close :: Ptr TCQDB -> IO Bool

foreign import ccall "tcqdb.h tcqdbput"
  c_put :: Ptr TCQDB -> Int64 -> CString -> IO Bool

foreign import ccall "tcqdb.h tcqdbout"
  c_out :: Ptr TCQDB -> Int64 -> CString -> IO Bool

foreign import ccall "tcqdb.h tcqdbsearch"
  c_search :: Ptr TCQDB -> CString -> CInt -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "tcqdb.h tcqdbsync"
  c_sync :: Ptr TCQDB -> IO Bool

foreign import ccall "tcqdb.h tcqdboptimize"
  c_optimize :: Ptr TCQDB -> IO Bool

foreign import ccall "tcqdb.h tcqdbvanish"
  c_vanish :: Ptr TCQDB -> IO Bool

foreign import ccall "tcqdb.h tcqdbcopy"
  c_copy :: Ptr TCQDB -> CString -> IO Bool

foreign import ccall "tcqdb.h tcqdbpath"
  c_path :: Ptr TCQDB -> IO CString

foreign import ccall "tcqdb.h tcqdbtnum"
  c_tnum :: Ptr TCQDB -> IO Int64

foreign import ccall "tcqdb.h tcqdbfsiz"
  c_fsiz :: Ptr TCQDB -> IO Int64

------------------------------------------------------------------------------
--
-- Advanced config, functions
--
------------------------------------------------------------------------------

foreign import ccall "tcqdb.h tcqdbsetdbgfd"
  c_setdbgfd :: Ptr TCQDB -> CInt -> IO ()

foreign import ccall "tcqdb.h tcqdbdbgfd"
  c_dbgfd :: Ptr TCQDB -> IO CInt

foreign import ccall "tcqdb.h tcqdbmemsync"
  c_memsync :: Ptr TCQDB -> CInt -> IO Bool

foreign import ccall "tcqdb.h tcqdbcacheclear"
  c_cacheclear :: Ptr TCQDB -> IO Bool

foreign import ccall "tcqdb.h tcqdbinode"
  c_inode :: Ptr TCQDB -> IO Int64

foreign import ccall "tcqdb.h tcqdbmtime"
  c_mtime :: Ptr TCQDB -> IO CTime

foreign import ccall "tcqdb.h tcqdbopts"
  c_opts :: Ptr TCQDB -> IO Int

foreign import ccall "tcqdb.h tcqdbfwmmax"
  c_fwmmax :: Ptr TCQDB -> IO CInt

foreign import ccall "tcqdb.h tcqdbcnum"
  c_cnum :: Ptr TCQDB -> IO CInt

foreign import ccall "wrapper"
  c_setsynccb_wrapper :: (CInt -> CInt -> CString -> IO Bool)
                      -> IO (FunPtr (CInt -> CInt -> CString -> IO Bool))

foreign import ccall "tcqdb.h tcqdbsetsynccb"
  c_setsynccb :: Ptr TCQDB -> FunPtr (CInt -> CInt -> CString -> IO Bool) -> IO Bool

foreign import ccall "tcqdb.h tcqdbresunion"
  c_resunion :: Ptr QDBRSET -> CInt -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "tcqdb.h tcqdbresisect"
  c_resisect :: Ptr QDBRSET -> CInt -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "tcqdb.h tcqdbresdiff"
  c_resdiff :: Ptr QDBRSET -> CInt -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall "tcqdb.h tctextnormalize"
  c_textnormalize :: CString -> CInt -> IO ()

foreign import ccall "tcqdb.h tcidsetnew"
  c_tcidsetnew :: Int32 -> IO (Ptr TCIDSET)

foreign import ccall "tcqdb.h tcidsetdel"
  c_tcidsetdel :: Ptr TCIDSET -> IO ()

foreign import ccall "tcqdb.h tcidsetmark"
  c_tcidsetmark :: Ptr TCIDSET -> Int64 -> IO ()

foreign import ccall "tcqdb.h tcidsetcheck"
  c_tcidsetcheck :: Ptr TCIDSET -> Int64 -> IO Bool

foreign import ccall "tcqdb.h tcidsetclear"
  c_tcidsetclear :: Ptr TCIDSET -> IO ()
