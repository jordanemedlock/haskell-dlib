{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Vision.DLib.Types.Constants where


import qualified Language.C.Inline as C
import qualified Language.C.Inline.Internal as C
import qualified Language.C.Inline.Cpp as C

import Vision.DLib.Types.InlineC

C.context dlibCtx

C.include "<dlib/geometry.h>"

C.emitVerbatim "#define alignof(type) offsetof(struct { char c; type d; }, d)"

sizeofPoint :: C.CLong
sizeofPoint = [C.pure| long { sizeof(dlib::point) }|]

alignofPoint :: C.CLong
alignofPoint = [C.pure| long { alignof(dlib::point) }|]