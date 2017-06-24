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
C.include "<dlib/image_processing/render_face_detections.h>"
C.include "<iostream>"

-- C.emitVerbatim "#define alignof(type) offsetof(struct { char c; type d; }, d)"

sizeofPoint :: C.CLong
sizeofPoint = [C.pure| long { sizeof(dlib::point) }|]

alignofPoint :: C.CLong
alignofPoint = [C.pure| long { alignof(dlib::point) }|]

sizeofShape :: C.CLong
sizeofShape = [C.pure| long { sizeof(dlib::full_object_detection) }|]

alignofShape :: C.CLong
alignofShape = [C.pure| long { alignof(dlib::full_object_detection) }|]

sizeofRect :: C.CLong
sizeofRect = [C.pure| long { sizeof(dlib::rectangle) }|]

alignofRect :: C.CLong
alignofRect = [C.pure| long { alignof(dlib::rectangle) }|]

sizeofVector :: C.CLong
sizeofVector = [C.pure| long { sizeof(std::vector<dlib::point>) }|]

alignofVector :: C.CLong
alignofVector = [C.pure| long { alignof(std::vector<dlib::point>) }|]
