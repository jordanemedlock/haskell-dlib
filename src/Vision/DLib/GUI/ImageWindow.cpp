
#include <dlib/gui_core.h>

#ifndef DLIB_NO_GUI_SUPPORT

#include <dlib/gui_widgets.h>

#endif

#include <dlib/image_processing/render_face_detections.h>

#include "typedefs.h"

using namespace dlib;

extern "C" {
image_window * inline_c_Vision_DLib_GUI_ImageWindow_0_f9b645b7bca2fe10c27fe117b4c091a8c69c34d8() {

#ifndef DLIB_NO_GUI_SUPPORT
  return new image_window();
#endif
  return 0;

}

}

extern "C" {
void inline_c_Vision_DLib_GUI_ImageWindow_1_36d34fec794a93a1f2dba310eeb54646b9826f88(image_window * ptr_inline_c_0) {

#ifndef DLIB_NO_GUI_SUPPORT
  ptr_inline_c_0->clear_overlay();
#endif

}

}

extern "C" {
void inline_c_Vision_DLib_GUI_ImageWindow_2_36c323164676a3018a9caad0c76839af71f9cbb9(image_window * winPtr_inline_c_0, image * imgPtr_inline_c_1) {

#ifndef DLIB_NO_GUI_SUPPORT
  winPtr_inline_c_0->set_image( *imgPtr_inline_c_1 );
#endif

}

}

extern "C" {
void inline_c_Vision_DLib_GUI_ImageWindow_3_5523ff38cc05afa402d3d7678822237923c5c237(image_window * winPtr_inline_c_0, full_object_detection * shapePtr_inline_c_1) {

    #ifndef DLIB_NO_GUI_SUPPORT
      winPtr_inline_c_0->add_overlay(render_face_detections(*shapePtr_inline_c_1));
    #endif
    
}

}
