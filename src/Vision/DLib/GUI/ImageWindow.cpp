
#include <dlib/gui_widgets.h>

#include <dlib/image_processing/render_face_detections.h>

#include "typedefs.h"

using namespace dlib;

extern "C" {
image_window * inline_c_Vision_DLib_GUI_ImageWindow_0_fb20adffe9ec4755f5ca002666c159d5325d961a() {
return ( new image_window() );
}

}

extern "C" {
void inline_c_Vision_DLib_GUI_ImageWindow_1_01faa3c038bd4ff0ae310c589adfd3647d7e971b(image_window * ptr_inline_c_0) {

  ptr_inline_c_0->clear_overlay();

}

}

extern "C" {
void inline_c_Vision_DLib_GUI_ImageWindow_2_3557962a462639b0f65dc7492212b610d3ea8992(image_window * winPtr_inline_c_0, image * imgPtr_inline_c_1) {

  winPtr_inline_c_0->set_image( *imgPtr_inline_c_1 );

}

}

extern "C" {
void inline_c_Vision_DLib_GUI_ImageWindow_3_e5ea350b79e7bc6f46d5515bf0a80be2ae56cc8f(image_window * winPtr_inline_c_0, full_object_detection * shapePtr_inline_c_1) {

      winPtr_inline_c_0->add_overlay(render_face_detections(*shapePtr_inline_c_1));
    
}

}
