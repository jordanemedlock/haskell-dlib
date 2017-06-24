
#include <dlib/gui_widgets.h>

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
void inline_c_Vision_DLib_GUI_ImageWindow_2_c16e9e93790a44a88f06d71cf039b416556294b4(image_window * winPtr_inline_c_0, image * imgPtr_inline_c_1) {

  winPtr_inline_c_0->set_image( imgPtr_inline_c_1 );

}

}
