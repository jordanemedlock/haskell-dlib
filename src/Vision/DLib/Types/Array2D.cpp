
#include <string>

#include <dlib/image_processing/frontal_face_detector.h>

#include <dlib/image_processing.h>

#include <dlib/image_io.h>

#include <iostream>

using namespace dlib;

using namespace std;

typedef array2d<rgb_pixel> image;

extern "C" {
image * inline_c_Vision_DLib_Types_Array2D_0_bb636f286b2b9c31fc83bf0baedfaf91d072770e() {
return ( new array2d<rgb_pixel>() );
}

}

extern "C" {
void inline_c_Vision_DLib_Types_Array2D_1_068251d466471b028af3d96c570b88e1ad45bbc4(image * img_inline_c_0, char * bs_inline_c_1) {

    load_image(*img_inline_c_0, bs_inline_c_1);
  
}

}

extern "C" {
image * inline_c_Vision_DLib_Types_Array2D_2_1d59673a9c092972b32a6d110a6ad29d24367d50(image * img_inline_c_0) {

    array2d<rgb_pixel> * img = img_inline_c_0;
    pyramid_up(*img);
    return img;

}

}
