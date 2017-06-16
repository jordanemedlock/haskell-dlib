
#include <string>

#include <dlib/image_processing/frontal_face_detector.h>

#include <dlib/image_processing.h>

#include <dlib/image_io.h>

#include <iostream>

using namespace dlib;

using namespace std;

extern "C" {
void * inline_c_Vision_DLib_Types_Array2D_0_bfce9d6facfd323e49e0e5bdfb602a8f5d1c78bc() {
return ( new array2d<rgb_pixel>() );
}

}

extern "C" {
void inline_c_Vision_DLib_Types_Array2D_1_b1770b451f1da53abdffdb114a698247d53e8c40(void * img_inline_c_0, char * bs_inline_c_1) {

    load_image(*(array2d<rgb_pixel> *)img_inline_c_0, bs_inline_c_1);
  
}

}

extern "C" {
void * inline_c_Vision_DLib_Types_Array2D_2_c6c787264a1da6ca89359ccbac69d53b9c4c7ec6(void * img_inline_c_0) {

    array2d<rgb_pixel> * img = (array2d<rgb_pixel> *)img_inline_c_0;
    pyramid_up(*img);
    return img;

}

}
