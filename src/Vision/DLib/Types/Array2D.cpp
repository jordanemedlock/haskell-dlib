
#include <string>

#include <dlib/image_processing/frontal_face_detector.h>

#include <dlib/image_processing.h>

#include <dlib/image_io.h>

#include <iostream>

#include "typedefs.h"

using namespace dlib;

using namespace std;

extern "C" {
image * inline_c_Vision_DLib_Types_Array2D_0_bb636f286b2b9c31fc83bf0baedfaf91d072770e() {
return ( new array2d<rgb_pixel>() );
}

}

extern "C" {
image * inline_c_Vision_DLib_Types_Array2D_1_355199e47a136d7d45d59571058aaaacf06213b4(image * img_inline_c_0) {

    array2d<rgb_pixel> * img = img_inline_c_0;
    pyramid_up(*img);
    return img;

}

}
