
#include <string>

#include <dlib/image_processing/frontal_face_detector.h>

#include <dlib/image_processing.h>

#include <dlib/image_io.h>

#include <iostream>

#include "typedefs.h"

extern "C" {
void inline_c_Vision_DLib_IO_0_a740cb1d54217ee48d12d4029e03281e76a70b8c(image * img_inline_c_0, char * bs_inline_c_1) {

    load_image(*img_inline_c_0, bs_inline_c_1);
  
}

}

extern "C" {
void inline_c_Vision_DLib_IO_1_32b8041e8cf795639d649c917e1e5f6ef9d26759(image * img_inline_c_0, char * bs_inline_c_1) {

    load_bmp(*img_inline_c_0, bs_inline_c_1);
  
}

}

extern "C" {
void inline_c_Vision_DLib_IO_2_e43f5f6a0ea06f172ce8df45195ce9ec1cecb573(image * img_inline_c_0, char * bs_inline_c_1) {

    load_dng(*img_inline_c_0, bs_inline_c_1);
  
}

}

extern "C" {
void inline_c_Vision_DLib_IO_3_66b940dca7472d619a47fd5b5796a4c27b2ae76c(image * img_inline_c_0, char * bs_inline_c_1) {

    load_jpeg(*img_inline_c_0, bs_inline_c_1);
  
}

}

extern "C" {
void inline_c_Vision_DLib_IO_4_1e9bc1d78e64c540307df37eb7d0ccc7b0f2e5ff(image * img_inline_c_0, char * bs_inline_c_1) {

    load_png(*img_inline_c_0, bs_inline_c_1);
  
}

}

extern "C" {
void inline_c_Vision_DLib_IO_5_83a5f72d01b0a25e3dc11c6220629dca686c3732(image * img_inline_c_0, char * bs_inline_c_1) {

    save_bmp(*img_inline_c_0, bs_inline_c_1); 
  
}

}

extern "C" {
void inline_c_Vision_DLib_IO_6_e731279b18ce8e316a90b538185e42af44554576(image * img_inline_c_0, char * bs_inline_c_1) {

    save_dng(*img_inline_c_0, bs_inline_c_1); 
  
}

}

extern "C" {
void inline_c_Vision_DLib_IO_7_c2f09924e569d62a63dd3b2ca037993cba611aff(image * img_inline_c_0, char * bs_inline_c_1) {

    save_jpeg(*img_inline_c_0, bs_inline_c_1); 
  
}

}

extern "C" {
void inline_c_Vision_DLib_IO_8_bff560c7fb7905ed3c046d1ec26fdd418064789b(image * img_inline_c_0, char * bs_inline_c_1) {

    save_png(*img_inline_c_0, bs_inline_c_1); 
  
}

}
