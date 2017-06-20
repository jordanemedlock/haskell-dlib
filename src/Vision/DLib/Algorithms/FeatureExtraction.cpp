
#include <dlib/image_processing.h>

using namespace dlib;

extern "C" {
void * inline_c_Vision_DLib_Algorithms_FeatureExtraction_0_c8be607a6edae506e43948c587e12bb80d671f2c(void * ptr_inline_c_0) {
return ( &((full_object_detection *)ptr_inline_c_0)->get_rect() );
}

}

extern "C" {
long inline_c_Vision_DLib_Algorithms_FeatureExtraction_1_8ad875683cf960f30feaea0f7a9d9afa4f6268bb(void * ptr_inline_c_0) {
return ( ((full_object_detection *)ptr_inline_c_0)->num_parts() );
}

}

extern "C" {
point * inline_c_Vision_DLib_Algorithms_FeatureExtraction_2_62aeded455786d7a35c9ab6bb36586d32d1e7ffe(void * ptr_inline_c_0, long idx_inline_c_1) {
return ( &((full_object_detection *)ptr_inline_c_0)->part(idx_inline_c_1) );
}

}

extern "C" {
void * inline_c_Vision_DLib_Algorithms_FeatureExtraction_3_879ac73ae50771ac41bbb1dbcdc2335277ae38a0() {
return ( new shape_predictor() );
}

}

extern "C" {
void inline_c_Vision_DLib_Algorithms_FeatureExtraction_4_a2ee231108ce9bec1075c93e5b5ac1b668744649(char * bs_inline_c_0, void * sp_inline_c_1) {
 deserialize(bs_inline_c_0) >> *((shape_predictor *)sp_inline_c_1); 
}

}

extern "C" {
void * inline_c_Vision_DLib_Algorithms_FeatureExtraction_5_85144a92d621a89f7953038e9d447ef9285427db(void * sp_inline_c_0, void * img_inline_c_1, void * voidPtr_inline_c_2) {

      full_object_detection * det = new full_object_detection();
      *det = (*(shape_predictor *)sp_inline_c_0)(*(array2d<rgb_pixel> *)img_inline_c_1, *(rectangle *)voidPtr_inline_c_2);
      return det;
    
}

}
