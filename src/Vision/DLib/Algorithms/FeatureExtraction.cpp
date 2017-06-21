
#include <dlib/image_processing.h>

using namespace dlib;

extern "C" {
void * inline_c_Vision_DLib_Algorithms_FeatureExtraction_0_879ac73ae50771ac41bbb1dbcdc2335277ae38a0() {
return ( new shape_predictor() );
}

}

extern "C" {
void inline_c_Vision_DLib_Algorithms_FeatureExtraction_1_a2ee231108ce9bec1075c93e5b5ac1b668744649(char * bs_inline_c_0, void * sp_inline_c_1) {
 deserialize(bs_inline_c_0) >> *((shape_predictor *)sp_inline_c_1); 
}

}

extern "C" {
void inline_c_Vision_DLib_Algorithms_FeatureExtraction_2_53c53980b5b41dfd483717aab29ae0009d370796(void * sp_inline_c_0, void * img_inline_c_1, void * voidPtr_inline_c_2) {

      full_object_detection * det = new full_object_detection();
      *det = (*(shape_predictor *)sp_inline_c_0)(*(array2d<rgb_pixel> *)img_inline_c_1, *(rectangle *)voidPtr_inline_c_2);

    
}

}

extern "C" {
full_object_detection * inline_c_Vision_DLib_Algorithms_FeatureExtraction_3_a8dc9565f664e3cc09717b3fa802eb8e7df4b3f1() {
return ( new full_object_detection() );
}

}

extern "C" {
void inline_c_Vision_DLib_Algorithms_FeatureExtraction_4_c1dbde5bf8e9b8a2397c588ad78a3b14fb8f7631(full_object_detection * ptr_inline_c_0) {
 delete ptr_inline_c_0; 
}

}
