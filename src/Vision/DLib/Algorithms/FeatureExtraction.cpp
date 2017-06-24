
#include <dlib/image_processing.h>

#include "typedefs.h"

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
full_object_detection * inline_c_Vision_DLib_Algorithms_FeatureExtraction_2_1d37791a0e3894bbb023c6bb18d87941102837db(void * sp_inline_c_0, image * img_inline_c_1, void * voidPtr_inline_c_2) {

      full_object_detection * det = new full_object_detection();
      *det = (*(shape_predictor *)sp_inline_c_0)(*img_inline_c_1, *(rectangle *)voidPtr_inline_c_2);
      return det;
    
}

}
