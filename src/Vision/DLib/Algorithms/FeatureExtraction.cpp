
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
full_object_detection * inline_c_Vision_DLib_Algorithms_FeatureExtraction_2_5b8b322825ff9ff6841925b3012a82c6cfb4a1c6(void * sp_inline_c_0, void * img_inline_c_1, void * voidPtr_inline_c_2) {

      full_object_detection * det = new full_object_detection();
      *det = (*(shape_predictor *)sp_inline_c_0)(*(array2d<rgb_pixel> *)img_inline_c_1, *(rectangle *)voidPtr_inline_c_2);
      return det;
    
}

}

extern "C" {
full_object_detection * inline_c_Vision_DLib_Algorithms_FeatureExtraction_3_7fa042e91f5cb89b6bc99729e41c64049776caec(point * arrPtr_inline_c_0, point * arrPtr_inline_c_1, long arrLen_inline_c_2, rectangle * rectPtr_inline_c_3) {

          rectangle rect();
          std::vector<point> points(arrPtr_inline_c_0, arrPtr_inline_c_1 + arrLen_inline_c_2);
          return new full_object_detection(
            *rectPtr_inline_c_3,
            points
          ); // TODO: implement
        
}

}

extern "C" {
void inline_c_Vision_DLib_Algorithms_FeatureExtraction_4_c1dbde5bf8e9b8a2397c588ad78a3b14fb8f7631(full_object_detection * ptr_inline_c_0) {
 delete ptr_inline_c_0; 
}

}
