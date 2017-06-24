
#include <dlib/image_processing.h>

using namespace dlib;

extern "C" {
rectangle * inline_c_Vision_DLib_Types_Shape_0_104f19ab974a720f1129c1b0176220f40e081db8(full_object_detection * ptr_inline_c_0) {
return ( &ptr_inline_c_0->get_rect() );
}

}

extern "C" {
long inline_c_Vision_DLib_Types_Shape_1_c89d5dd1d7a99a4ee842113e267b93c77b3196f5(full_object_detection * ptr_inline_c_0) {
return ( ptr_inline_c_0->num_parts() );
}

}

extern "C" {
void inline_c_Vision_DLib_Types_Shape_2_71f54ec90954d8599672150ce859c5b4cb0558aa(point * elemPtr_inline_c_0, full_object_detection * ptr_inline_c_1, long i_inline_c_2) {

          *elemPtr_inline_c_0 = ptr_inline_c_1->part(i_inline_c_2);
        
}

}

extern "C" {
full_object_detection * inline_c_Vision_DLib_Types_Shape_3_b8ae5b6cff2175929b1cfc9e39b0b923c93b2172(point * arrPtr_inline_c_0, point * arrPtr_inline_c_1, long arrLen_inline_c_2, rectangle * rectPtr_inline_c_3) {

          rectangle rect;
          std::vector<point> points(arrPtr_inline_c_0, arrPtr_inline_c_1 + arrLen_inline_c_2);
          return new full_object_detection(
            *rectPtr_inline_c_3,
            points
          );
        
}

}

extern "C" {
void inline_c_Vision_DLib_Types_Shape_4_c1dbde5bf8e9b8a2397c588ad78a3b14fb8f7631(full_object_detection * ptr_inline_c_0) {
 delete ptr_inline_c_0; 
}

}