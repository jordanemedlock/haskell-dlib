
#include <dlib/image_processing/frontal_face_detector.h>

using namespace dlib;

extern "C" {
void * inline_c_Vision_DLib_Algorithms_ObjectDetection_0_eda033fa90b23e4b9d93f298f438800a0ecd93c8() {
return ( new frontal_face_detector(get_frontal_face_detector()) );
}

}

extern "C" {
void inline_c_Vision_DLib_Algorithms_ObjectDetection_1_36b42ded9d76114d8661491c8e468a3566432c17(void * det_inline_c_0, void * img_inline_c_1, int * intPtr_inline_c_2, void ** dblPtr_inline_c_3) {

    frontal_face_detector * det = (frontal_face_detector *)det_inline_c_0;
    array2d<rgb_pixel> * img = (array2d<rgb_pixel> *)img_inline_c_1;
    std::vector<rectangle> rects = (* det)(* img);
    (*intPtr_inline_c_2) = rects.size();
    (*dblPtr_inline_c_3) = &rects[0];
  
}

}
