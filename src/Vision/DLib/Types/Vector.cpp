
#include <dlib/geometry.h>

#include <iostream>

using namespace dlib;

extern "C" {
point * inline_c_Vision_DLib_Types_Vector_0_8faad0266bd4a9cb525c92abe5bb6cb6f0af5466() {
return ( new point() );
}

}

extern "C" {
void inline_c_Vision_DLib_Types_Vector_1_eea5d7e4f2f5c2c367614eb6f1abcd792600df11(point * ptr_inline_c_0) {
 delete ptr_inline_c_0; 
}

}

extern "C" {
long inline_c_Vision_DLib_Types_Vector_2_0fcc570750a68145a00ae91dcc60a48a1a7f21f0(point * ptr_inline_c_0) {
return ( ptr_inline_c_0->x() );
}

}

extern "C" {
long inline_c_Vision_DLib_Types_Vector_3_236ef93b70b73234cb174191d2479d1fec12f277(point * ptr_inline_c_0) {
return ( ptr_inline_c_0->y() );
}

}

extern "C" {
void inline_c_Vision_DLib_Types_Vector_4_e8a89df3373df7490be70fa3382d98519b027eb8(point * ptr_inline_c_0, long x_inline_c_1, point * ptr_inline_c_2, long y_inline_c_3) {

        ptr_inline_c_0->x() = x_inline_c_1;         
        ptr_inline_c_2->y() = y_inline_c_3;
      
}

}

extern "C" {
long inline_c_Vision_DLib_Types_Vector_5_21d0deb62b12fd984a5313034cd73e60f2301dcf() {
return ( sizeof(dlib::point) );
}

}

extern "C" {
long inline_c_Vision_DLib_Types_Vector_6_e446a29f3d2c8cddd1314c48b9f487536e3844f6() {
return ( alignof(dlib::point) );
}

}
