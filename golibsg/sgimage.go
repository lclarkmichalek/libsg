package golibsg

/*
#include <stdlib.h>
#include <sg/sgimage.h>
#cgo LDFLAGS: -lsg
*/
import "C"

import (
	"unsafe"
	"runtime"
	"reflect"
)

type SgImage struct {
	ptr unsafe.Pointer

	// Keep the file alive, as img is collected in it's dtor
	parent *SgFile
}

func createImage(ptr *C.struct_SgImage, parent *SgFile) *SgImage {
	img := &SgImage{unsafe.Pointer(ptr), parent}
	return img
}

func (i *SgImage) Width() uint16 {
	return uint16(C.sg_get_image_width((*C.struct_SgImage)(i.ptr)))
}

func (i *SgImage) Height() uint16 {
	return uint16(C.sg_get_image_height((*C.struct_SgImage)(i.ptr)))
}

func (i *SgImage) Type() uint16 {
	return uint16(C.sg_get_image_type((*C.struct_SgImage)(i.ptr)))
}

func (i *SgImage) IsExtern() bool {
	return bool(C.sg_get_image_extern((*C.struct_SgImage)(i.ptr)))
}

func (i *SgImage) Parent() *SgBitmap {
	return createBitmap(C.sg_get_image_parent((*C.struct_SgImage)(i.ptr)), i.parent)
}

type SgImageData struct {
	ptr unsafe.Pointer

	Width, Height int
	RMask, GMask, BMask, AMask int
	Data []byte
}

func (i *SgImage) LoadData(filename string) *SgImageData {
	cFilename := C.CString(filename)
	defer C.free(unsafe.Pointer(cFilename))

	cData := C.sg_load_image_data((*C.struct_SgImage)(i.ptr), cFilename)
	imgData := &SgImageData{
		unsafe.Pointer(cData),
		int(cData.width),
		int(cData.height),
		int(cData.rMask),
		int(cData.gMask),
		int(cData.bMask),
		int(cData.aMask),
		[]byte{}}
	
	header := (*reflect.SliceHeader)(unsafe.Pointer(&imgData.Data))
	header.Data = uintptr(unsafe.Pointer(cData.data))
	header.Len = int(cData.width * cData.height) * 4
	header.Cap = header.Len

	runtime.SetFinalizer(imgData, func (d *SgImageData) {
		// This will free the memory backing the Data slice
		C.sg_delete_image_data((*C.struct_SgImageData)(imgData.ptr))
	})

	return imgData
}