package golibsg

/*
#include <stdlib.h>
#include <sg/sgfile.h>
#cgo LDFLAGS: -lsg
*/
import "C"

import (
	"unsafe"
	"runtime"
	"fmt"
)

type SgFile struct {
	ptr unsafe.Pointer
}

func createFile(ptr *C.struct_SgFile) *SgFile {
	file := &SgFile{unsafe.Pointer(ptr)}
	runtime.SetFinalizer(file, func (f *SgFile) {
		C.sg_delete_file((*C.struct_SgFile)(f.ptr))
	})
	return file
}

/*
 Reads the sg3 file.
*/
func ReadFile(filename string) (*SgFile, error) {
	cStr := C.CString(filename)
	defer C.free(unsafe.Pointer(cStr))

	ptr := C.sg_read_file(cStr)
	if ptr == nil {
		return nil, fmt.Errorf("Could not load file")
	}
	return createFile(ptr), nil
}

// The sg(\d) file's version. 0xd3 is sg2, 0xd4/0xd5 is sg3. Other values are
// not "supported"
func (f *SgFile) Version() uint32 {
	return uint32(C.sg_get_file_version((*C.struct_SgFile)(f.ptr)))
}

// The size of the internal 555 + the external 555 file
func (f *SgFile) TotalFilesize() uint32 {
	return uint32(C.sg_get_file_total_filesize((*C.struct_SgFile)(f.ptr)))
}

// The size of the internal 555 file
func (f *SgFile) Filesize555() uint32 {
	return uint32(C.sg_get_file_555_filesize((*C.struct_SgFile)(f.ptr)))
}

// The size of the external 555 file
func (f *SgFile) FilesizeExternal() uint32 {
	return uint32(C.sg_get_file_external_filesize((*C.struct_SgFile)(f.ptr)))
}

// A slice of the bitmaps. Changes to the slice do not change the SgFile.
func (f *SgFile) Bitmaps() ([]*SgBitmap, error) {
	n := int(C.sg_get_file_bitmap_count((*C.struct_SgFile)(f.ptr)))
	bmps := make([]*SgBitmap, n)
	for i := 0; i < n; i++ {
		bmp := C.sg_get_file_bitmap((*C.struct_SgFile)(f.ptr), C.int(i))
		if bmp == nil {
			return []*SgBitmap{}, fmt.Errorf("Bitmap %v was nil", i)
		}
		bmps[i] = createBitmap(bmp, f)
	}
	return bmps, nil
}

// Slice of all images in the file. Changes to the slice do not change the SgFile.
func (f *SgFile) Images() ([]*SgImage, error) {
	n := int(C.sg_get_file_image_count((*C.struct_SgImage)(f.ptr)))
	imgs := make([]*SgImage, n)
	for i := 0; i < n; i++ {
		img := C.sg_get_file_image((*C.struct_SgImage)(f.ptr), C.int(i))
		if img == nil {
			return []*SgImage{}, fmt.Errorf("Image %v was nil", i)
		}
		imgs[i] = createImage(img, f)
	}
	return imgs, nil
}