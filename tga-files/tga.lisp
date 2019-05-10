;;  0 - 1a                ID Length (26 bytes)
;;  1 - 00                Color Map Type
;;  2 - 0a                Image Type
;;  3 - 00 00 00 00 00    Color Map Specification
;;  8 - 00 00             X-Origin
;; 10 - 00 00             Y-Origin
;; 12 - 80 00             Width (128 pixels)
;; 14 - 80 00             Height (128 pixels)
;; 16 - 10                Pixel Depth (16 bit)
;; 17 - 01                Image Descriptor (2 bits unused, 2 bits image origin, 4 bits alpha channels)
;; 18 - 54 72 75 65 76
;; 23 - 69 73 69 6f 6e    Image ID
;; 28 - 28 52 29 20 53    Length Above
;; 33 - 61 6d 70 6c 65    ASCII
;; 38 - 20 49 6d 61 67
;; 43 - 65
;;    -                   Color Map Data, nothing if Color Map Type == 0
;; 44 - 8700 7c87 e003
;;    - 871f 0087 0000 8700 7c87 e003 871f
;;    - 0087 ff7f 8700 7c87 e003 871f 0087 0000
;;    - 8700 7c87 e003 871f 0087 ff7f 8700 7c87

(defun write-tga (s)
  ; ID Length (00-ff), 1 Byte
  (write-byte #x00 s)
  ; Color Map Type (0, 1) 1 Byte
  (write-byte #x00 s)
  ; Image Type 00 = None
  ;            01 = Uncompressed, Color-Mapped
  ;            02 = Uncompressed, True-Color
  ;            03 = Uncompressed, B&W
  ;            09 = Run-Length Encoded, Color-Mapped
  ;            0a = Run-Length Encoded, True-Color
  ;            0b = Run-Length Encoded, B&W
  (write-byte #x03 s)
  ; Color Map Specification, 5 Bytes
  ;    Required, but all 0 if no color map
  (write-sequence '(#x00 #x00 #x00 #x00 #x00) s)
  ; Image Specification Field, 10 Bytes
  ;    X-Origin of Image, 2 Bytes
  (write-sequence '(#x00 #x00) s)
  ;    Y-Origin of Image, 2 Bytes
  (write-sequence '(#x00 #x00) s)
  ;    Width of Image, 2 Bytes (10 pixels)
  (write-sequence '(#x0a #x00) s)
  ;    Height of Image, 2 Bytes (10 pixels)
  (write-sequence '(#x0a #x00) s)
  ;    Pixel Depth (16 bit)
  (write-byte #x10 s)
  ;    Image Descriptor
  (write-byte #x00 s)
