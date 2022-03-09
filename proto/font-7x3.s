;; ---------------------------
;; Using the hex_font_table, build a 256 8x8 character ram at the location
;; found at zero page pointer passed in X.
;; Modifies A, X, Y;
;; ---------------------------

compressed_hex_font_table:
hex80:
	.byte %00000000
	.byte %01100110
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %11101010
	.byte %10101010
	.byte %11101110
	.byte %00000000

hex91:
	.byte %00000000
	.byte %01101100
	.byte %10010100
	.byte %10010100
	.byte %10010100
	.byte %10110100
	.byte %00010100
	.byte %00010100
	.byte %00000000

hexA2:
	.byte %00000000
	.byte %01101100
	.byte %10100010
	.byte %10100010
	.byte %10100010
	.byte %11100100
	.byte %10101000
	.byte %10101110
	.byte %00000000

hexB3:
	.byte %00000000
	.byte %01101110
	.byte %10100010
	.byte %10100010
	.byte %10100010
	.byte %11001110
	.byte %10100010
	.byte %11101110
	.byte %00000000

hexC4:
	.byte %00000000
	.byte %01101010
	.byte %10001010
	.byte %10001010
	.byte %10001010
	.byte %10001110
	.byte %10000010
	.byte %11100010
	.byte %00000000

hexD5:
	.byte %00000000
	.byte %11001110
	.byte %10101000
	.byte %10101000
	.byte %10101000
	.byte %10101110
	.byte %10100010
	.byte %11101100
	.byte %00000000

hexE6:
	.byte %00000000
	.byte %01100110
	.byte %10001000
	.byte %10001000
	.byte %10001000
	.byte %11101110
	.byte %10001010
	.byte %11101110
	.byte %00000000

hexF7:
	.byte %00000000
	.byte %01101110
	.byte %10000010
	.byte %10000010
	.byte %10000010
	.byte %11100010
	.byte %10000010
	.byte %10000010
	.byte %00000000

