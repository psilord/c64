;; This library is for prototyping and exploring ideas on a C64 while learning
;; the platform from scratch. It is not optimized for games or demos and
;; doesn't demonstrate good techniques for 6502 ASM or C64 ASM coding.
;; This may be corrected in the future.

;;; Constants

;; VIC-II Constants: C64PRG: p102
; Video Bank Identification (sets video bank start)
VIC_VBankLoc_Mask = #$03		; Bits 1-0
VIC_VBankLoc_0 = #$03			; $0000 - $3fff
VIC_VBankLoc_1 = #$02   		; $4000 - $7fff
VIC_VBankLoc_2 = #$01			; $8000 - $bfff
VIC_VBankLoc_3 = #$00   		; $C000 - $ffff (default value)
; Screen Memory Location (as an offset from the Video Bank start)
VIC_ScrMemLoc_Mask = #$f0		; Bits 7-4
VIC_ScrMemLoc_0 = #$00			; $0000
VIC_ScrMemLoc_1 = #$10			; $0400 (default)
VIC_ScrMemLoc_2 = #$20			; $0800
VIC_ScrMemLoc_3 = #$30			; $0c00
VIC_ScrMemLoc_4 = #$40			; $1000
VIC_ScrMemLoc_5 = #$50			; $1400
VIC_ScrMemLoc_6 = #$60			; $1800
VIC_ScrMemLoc_7 = #$70			; $1C00
VIC_ScrMemLoc_8 = #$80			; $2000
VIC_ScrMemLoc_9 = #$90			; $2400
VIC_ScrMemLoc_a = #$a0			; $2800
VIC_ScrMemLoc_b = #$b0			; $2C00
VIC_ScrMemLoc_c = #$c0			; $3000
VIC_ScrMemLoc_d = #$d0			; $3400
VIC_ScrMemLoc_e = #$e0			; $3800
VIC_ScrMemLoc_f = #$f0			; $3c00
;; Character Memory Location (as an offset from the Video Bank start)
VIC_CharMemLoc_Mask = #$0e		; Bits 3-1
VIC_CharMemLoc_0 = #$00			; $0000 - $07ff
VIC_CharMemLoc_1 = #$02			; $0800 - $0fff
VIC_CharMemLoc_2 = #$04			; $1000 - $17ff (Rom Image Bank 0 & 2 block 0 Upper)
VIC_CharMemLoc_3 = #$06			; $1800 - $1fff (Rom Image Bank 0 & 2 block 1 Lower)
VIC_CharMemLoc_4 = #$08			; $2000 - $27ff
VIC_CharMemLoc_5 = #$0a			; $2800 - $2fff
VIC_CharMemLoc_6 = #$0c			; $3000 - $37ff
VIC_CharMemLoc_7 = #$0e			; $3800 - $3fff
;; VIC Standard/Multi Color Mode Constants
VIC_Color_Mode_Mask = #$10
VIC_Color_Mode_Standard = #$00
VIC_Color_Mode_Multi = #$10
; The mask in the color memory for multi-color mode
VIC_Color_Mode_Char_Mask = #$08
VIC_Color_Mode_Char_Enable = #$08
VIC_Color_Mode_Char_Disable = #$08
;; VIC Extended Color Mode Constants
VIC_Extended_Color_Mode_Mask = #$40
VIC_Extended_Color_Mode_Disable = #$00
VIC_Extended_Color_Mode_Enable = #$40
;; VIC Bitmap Mode Constants
VIC_Bitmap_Mode_Mask = #$20
VIC_Bitmap_Mode_Disable = #$00
VIC_Bitmap_Mode_Enable = #$20
;; VIC 38/40 Column Mode Constants
VIC_Column_Mode_Mask = #$08
VIC_Column_38_Mode = #$00
VIC_Column_40_Mode = #$08
;; VIC 24/25 Row Mode Constants
VIC_Row_Mode_Mask = #$08
VIC_Row_24_Mode = #$00
VIC_Row_25_Mode = #$08
;; VIC Hardware Scrolling
VIC_Scroll_X_Mask = #$07
VIC_Scroll_Y_Mask = #$07

;; Regular Color Names
Black = #$00
White = #$01
Red = #$02
Cyan = #$03
Purple = #$04
Green = #$05
Blue = #$06
Yellow = #$07
Orange = #$08
Brown = #$09
LightRed = #$0a
Gray1 = #$0b
Grey1 = Gray1
Gray2 = #$0c
Grey2 = Gray2
LightGreen = #$0d
LightBlue = #$0e
Gray3 = #$0f
Grey3 = Gray3
;; Color names that include the Multicolor bit for multi-color character mode.
MC_Black = #$08
MC_White = #$09
MC_Red = #$0a
MC_Cyan = #$0b
MC_Purple = #$0c
MC_Green = #$0d
MC_Blue = #$0e
MC_Yellow = #$0f


;; CIA1 Constants
; Manipulation of Timer A control register
CIA1_TimerA_Timer_Mask = #$01	; Bit 0
CIA1_TimerA_Timer_Disable = #$00
CIA1_TimerA_Timer_Enable = #$01

;; 6510 Processor Port Constants
PP_RAM_Mask = #$07
PP_BASIC_Mask = #$01
PP_BASIC_Enable = #$01
PP_BASIC_Disable = #$00
PP_Kernel_Mask = #$02
PP_Kernel_Enable = #$02
PP_Kernel_Disable = #$00
PP_Char_Mask = #$04
PP_Char_Enable = #$00
PP_Char_Disable = #$04

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sprite_row: .macro
	;; 24 bit number comes in a little endian
	.byte \1[16:24], \1[8:16], \1[0:8]
.endm

end_sprite: .macro
	.byte $00
.endm

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zero Page memory locations
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mem_src = $fb
mem_dst = $fd

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

*	= $0800
basic_auto_run:
    .byte $00                ; first byte has to be zero for BASIC program to work
    .byte $0c, $08           ; pointer to next BASIC line in memory, i.e. $080C
    .byte $0a, $00, $9e, $20 ; the code for: 10 SYS (includes trailing space)
    .byte $32, $30, $36, $32 ; ASCII version of SYS address, i.e. 2062
    .byte $00                ; line terminator
    .byte $00, $00           ; the pointer above points to this place in memory
                             ; as we have no other line, we leave both bytes zero
                             ; end of program

    ; memory location 2062 comes next, which our SYS command above calls
    ; (i.e. 2048 + 14 bytes for our BASIC program above)

start:
	jsr reset_c64

	;jsr TEST_vic_get_video_bank_loc 		; C UL corner
	;jsr TEST_vic_set_video_bank_loc		; A UL corner
	;jsr TEST_vic_get_scr_mem_loc			; P UL corner
	;jsr TEST_vic_set_scr_mem_loc			; A UL corner
	;jsr TEST_vic_get_char_mem_loc			; B UL corner
	;jsr TEST_vic_set_char_mem_loc			; smiley face first row of text
	;jsr TEST_cia1_timer_a_get_timer		; A UL corner
	;jsr TEST_cia1_timer_a_set_timer		; AB UL corner
	;jsr TEST_pp_get_ram_state				; $07 UL corner
	;jsr TEST_pp_set_ram_state				; $04 UL corner
	;jsr TEST_pp_get_ram_basic_state		; $01 UL corner
	;jsr TEST_pp_set_ram_basic_state		; $00 UL corner
	;jsr TEST_pp_get_ram_kernel_state		; $02 UL corner
	;jsr TEST_pp_set_ram_kernel_state		; $00 UL corner
	;jsr TEST_pp_get_ram_char_state			; $04 UL corner
	;jsr TEST_pp_set_ram_char_state			; $00 UL corner
	;jsr TEST_vic_get_color_mode_state		; $00 UL Corner
	;jsr TEST_vic_set_color_mode_state		; $10 UL Corner
	;jsr TEST_vic_multi_color_display		; A specific row of output glyphs
	;jsr TEST_vic_get_extended_color_mode_state	; $00 UL Corner
	;jsr TEST_vic_set_extended_color_mode_state	; $40 UL Corner
	;jsr TEST_vic_extended_color_display		; A specific set of A's
	;jsr TEST_vic_get_bitmap_mode_state		; $00 UL Corner
	;jsr TEST_vic_set_bitmap_mode_state		; $20 UL Corner
	;jsr TEST_vic_bitmap_display			; A specific image
	;jsr TEST_vic_multi_color_bitmap_display			; A specific image
	;jsr TEST_vic_get_column_mode_state		; $08 UL Corner
	;jsr TEST_vic_set_column_mode_state		; $00 UL Corner
	;jsr TEST_vic_get_row_mode_state		; $08 UL Corner
	;jsr TEST_vic_set_column_mode_state		; $00 UL Corner
	;jsr TEST_vic_get_scroll_x				; $00 UL Corner
	;jsr TEST_vic_set_scroll_x				; $03 UL Corner
	;jsr TEST_vic_scroll_x_bitmap_display	; A specific image
	;jsr TEST_vic_get_scroll_y				; $03 UL Corner
	;jsr TEST_vic_set_scroll_y				; $05 UL Corner
	;jsr TEST_vic_scroll_y_bitmap_display	; A specific image

	lda #$00
	sta mem_dst
	lda #$04
	sta mem_dst+1
	lda #$01
	ldx #$e8
	ldy #$03
	jsr memset

exit:
	rts


;; ---------------------------
;; Utils
;; ---------------------------

hang_c64: .proc
	jmp hang_c64
.pend

reset_c64: .proc
	jsr reset_processor_port
	jsr reset_video_memory
	rts
.pend

reset_processor_port: .proc
	lda PP_Kernel_Enable
	ora PP_Char_Disable
	ora PP_BASIC_Enable
	jsr pp_set_ram_state
	rts
.pend

reset_video_memory: .proc
	lda VIC_VBankLoc_0
	jsr vic_set_video_bank_loc
	lda VIC_ScrMemLoc_1
	jsr vic_set_scr_mem_loc
	lda VIC_CharMemLoc_2 ;; Character set 1 (upper case)
	jsr vic_set_char_mem_loc
	lda VIC_Color_Mode_Char_Disable
	jsr vic_set_color_mode_state
	lda VIC_Bitmap_Mode_Disable
	jsr vic_set_bitmap_mode_state
	lda #$20 ;; Space Character in character set 1
	jsr vic_clear_default_screen
	lda VIC_Column_40_Mode
	jsr vic_set_column_mode_state
	lda VIC_Row_25_Mode
	jsr vic_set_row_mode_state
	lda #$00
	jsr vic_set_scroll_x
	lda #$03
	jsr vic_set_scroll_y
	rts
.pend

;; ---------------------------
;; Function: memset
;; Fills memory with a value
;;
;; Store the destination address at: mem_dst (low byte), memdst+1 (high byte)
;; A is the value we'd like to store in the memory. 
;; X is the low byte of the number of bytes to store
;; Y is the high byte of the number of bytes to store
;;
;; Input Registers: A, X, Y
;; Return Value: None
;; Destroys: A, X, Y
;; ---------------------------
;; This is an unoptimized way to memset, but general in its use
;; and convenient in allowing me to prototype. The code is self
;; modifying to simplify it.
;; Inputs:
;; A contains value to set into each byte.
;; X contains low byte of length
;; Y contains high byte of length.
;; if $YX takes you past end of memory, will wrap.
;; mem_dst, mem_dst+1, the 2 byte ZP pointer to start the memset.
;;
;; Destroys: A, X, Y.
memset: .proc
	;; Save off the value we're copying
	sta tmp_val

	;; Save off the number of bytes we're needing to copy
	stx tmp_num_bytes
	sty tmp_num_bytes+1

	;; Compute end address (allows roll-over in memory)
	clc
	lda mem_dst
	adc tmp_num_bytes
	sta tmp_end_addr
	lda mem_dst+1
	adc tmp_num_bytes+1
	sta tmp_end_addr+1

	;; Initialize storing location to where the user specified.
	lda mem_dst		;; lo byte
	sta store_loc+1
	lda mem_dst+1	;; hi byte
	sta store_loc+2

loop:
	;; 1. Check if done
	lda store_loc+2 ;; check hi byte first
	cmp tmp_end_addr+1
	bne fill_byte
check_lo_byte:
	lda store_loc+1
	cmp tmp_end_addr
	beq done

	;; 2. Do a copy (self modifyng code)
fill_byte:
	lda tmp_val
store_loc:
	;; store_loc+1 is addr lo byte, store_loc+2 is addr hi byte
	;; NOTE: can't use $0000 here cause it'll be a 2-byte instruction!
	sta $ffff

	;; 3. Increment storing address, from low bytes to high byte
	inc store_loc+1		;; incr lo byte
	beq inc_high_byte
	jmp loop
inc_high_byte:
	inc store_loc+2
	jmp loop

done:
	rts

tmp_val:
	.byte $00 ;; val to fill memory
tmp_num_bytes:
	.byte $00 ;; lo byte (X register)
	.byte $00 ;; hi byte (Y register)
tmp_end_addr:
	.byte $00 ;; lo byte
	.byte $00 ;; hi byte
.pend

;; ---------------------------
;; Function: vic_clear_default_screen
;; Clears the default screen to value passed in A.
;;
;; Input Registers: A
;; Return Value: None
;; Destroys: A, X
;; ---------------------------
vic_clear_default_screen: .proc
	ldx #$00
clear_loop:
	;; clear 250 characters in 4 strips, for 1000 characters in total.
	cpx #$fa 
	beq done
	sta $0400,x
	sta $04fa,x
	sta $05f4,x
	sta $06ee,x
	inx
	jmp clear_loop
done:
	rts
.pend

;; ---------------------------
;; Function: vic_get_video_bank_loc
;; Return which video bank is active.
;; C64PRG: p101-p102
;;
;; Input Registers: None
;; Return Value: A, a VBank_* constant
;;  Bank 0 for ($0000 - $3fff) (default value)
;;  Bank 1 for ($4000 - $7fff)
;;  Bank 2 for ($8000 - $bfff)
;;  Bank 3 for ($c000 - $ffff)
;; Destroys: A
;; ---------------------------

vic_get_video_bank_loc: .proc
	lda $dd00
	and VIC_VBankLoc_Mask
	rts
.pend

TEST_vic_get_video_bank_loc: .proc
	jsr reset_c64
	jsr vic_get_video_bank_loc
	sta $0400
	rts
.pend

;; ---------------------------
;; Function: cia1_enable_vbank_select
;; Set CIA#2 Port A to output
;; C64PRG: p101
;;
;; Input Registers: None
;; Return Value: None
;; Destroys: A
;; ---------------------------
cia1_enable_vbank_select: .proc
	lda $dd02 
	ora #$03
	sta $dd02
	rts
.pend

;; ---------------------------
;; Function: vic_set_video_bank_loc
;; Set VIC-II to specified video bank.
;; C64PRG: p101-p102
;;
;; Input Registers: A, a VBank_* constant
;;  Bank number to switch to in bits 0-1 of A.
;;  Bank 0 for ($0000 - $3fff)
;;  Bank 1 for ($4000 - $7fff)
;;  Bank 2 for ($8000 - $bfff)
;;  Bank 3 for ($c000 - $ffff)
;; Return Value: None
;; Destroys: A
;; ---------------------------

vic_set_video_bank_loc: .proc
	and VIC_VBankLoc_Mask
	sta tmp_0

	jsr cia1_enable_vbank_select

	lda $dd00 ;; load current bank bits register
	and ~VIC_VBankLoc_Mask ;; mask away the current bank bits, preserve other stuff
	ora tmp_0
	sta $dd00
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_set_video_bank_loc: .proc
	jsr reset_c64
	lda VIC_VBankLoc_2
	jsr vic_set_video_bank_loc
	lda #$01
	sta $8400 ;; VBank_2 @ $8000 + screen memory loc @ $0400
	rts
.pend

;; ---------------------------
;; Function: vic_get_scr_mem_loc
;; Return Screen Memory Location
;; C64PRG: p102
;;
;; Input Registers: None
;; Return Value: A, a ScrMem_* constant
;; Destroys: A
;; ---------------------------

vic_get_scr_mem_loc: .proc
	lda $d018
	and VIC_ScrMemLoc_Mask
	rts
.pend

TEST_vic_get_scr_mem_loc: .proc
	jsr reset_c64
	jsr vic_get_scr_mem_loc ;; default, so A is $f0, or the letter P in ROM
	sta $0400
	rts
.pend

;; ---------------------------
;; Function: vic_set_scr_mem_loc
;; Set Screen Memory Location
;; C64PRG: p102-p103
;;
;; Input Registers: A, a ScrMem_* constant
;; Return Value: None
;; Destroys: A
;; ---------------------------

vic_set_scr_mem_loc: .proc
	and VIC_ScrMemLoc_Mask
	sta tmp_0
	lda $d018
	and ~VIC_ScrMemLoc_Mask
	ora tmp_0
	sta $d018
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_set_scr_mem_loc: .proc
	jsr reset_c64
	lda VIC_ScrMemLoc_8
	jsr vic_set_scr_mem_loc
	lda #$01
	sta $2000 ;; VBank_0 @ $0000 + screen memory loc @ $2000
	rts
.pend


;; ---------------------------
;; Function: vic_get_char_mem_loc
;; Return Character Memory Location
;; C64PRG: p104
;;
;; Input Registers: None
;; Return Value: A, a CharMem_* constant
;; Destroys: A
;; ---------------------------

vic_get_char_mem_loc: .proc
	lda $d018
	and VIC_CharMemLoc_Mask
	rts
.pend

TEST_vic_get_char_mem_loc: .proc
	jsr reset_c64 
	jsr vic_get_char_mem_loc ;; default is VIC_CharMemLoc_2, so letter B after shift
	lsr a
	sta $0400
	rts
.pend


;; ---------------------------
;; Function: vic_set_char_mem_loc
;; Set Character Memory Location
;; C64PRG: p104
;;
;; Input Registers: A, a CharMem_* constant
;; Return Value: None
;; Destroys: A
;; ---------------------------

vic_set_char_mem_loc: .proc
	and VIC_CharMemLoc_Mask
	sta tmp_0
	lda $d018
	and ~VIC_CharMemLoc_Mask
	ora tmp_0
	sta $d018
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_set_char_mem_loc: .proc
	jsr reset_c64

	;; Crappily put a new char set character at char location 
	;; $2000 (one new character only)
	ldy #$00
	clc
byte_copy_loop:
	cpy #$08
	beq done_byte_copy
	lda smiley_face,y
	sta $2000,y
	iny
	jmp byte_copy_loop
done_byte_copy:
	lda VIC_CharMemLoc_4 ;; In VBank_0
	jsr vic_set_char_mem_loc
	;; now display the smiley face as the first row of text
	clc
	ldy #$00
one_row:
	cpy #40
	beq done
	lda #$00
	sta $0400,y
	iny
	jmp one_row
done:
	rts
smiley_face:
	.byte %00111100
	.byte %01000010
	.byte %10100101
	.byte %10000001
	.byte %10100101
	.byte %10011001
	.byte %01000010
	.byte %00111100
.pend

;; ---------------------------
;; Function: cia1_timer_a_get_timer
;; Read the CIA#1 Timer A Timer status bit
;; C64PRG: p106
;;
;; Input Registers: None
;; Return Value: A CIA1_TimerA_* constant
;; Destroys: A
;; ---------------------------
cia1_timer_a_get_timer: .proc
	lda $dc0e
	and CIA1_TimerA_Timer_Mask
	rts
.pend

TEST_cia1_timer_a_get_timer: .proc
	jsr reset_c64
	jsr cia1_timer_a_get_timer
	ldx #$00
	jsr TESTUtil_display_CIA1_TimerA_Timer_value
	rts
.pend


;; ---------------------------
;; Input registers:
;; A holds a CIA1_TimerA_Timer(On|Off) value
;; X holds offset from start of screen
;; ---------------------------
TESTUtil_display_CIA1_TimerA_Timer_value: .proc
	cmp CIA1_TimerA_Timer_Enable
	beq is_on
	cmp CIA1_TimerA_Timer_Disable
	beq is_off
	jmp is_wrong
is_on:
	lda #$01 ;; A
	jmp display_result
is_off:
	lda #$02 ;; B
	jmp display_result
is_wrong:
	lda #$03 ;; C
display_result:
	sta $0400,x
	rts
.pend

;; ---------------------------
;; Function: cia1_timer_a_set_timer
;; Turn on/off the CIA#1 Timer A Timer
;; C64PRG: p106
;;
;; Input Registers: A, a CIA1_TimerA* constant
;; Return Value: None
;; Destroys: A
;; ---------------------------

cia1_timer_a_set_timer: .proc
	and CIA1_TimerA_Timer_Mask
	sta tmp_0
	lda $dc0e
	and ~CIA1_TimerA_Timer_Mask
	ora tmp_0
	sta $dc0e
	rts
tmp_0:
	.byte $00
.pend

TEST_cia1_timer_a_set_timer: .proc
	jsr reset_c64
	ldx #$00
	jsr cia1_timer_a_get_timer
	jsr TESTUtil_display_CIA1_TimerA_Timer_value	; emit A

	lda CIA1_TimerA_Timer_Disable
	jsr cia1_timer_a_set_timer

	ldx #$01
	jsr cia1_timer_a_get_timer
	jsr TESTUtil_display_CIA1_TimerA_Timer_value	; emit B

	lda CIA1_TimerA_Timer_Enable
	jsr cia1_timer_a_set_timer

	ldx #$02
	jsr cia1_timer_a_get_timer
	jsr TESTUtil_display_CIA1_TimerA_Timer_value	; emit A
	rts
.pend


;; ---------------------------
;; Function: pp_get_ram_state
;; Return the state of all 3 RAM enable/disable bits in the Processor Port
;; C64PRG: p106 p320-p321
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: A
;; ---------------------------

pp_get_ram_state: .proc
	lda $0001
	and PP_RAM_Mask
	rts
.pend

TEST_pp_get_ram_state: .proc
	jsr reset_c64
	jsr pp_get_ram_state ;; A <- $07

	ldx #$00
	jsr TESTUtil_display_hex_byte
	rts
.pend


;; ---------------------------
;; Function: TESTUtil_display_hex_byte
;; A contains byte to display, X contains line offset to start display.
;; Y is preserved across the function, but could be different if this 
;; function is interrupted. 
;;
;; NOTE: Assumes video bank 0, and screen loc 2 ($0400)
;; 
;; The output is: $<upper nibble><lower nibble>
;; ---------------------------
TESTUtil_display_hex_byte: .proc
	sta tmp_0
	sty tmp_1
	;; write dollar sign
	lda #'$'
	sta $0400,x
	inx
	lda tmp_0
	;; write upper nibble
	and #$f0
	clc
	lsr a
	lsr a
	lsr a
	lsr a
	tay
	lda hex_to_char_rom_loc_lut,y
	sta $0400,x
	;; write lower nibble
	lda tmp_0
	and #$0f
	tay
	inx
	lda hex_to_char_rom_loc_lut,y
	sta $0400,x
	ldy tmp_1
	rts
tmp_0:
	.byte $00
tmp_1:
	.byte $00
hex_to_char_rom_loc_lut:
	.byte $30 ;; 0
	.byte $31 ;; 1
	.byte $32 ;; 2
	.byte $33 ;; 3
	.byte $34 ;; 4
	.byte $35 ;; 5
	.byte $36 ;; 6
	.byte $37 ;; 7
	.byte $38 ;; 8
	.byte $39 ;; 9
	.byte $01 ;; A
	.byte $02 ;; B
	.byte $03 ;; C
	.byte $04 ;; D
	.byte $05 ;; E
	.byte $06 ;; F
.pend

;; ---------------------------
;; Function: pp_set_ram_state
;; Set the state of all 3 RAM enable/disable bits in the Processor Port
;; C64PRG: p106,p320-p321
;;
;; Input Registers: A
;; Return Value: None
;; Destroys: A
;; ---------------------------
pp_set_ram_state: .proc
	and PP_RAM_Mask
	sta tmp_0
	lda $0001
	and ~PP_RAM_Mask
	ora tmp_0
	sta $0001
	rts
tmp_0:
	.byte $00
.pend

TEST_pp_set_ram_state: .proc
	jsr reset_c64

	;; TODO: Might have to turn off some (all?) interrupt timers too while
	;; basic and the kernel are off. Look into that.

	lda PP_Kernel_Disable
	ora PP_Char_Disable
	ora PP_BASIC_Disable
	jsr pp_set_ram_state

	jsr pp_get_ram_state
	sta tmp_0
	jsr reset_processor_port

	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ; <- $04

	rts
tmp_0:
	.byte $00
.pend


;; ---------------------------
;; Function: pp_get_ram_basic_state
;; Return the current state of the BASIC RAM Bit in the processor port.
;; C64PRG: p106 p320-p321
;;
;; The return value in A will be one of these two constants:
;; 	PP_BASIC_Enable, or PP_BASIC_Disable
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: A
;; ---------------------------

pp_get_ram_basic_state: .proc
	lda $0001
	and PP_BASIC_Mask
	rts
.pend

TEST_pp_get_ram_basic_state: .proc
	jsr reset_c64
	jsr pp_get_ram_basic_state
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $01
	rts
.pend

;; ---------------------------
;; Function: pp_set_ram_basic_state
;; Set the state of the BASIC RAM bit in the processor port.
;; C64PRG: p106 p320-p321
;;
;; The return value in A will be one of these two constants:
;; 	PP_BASIC_Enable, or PP_BASIC_Disable
;;
;; Input Registers: A, a PP_BASIC_(Enable|Disable) constant
;; Return Value: None
;; Destroys: A
;; ---------------------------
pp_set_ram_basic_state: .proc
	and PP_BASIC_Mask
	sta tmp_0
	lda $0001
	and ~PP_BASIC_Mask
	ora tmp_0
	sta $0001
	rts
tmp_0:
	.byte $00
.pend

TEST_pp_set_ram_basic_state: .proc
	jsr reset_c64
	lda PP_BASIC_Disable
	jsr pp_set_ram_basic_state
	jsr pp_get_ram_basic_state
	sta tmp_0
	jsr reset_processor_port
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $00
	rts
tmp_0:
	.byte $00
.pend

;; ---------------------------
;; Function: pp_get_ram_kernel_state
;; Return the current state of the Kernel RAM Bit in the processor port.
;; C64PRG: p106 p320-p321
;;
;; The return value in A will be one of these two constants:
;; 	PP_Kernel_Enable, or PP_Kernel_Disable
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: A
;; ---------------------------

pp_get_ram_kernel_state: .proc
	lda $0001
	and PP_Kernel_Mask
	rts
.pend

TEST_pp_get_ram_kernel_state: .proc
	jsr reset_c64
	jsr pp_get_ram_kernel_state
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $02
	rts
.pend

;; ---------------------------
;; Function: pp_set_ram_kernel_state
;; Set the state of the Kernel RAM bit in the processor port.
;; C64PRG: p106 p320-p321
;;
;; Input Registers: A, a PP_Kernel_(Enable|Disable) constant
;; Return Value: None
;; Destroys: A
;; ---------------------------
pp_set_ram_kernel_state: .proc
	and PP_Kernel_Mask
	sta tmp_0
	lda $0001
	and ~PP_Kernel_Mask
	ora tmp_0
	sta $0001
	rts
tmp_0:
	.byte $00
.pend

TEST_pp_set_ram_kernel_state: .proc
	jsr reset_c64
	lda PP_Kernel_Disable
	jsr pp_set_ram_kernel_state
	jsr pp_get_ram_kernel_state
	sta tmp_0
	jsr reset_processor_port
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $00
	rts
tmp_0:
	.byte $00
.pend

;; ---------------------------
;; Function: pp_get_ram_char_state
;; Return the current state of the CHAR RAM Bit in the processor port.
;; C64PRG: p106 p320-p321
;;
;; The return value in A will be one of these two constants:
;; 	PP_Char_Enable, or PP_Char_Disable
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: A
;; ---------------------------

pp_get_ram_char_state: .proc
	lda $0001
	and PP_Char_Mask
	rts
.pend

TEST_pp_get_ram_char_state: .proc
	jsr reset_c64
	lda CIA1_TimerA_Timer_Disable
	jsr cia1_timer_a_set_timer
	jsr pp_get_ram_char_state
	sta tmp_0
	lda CIA1_TimerA_Timer_Enable
	jsr cia1_timer_a_set_timer
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $04 (means disabled!)
	rts
tmp_0:
	.byte $00
.pend

;; ---------------------------
;; Function: pp_set_ram_char_state
;; Set the state of the Char RAM bit in the processor port.
;; C64PRG: p106 p320-p321
;;
;; Input Registers: A, a PP_Char_(Enable|Disable) constant
;; Return Value: None
;; Destroys: A
;; ---------------------------
pp_set_ram_char_state: .proc
	and PP_Char_Mask
	sta tmp_0
	lda $0001
	and ~PP_Char_Mask
	ora tmp_0
	sta $0001
	rts
tmp_0:
	.byte $00
.pend

TEST_pp_set_ram_char_state: .proc
	jsr reset_c64
	lda CIA1_TimerA_Timer_Disable
	jsr cia1_timer_a_set_timer
	lda PP_Char_Enable
	jsr pp_set_ram_char_state
	jsr pp_get_ram_char_state
	sta tmp_0
	jsr reset_processor_port
	lda CIA1_TimerA_Timer_Enable
	jsr cia1_timer_a_set_timer
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $00 (means enabled)
	rts
tmp_0:
	.byte $00
.pend

;; ---------------------------
;; Function: vic_get_color_mode_state
;; Return the current state of the VIC standard/multi color mode
;; C64PRG: p115
;;
;; The return value in A will be one of these two constants:
;; 	VIC_Color_Mode_Standard, VIC_Color_Mode_Multi
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: A
;; ---------------------------

vic_get_color_mode_state: .proc
	lda $d016
	and VIC_Color_Mode_Mask
	rts
.pend

TEST_vic_get_color_mode_state: .proc
	jsr reset_c64
	jsr vic_get_color_mode_state
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $00
	rts
.pend

;; ---------------------------
;; Function: vic_set_color_mode_state
;; Sets the current state of the VIC standard/multi color mode
;; C64PRG: p115
;;
;; The input value in A must be one of these two constants:
;; 	VIC_Color_Mode_Standard, VIC_Color_Mode_Multi
;;
;; Input Registers: A
;; Return Value: None
;; Destroys: A
;; ---------------------------

vic_set_color_mode_state: .proc
	and VIC_Color_Mode_Mask
	sta tmp_0
	lda $d016
	and ~VIC_Color_Mode_Mask
	ora tmp_0
	sta $d016
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_set_color_mode_state: .proc
	jsr reset_c64
	lda VIC_Color_Mode_Multi
	jsr vic_set_color_mode_state
	jsr vic_get_color_mode_state
	sta tmp_0
	lda VIC_Color_Mode_Standard
	jsr vic_set_color_mode_state
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $10
	rts
tmp_0:
	.byte $00
.pend




TEST_vic_multi_color_display: .proc
	jsr reset_c64
	ldx #$00
font_copy_loop:
	cpx #(end_multi_char_font - start_multi_char_font)
	beq font_copy_done
	lda start_multi_char_font,x
	sta $3000,x
	inx
	jmp font_copy_loop
font_copy_done:

	;; tell vic where to find new font
	lda VIC_CharMemLoc_6
	jsr vic_set_char_mem_loc

	;; turn on multi color mode
	lda VIC_Color_Mode_Multi
	jsr vic_set_color_mode_state

	lda #$00 ;; NOTE: The empty background color char in the font.
	jsr vic_clear_default_screen

	;; set up color registers
	lda Black
	sta $d021 ;; background color #0 (screen color)
	lda White
	sta $d022 ;; background color #1
	lda Red
	sta $d023 ;; background color #2

	;; show all Multicolor font characters.
	ldx #$00
display_chars_loop:
	cpx #((end_multi_char_font - start_multi_char_font) / 8)
	beq display_chars_done
	txa
	sta $0400,x
	lda MC_Cyan
	sta $d800,x
	inx
	jmp display_chars_loop
display_chars_done:

	rts

start_multi_char_font:
color_0:
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
color_1:
	.byte %01010101
	.byte %01010101
	.byte %01010101
	.byte %01010101
	.byte %01010101
	.byte %01010101
	.byte %01010101
	.byte %01010101
color_2:
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
	.byte %10101010
color_3:
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
mix_c0c1:
	.byte %00010101
	.byte %00000101
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %00000101
	.byte %00010101
mix_c0c2:
	.byte %00101010
	.byte %00001010
	.byte %00000010
	.byte %00000000
	.byte %00000000
	.byte %00000010
	.byte %00001010
	.byte %00101010
mix_c0c3:
	.byte %00111111
	.byte %00001111
	.byte %00000011
	.byte %00000000
	.byte %00000000
	.byte %00000011
	.byte %00001111
	.byte %00111111
mix_c1c0:
	.byte %01000000
	.byte %01010000
	.byte %01010100
	.byte %01010101
	.byte %01010101
	.byte %01010100
	.byte %01010000
	.byte %01000000
mix_c1c2:
	.byte %01101010
	.byte %01011010
	.byte %01010110
	.byte %01010101
	.byte %01010101
	.byte %01010110
	.byte %01011010
	.byte %01101010
mix_c1c3:
	.byte %01111111
	.byte %01011111
	.byte %01010111
	.byte %01010101
	.byte %01010101
	.byte %01010111
	.byte %01011111
	.byte %01111111
mix_c2c0:
	.byte %10000000
	.byte %10100000
	.byte %10101000
	.byte %10101010
	.byte %10101010
	.byte %10101000
	.byte %10100000
	.byte %10000000
mix_c2c1:
	.byte %10010101
	.byte %10100101
	.byte %10101001
	.byte %10101010
	.byte %10101010
	.byte %10101001
	.byte %10100101
	.byte %10010101
mix_c2c3:
	.byte %10111111
	.byte %10101111
	.byte %10101011
	.byte %10101010
	.byte %10101010
	.byte %10101011
	.byte %10101111
	.byte %10111111
mix_c3c0:
	.byte %11000000
	.byte %11110000
	.byte %11111100
	.byte %11111111
	.byte %11111111
	.byte %11111100
	.byte %11110000
	.byte %11000000
mix_c3c1:
	.byte %11010101
	.byte %11110101
	.byte %11111101
	.byte %11111111
	.byte %11111111
	.byte %11111101
	.byte %11110101
	.byte %11010101
mix_c3c2:
	.byte %11101010
	.byte %11111010
	.byte %11111110
	.byte %11111111
	.byte %11111111
	.byte %11111110
	.byte %11111010
	.byte %11101010
end_marker: ;; a box so I know I saw everything.
	.byte %11111111
	.byte %11000011
	.byte %11000011
	.byte %11000011
	.byte %11000011
	.byte %11000011
	.byte %11000011
	.byte %11111111
end_multi_char_font:

.pend

;; ---------------------------
;; Function: vic_get_extended_color_mode_state
;; Return the current state of the VIC extended color mode
;; C64PRG: p120
;;
;; The return value in A will be one of these two constants:
;; 	VIC_Extended_Color_Mode_Enable, or VIC_Extended_Color_Mode_Disable
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: A
;; ---------------------------

vic_get_extended_color_mode_state: .proc
	lda $d011
	and VIC_Extended_Color_Mode_Mask
	rts
.pend

TEST_vic_get_extended_color_mode_state: .proc
	jsr reset_c64
	jsr vic_get_extended_color_mode_state
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $00
	rts
.pend

;; ---------------------------
;; Function: vic_set_extended_color_mode_state
;; Sets the current state of the VIC extened color mode
;; C64PRG: p120
;;
;; The input value in A must be one of these two constants:
;; 	VIC_Extended_Color_Mode_Enable, VIC_Extended_Color_Mode_Disable
;;
;; Input Registers: A
;; Return Value: None
;; Destroys: A
;; ---------------------------

vic_set_extended_color_mode_state: .proc
	and VIC_Extended_Color_Mode_Mask
	sta tmp_0
	lda $d011
	and ~VIC_Extended_Color_Mode_Mask
	ora tmp_0
	sta $d011
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_set_extended_color_mode_state: .proc
	jsr reset_c64
	lda VIC_Extended_Color_Mode_Enable
	jsr vic_set_extended_color_mode_state
	jsr vic_get_extended_color_mode_state
	sta tmp_0
	lda VIC_Extended_Color_Mode_Disable
	jsr vic_set_extended_color_mode_state
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $40
	rts
tmp_0:
	.byte $00
.pend


TEST_vic_extended_color_display: .proc
	jsr reset_c64

	lda VIC_Extended_Color_Mode_Enable
	jsr vic_set_extended_color_mode_state

	;; Set background colors
	lda Black
	sta $d021 ;; bkgd #0
	lda White
	sta $d022 ;; bkgd #1
	lda Red
	sta $d023 ;; bkgd #2
	lda Cyan
	sta $d024 ;; bkgd #3

	;; First row

	; white A on a black background
	lda #$01
	sta $0400
	lda White
	sta $d800

	; Purple A on a white background
	lda #$41
	sta $0401
	lda Purple
	sta $d801

	; Cyan A on a red background
	lda #$81
	sta $0402
	lda Cyan
	sta $d802

	; Black A on a cyan background
	lda #$c1
	sta $0403
	lda Black
	sta $d803
	
	;; Second row 

	; Purple A on a black background
	lda #$01
	sta $0428
	lda Purple
	sta $d828

	; Blue A on a white background
	lda #$41
	sta $0429
	lda Blue
	sta $d829

	; Yellow A on a red background
	lda #$81
	sta $042a
	lda Yellow
	sta $d82a

	; Brown A on a cyan background
	lda #$c1
	sta $042b
	lda Brown
	sta $d82b

	rts
.pend


;; TODO: All this stuff below!


;; standard bit map mode
; $d011 bit-5 is 1 (enabled)
; $d011 bit-5 is 0 (disabled)

;; ---------------------------
;; Function: vic_get_bitmap_mode_state
;; Return the current state of the VIC bitmap mode
;; C64PRG: p122
;;
;; The return value in A will be one of these two constants:
;; 	VIC_Bitmap_Mode_Enable, or VIC_Bitmap_Mode_Disable
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: A
;; ---------------------------

vic_get_bitmap_mode_state: .proc
	lda $d011
	and VIC_Bitmap_Mode_Mask
	rts
.pend

TEST_vic_get_bitmap_mode_state: .proc
	jsr reset_c64
	jsr vic_get_bitmap_mode_state
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $00
	rts
.pend

;; ---------------------------
;; Function: vic_set_bitmap_mode_state
;; Sets the current state of the VIC bitmap mode
;; C64PRG: p122
;;
;; The input value in A must be one of these two constants:
;; 	VIC_Bitmap_Mode_Enable, VIC_Bitmap_Mode_Disable
;;
;; Input Registers: A
;; Return Value: None
;; Destroys: A
;; ---------------------------

vic_set_bitmap_mode_state: .proc
	and VIC_Bitmap_Mode_Mask
	sta tmp_0
	lda $d011
	and ~VIC_Bitmap_Mode_Mask
	ora tmp_0
	sta $d011
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_set_bitmap_mode_state: .proc
	jsr reset_c64
	lda VIC_Bitmap_Mode_Enable
	jsr vic_set_bitmap_mode_state
	jsr vic_get_bitmap_mode_state
	sta tmp_0
	lda VIC_Bitmap_Mode_Disable
	jsr vic_set_bitmap_mode_state
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $20
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_bitmap_display: .proc
	jsr reset_c64

	;; tell vic where to find the bitmap area (in VBank_0 use CML_4 to make 8K
	;; of space that doesn't interfere with zero page or character roms)
	lda VIC_CharMemLoc_4
	jsr vic_set_char_mem_loc

	lda VIC_Bitmap_Mode_Enable
	jsr vic_set_bitmap_mode_state

	;; Set foreground and background bitmap colors to black for all 8x8
	;; regions
	lda #$00
	jsr vic_clear_default_screen

	;; Now write something into the bitmap area and see if it shows up
	lda White << 4 ;; TODO: Figure out constant math in 64tass manual
	ora Black
	sta $0400

	ldx #$00
	lda #$80
line_0_loop:
	cpx #$08
	beq line_0_done
	sta $2000,x
	lsr a
	inx
	jmp line_0_loop
line_0_done:

	;; Now write something into the bitmap area and see if it shows up
	lda Red << 4 ;; TODO: Figure out constant math in 64tass manual
	ora Black
	sta $0401

	ldx #$00
	lda #$80
line_1_loop:
	cpx #$08
	beq line_1_done
	sta $2008,x
	lsr a
	inx
	jmp line_1_loop
line_1_done:
	
	rts
.pend


;; NOTE: Multicolor bit map mode is built from Bitmap Mode and Multicolor
;; mode. So there isn't any code specifically to enable/disable it. If I 
;; want a convenience function, I can write one (or two)...

;; multi-color bit map mode
; $d011 bit-5 is 1 (enabled)
; $d016 bit-4 is 1 

; $d011 bit-5 is 0 (disabled)
; $d016 bit-4 is 0

TEST_vic_multi_color_bitmap_display: .proc
	jsr reset_c64

	;; tell vic where to find the bitmap area (in VBank_0 use CML_4 to make 8K
	;; of space that doesn't interfere with zero page or character roms)
	lda VIC_CharMemLoc_4 ; $2000
	jsr vic_set_char_mem_loc

	lda VIC_Bitmap_Mode_Enable
	jsr vic_set_bitmap_mode_state

	lda VIC_Color_Mode_Multi
	jsr vic_set_color_mode_state


	;; 2-bits in each charmem byte in Multicolor bitmap mode is:
	;; 00 Background #0
	;; 01 Upper 4 bits color of screen memory
	;; 10 Lower 4 bits color of screen memory
	;; 11 lower 4 bits color of color memory

	;; Set foreground and background bitmap colors to black for all 8x8
	;; regions
	lda #$00
	jsr vic_clear_default_screen

	;; Now write something into the bitmap area and see if it shows up
	lda Black
	sta $d021	;; 00: bkgd #0
	lda White << 4 ;; TODO: Figure out constant math in 64tass manual
	ora Red
	sta $0400	;; 01: bits 7-4 screen memory color
				;; 10: bits 3-0 screen memory color
	lda Cyan
	sta $d800	;; 11: use lower nibble of color ram

	lda #%00000000
	sta $2000
	lda #%01010101
	sta $2001
	lda #%10101010
	sta $2002
	lda #%11111111
	sta $2003

	lda #%11111111
	sta $2004
	lda #%10101010
	sta $2005
	lda #%01010101
	sta $2006
	lda #%00000000
	sta $2007

	;; Now write something into the bitmap area and see if it shows up
	;lda Red << 4 ;; TODO: Figure out constant math in 64tass manual
	;ora Black
	;sta $0401

	
	rts
.pend



;; ---------------------------
;; Function: vic_get_column_mode_state
;; Return the current state of the VIC column mode
;; C64PRG: p129
;;
;; The return value in A will be one of these two constants:
;; 	VIC_Column_40_Mode or VIC_Column_38_Mode
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: A
;; ---------------------------

vic_get_column_mode_state: .proc
	lda $d016
	and VIC_Column_Mode_Mask
	rts
.pend

TEST_vic_get_column_mode_state: .proc
	jsr reset_c64
	jsr vic_get_column_mode_state
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $08
	rts
.pend


;; ---------------------------
;; Function: vic_set_column_mode_state
;; Sets the current column mode state of the VIC chip
;; C64PRG: p129
;;
;; The input value in A must be one of these two constants:
;; 	VIC_Column_38_Mode, VIC_Column_40_Mode
;;
;; Input Registers: A
;; Return Value: None
;; Destroys: A
;; ---------------------------

vic_set_column_mode_state: .proc
	and VIC_Column_Mode_Mask
	sta tmp_0
	lda $d016
	and ~VIC_Column_Mode_Mask
	ora tmp_0
	sta $d016
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_set_column_mode_state: .proc
	jsr reset_c64
	lda VIC_Column_38_Mode
	jsr vic_set_column_mode_state
	jsr vic_get_column_mode_state
	sta tmp_0
	lda VIC_Column_40_Mode
	jsr vic_set_column_mode_state
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $00
	rts
tmp_0:
	.byte $00
.pend


;; ---------------------------
;; Function: vic_get_row_mode_state
;; Return the current state of the VIC row mode
;; C64PRG: p129
;;
;; The return value in A will be one of these two constants:
;; 	VIC_Row_24_Mode or VIC_Row_25_Mode
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: A
;; ---------------------------

vic_get_row_mode_state: .proc
	lda $d011
	and VIC_Row_Mode_Mask
	rts
.pend

TEST_vic_get_row_mode_state: .proc
	jsr reset_c64
	jsr vic_get_row_mode_state
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $08
	rts
.pend


;; ---------------------------
;; Function: vic_set_row_mode_state
;; Sets the current row mode state of the VIC chip
;; C64PRG: p129
;;
;; The input value in A must be one of these two constants:
;; 	VIC_Row_24_Mode, VIC_Row_25_Mode
;;
;; Input Registers: A
;; Return Value: None
;; Destroys: A
;; ---------------------------

vic_set_row_mode_state: .proc
	and VIC_Row_Mode_Mask
	sta tmp_0
	lda $d011
	and ~VIC_Row_Mode_Mask
	ora tmp_0
	sta $d011
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_set_row_mode_state: .proc
	jsr reset_c64
	lda VIC_Row_24_Mode
	jsr vic_set_row_mode_state
	jsr vic_get_row_mode_state
	sta tmp_0
	lda VIC_Row_25_Mode
	jsr vic_set_row_mode_state
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $00
	rts
tmp_0:
	.byte $00
.pend

;; ---------------------------
;; Function: vic_get_scroll_x
;; Return the current hardware X scroll value in A. A value from 0 to 7.
;; C64PRG: p130
;;
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: None
;; ---------------------------

vic_get_scroll_x: .proc
	lda $d016
	and VIC_Scroll_X_Mask
	rts
.pend

TEST_vic_get_scroll_x: .proc
	jsr reset_c64
	jsr vic_get_scroll_x
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $00
	rts
tmp_0:
	.byte $00
.pend

;; ---------------------------
;; Function: vic_set_scroll_x
;; Hardware scroll in the x direction.
;; C64PRG: p130
;;
;; The input value in A bits 0-2 represent a scroll from 0 to 7 pixels.
;;
;; Input Registers: A
;; Return Value: None
;; Destroys: A
;; ---------------------------

vic_set_scroll_x: .proc
	and VIC_Scroll_X_Mask
	sta tmp_0
	lda $d016
	and ~VIC_Scroll_X_Mask
	ora tmp_0
	sta $d016
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_set_scroll_x: .proc
	jsr reset_c64
	lda VIC_Column_38_Mode
	jsr vic_set_column_mode_state
	lda #$03
	jsr vic_set_scroll_x
	jsr vic_get_scroll_x
	sta tmp_0
	lda #$00
	jsr vic_set_scroll_x
	lda VIC_Column_40_Mode
	jsr vic_set_column_mode_state
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $03
	rts
tmp_0:
	.byte $00
.pend

;; A very poor demonstration of hardware X scrolling. Only 8 pixels to shift
;; and too fast to see.
TEST_vic_scroll_x_bitmap_display: .proc
	jsr reset_c64
	lda VIC_Column_38_Mode
	jsr vic_set_column_mode_state
	jsr TEST_vic_bitmap_display
	ldx #$00
scroll_forever:
	lda #$fa ;; line 250 on screen
	jsr vic_wait_vblank_simple
	txa
	jsr vic_set_scroll_x
	inx
	jmp scroll_forever
	rts
.pend

;; ---------------------------
;; Function: vic_get_scroll_y
;; Return the current hardware Y scroll value in A. A value from 0 to 7.
;; C64PRG: p130
;;
;;
;; Input Registers: None
;; Return Value: A
;; Destroys: None
;; ---------------------------

vic_get_scroll_y: .proc
	lda $d011
	and VIC_Scroll_Y_Mask
	rts
.pend

TEST_vic_get_scroll_y: .proc
	jsr reset_c64
	jsr vic_get_scroll_y
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $03
	rts
tmp_0:
	.byte $00
.pend

;; ---------------------------
;; Function: vic_set_scroll_y
;; Hardware scroll in the y direction.
;; C64PRG: p130
;;
;; The input value in A bits 0-2 represent a scroll from 0 to 7 pixels.
;;
;; Input Registers: A
;; Return Value: None
;; Destroys: A
;; ---------------------------

vic_set_scroll_y: .proc
	and VIC_Scroll_Y_Mask
	sta tmp_0
	lda $d011
	and ~VIC_Scroll_Y_Mask
	ora tmp_0
	sta $d011
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_set_scroll_y: .proc
	jsr reset_c64
	lda VIC_Row_24_Mode
	jsr vic_set_row_mode_state
	lda #$05
	jsr vic_set_scroll_y
	jsr vic_get_scroll_y
	sta tmp_0
	lda #$03 ;; default
	jsr vic_set_scroll_y
	lda VIC_Row_25_Mode
	jsr vic_set_row_mode_state
	lda tmp_0
	ldx #$00
	jsr TESTUtil_display_hex_byte ;; <- emit $05
	rts
tmp_0:
	.byte $00
.pend

TEST_vic_scroll_y_bitmap_display: .proc
	jsr reset_c64
	lda VIC_Row_24_Mode
	jsr vic_set_row_mode_state
	jsr TEST_vic_bitmap_display
	ldx #$00
scroll_forever:
	lda #$fa ;; line 250 on screen
	jsr vic_wait_vblank_simple
	txa
	jsr vic_set_scroll_y
	inx
	jmp scroll_forever
	rts
.pend

;; ---------------------------
;; Function: vic_wait_vblank_simple
;; Spinloop until the raster on on line #$fa then exit the loop.
;; C64PRG: p150
;;
;; The input value in A is the lower 8 bits of the raster register.
;; TODO: Add in the carry register to represent the 9th bit....
;; This function is a bit crappy and just good enough for exploring stuff.
;;
;; Input Registers: A
;; Return Value: None
;; Destroys: A
;; ---------------------------
vic_wait_vblank_simple: .proc
	sta tmp_0
wait_vblank: ;; TODO: Should check 9th bit of raster position too!
	lda $d012
	cmp tmp_0 
	bne wait_vblank
in_vblank:
	rts
tmp_0:
	.byte $00
.pend


hr_sprite_data_start:
hr_sprite_0:
	sprite_row %111111111111111111111111
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %111111111111111111111111
	end_sprite
hr_sprite_1:
	sprite_row %111111111111111111111111
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %111111111111111111111111
	end_sprite
hr_sprite_2:
	sprite_row %111111111111111111111111
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %111111111111111111111111
	end_sprite
hr_sprite_3:
	sprite_row %111111111111111111111111
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %111111111111111111111111
	end_sprite
hr_sprite_4:
	sprite_row %111111111111111111111111
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %111111111111111111111111
	end_sprite
hr_sprite_5:
	sprite_row %111111111111111111111111
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %111111111111111111111111
	end_sprite
hr_sprite_6:
	sprite_row %111111111111111111111111
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %111111111111111111111111
	end_sprite
hr_sprite_7:
	sprite_row %111111111111111111111111
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100111111111111111111001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %100000000000000000000001
	sprite_row %111111111111111111111111
	end_sprite
hr_sprite_data_end:


;; VIC Sprite Constants
VIC_Sprite_0_Mask = #$01
VIC_Sprite_0_Enable = #$01
VIC_Sprite_0_Disable = #$00
VIC_Sprite_1_Mask = #$02
VIC_Sprite_1_Enable = #$02
VIC_Sprite_1_Disable = #$00
VIC_Sprite_2_Mask = #$04
VIC_Sprite_2_Enable = #$04
VIC_Sprite_2_Disable = #$00
VIC_Sprite_3_Mask = #$08
VIC_Sprite_3_Enable = #$08
VIC_Sprite_3_Disable = #$00
VIC_Sprite_4_Mask = #$10
VIC_Sprite_4_Enable = #$10
VIC_Sprite_4_Disable = #$00
VIC_Sprite_5_Mask = #$20
VIC_Sprite_5_Enable = #$20
VIC_Sprite_5_Disable = #$00
VIC_Sprite_6_Mask = #$40
VIC_Sprite_6_Enable = #$40
VIC_Sprite_6_Disable = #$00
VIC_Sprite_7_Mask = #$80
VIC_Sprite_7_Enable = #$80
VIC_Sprite_7_Disable = #$00
VIC_Sprite_All_Mask = #$ff
VIC_Sprite_All_Enable = #$ff
VIC_Sprite_All_Disable = #$00

;; standard sprites
; $d015, bit-7 is sprite 7, ..., bit-0 is sprite-0 ; 1 to enable



;; VIC Sprite Color Mode Constants
VIC_Sprite_0_MC_Mask = #$01
VIC_Sprite_0_MC_Enable = #$01
VIC_Sprite_0_MC_Disable = #$00
VIC_Sprite_1_MC_Mask = #$02
VIC_Sprite_1_MC_Enable = #$02
VIC_Sprite_1_MC_Disable = #$00
VIC_Sprite_2_MC_Mask = #$04
VIC_Sprite_2_MC_Enable = #$04
VIC_Sprite_2_MC_Disable = #$00
VIC_Sprite_3_MC_Mask = #$08
VIC_Sprite_3_MC_Enable = #$08
VIC_Sprite_3_MC_Disable = #$00
VIC_Sprite_4_MC_Mask = #$10
VIC_Sprite_4_MC_Enable = #$10
VIC_Sprite_4_MC_Disable = #$00
VIC_Sprite_5_MC_Mask = #$20
VIC_Sprite_5_MC_Enable = #$20
VIC_Sprite_5_MC_Disable = #$00
VIC_Sprite_6_MC_Mask = #$40
VIC_Sprite_6_MC_Enable = #$40
VIC_Sprite_6_MC_Disable = #$00
VIC_Sprite_7_MC_Mask = #$80
VIC_Sprite_7_MC_Enable = #$80
VIC_Sprite_7_MC_Disable = #$00
VIC_Sprite_All_MC_Mask = #$ff
VIC_Sprite_All_MC_Enable = #$ff
VIC_Sprite_All_MC_Disable = #$00

;; multi-color sprites
; $d01c, bit-7 is multi-color mode for sprite 7, and so forth.


