;---------------------------------------------
; Variables counters flags etc.
;---------------------------------------------

* = $0900

spriteon byte $01,$01,$00,$00,$01,$01,$01,$01
spritex byte $a0,$00,$50,$70,$90,$b0,$e0,$f0
spritey byte $c0,$00,$60,$60,$60,60,$b0,$70
spritec byte $07,$07,$01,$02,$07,$03,$04,$05
spritemsb byte $00,$00,$00,$00,$00,$00,$00,$00
spritepointer byte $80,$83,$86,$86,$86,$86,$86,$92
;spritexpx byte $01,$00,$00,$01,$00,$00,$00,$00 ; d017  
;spritexpy byte $00,$01,$00,$01,$00,$00,$00,$00 ; d01d
spriteminleft  byte $30
spritemaxright  byte $20
spriteminy byte $50
spritemaxy byte $d0
spritemc1 byte $0b
spritemc2 byte $0c
spritebullit byte $00
bullitspeed byte $05
frameskip byte $00
joyvar byte $00  ; joystick read/hamdle variabble 
colvar byte $00 ; collision detection variable
enpatbase = $9300
enpat byte $00 ; which pattern for enemy sprites
enpatcnts4 byte $00 ; stepcounter pattern sprite 4
enpatcnts5 byte $00 ; stepcounter pattern sprite 5
enpatcnts6 byte $00 ; stepcounter pattern sprite 6
enpatcnts7 byte $00 ; stepcounter pattern sprite 7
enpatcnts8 byte $00 ; stepcounter pattern sprite 8
enpat4on byte $00   ; start pattern sprite 4    
enpat5on byte $00
enpat6on byte $00
enpat7on byte $00
enpat8on byte $00
spritehit byte $00

;----------------------------------------------------------
; main
;----------------------------------------------------------

* = $1000

startflag 
        ldx #$00
copyvar lda $0900,x
        sta $0a00,x
        inx
        cpx #$ff
        bne copyvar
start   ldx #$00
copyres lda $0a00,x
        sta $0900,x
        inx
        cpx #$ff
        bne copyres
        lda #$00
        sta spritehit
        jsr screensetup
        jsr irqsetup
        jsr intro
        jsr spritesetup 
        jsr waitfirebut
        jmp lockup

;-----------------------------------------------------------------


;-----------------------------------------------------------------
; screensetup
;-----------------------------------------------------------------

* = $1050

screensetup

        lda #$00
        sta $d020
        inc $d020
        sta $d01e
        lda $d018 ; sreen 0400 char 3000
        and #$f1
        ora #$0c
        sta $d018 ; tell vic that
        ldx #$00    ;
scr
        lda #$20    ;
        sta $0400,x ;this works. The
        sta $0500,x ;screen gets
        sta $0600,x ;cleared. Simply
        sta $06e8,x ;better than using
        inx         ;JSR $E544 :o))
        bne scr; 
        rts

;-----------------------------------------------------------------
; spritesetup
;-----------------------------------------------------------------



spritesetup

        lda #%11001111 ; turn on sprite bit 7,6,5,4,3,2,1,0
        sta $d015 ; tell vic that
        lda #$ff   ; turm on multi color sprite 
        sta $d01c ; tell vic that
        lda #12 ; set multicolor 1  for sprite    
        sta $d025 ; tell vic that
        lda #11 ; set multicolor 2  for sprite
        sta $d026 ; tell vic that
        lda #%0000000 ; bits set sprite x expand 8 sprites
        sta $d01d ; tell vic
        lda #%00000000 ; bits set sprite y expand 8 sprites 
        sta $d017 ; tell vic that
        rts
       

;-----------------------------------------------------------------
; music en irq setup memsetup
;-----------------------------------------------------------------

irqsetup

        lda #$01        ; load music track o1
        jsr $c000       ; init music routine
        sei             ; set interrupt bit , cpu ignores irq 
        lda #$36        ; make $a000-$bfff available for ram
        sta $01
        lda #%01111111  ; switch off interrupt signals from CIA-1
        sta $dc0d
        and $d011       ; clear most significant bit of vic raster register
        sta $d011
        lda $dc0d       ; acknowledge pending interrupts from CIA-1
        lda $dd0d       ; acknowledge pending interrupts from CIA-2
        lda #$e1        ; set rasterline where interrupt shall occur
        sta $d012       
        lda #<tosirq
        sta $0314
        lda #>tosirq
        sta $0315
        lda #%00000001
        sta $d01a
        cli
        rts
;-----------------------------------------------------------------
; musicplay and sprite screen update via irq
;-----------------------------------------------------------------

tosirq
        inc frameskip
        lda frameskip
        and #$01
        beq pass
        lda spritepointer+1
        cmp #$85
        beq resetframe
        inc spritepointer+1
        inc spritepointer+4
        inc spritepointer+5
        inc spritepointer+6
        inc spritepointer+7
pass    jmp port
resetframe
        lda #$83
        sta spritepointer+1
        lda #$86 ;             can be labeled
        sta spritepointer+4
        sta spritepointer+5
        sta spritepointer+6
        sta spritepointer+7  
port    jsr $c012
        jsr spritemove
        asl $d019       ; acknowledge the interrupt by clearing the VIC's interrupt flag
        jmp $ea31
;-----------------------------------------------------------------
; endless lockup
;-----------------------------------------------------------------

* = $1500

lockup
        
        
        jsr enspritemove0
        jsr enspritemove1
        jsr enspritemove2
        jsr enspritemove3
        jsr joyupdate
        jsr spritecollision
        lda spritehit
        cmp #$08
        bne goon
        lda #$00
        sta spritehit
        jmp start
goon    sta $0408
        lda #$30
waitras cmp $d012
        bne waitras
       
        lda enpat4on
        cmp #$00
        bne restart
        jmp lockup
restart lda spriteon+7
chkspr  cmp #$00        
        bne restart1
        lda spriteon+6
        cmp #$00
        bne restart1 
        lda spriteon+5
        cmp #$00
        bne restart1 
        lda spriteon+4
        cmp #$00
        bne restart2 
        jmp restart1
restart1
        jmp lockup
restart2
        ldx #$00
        lda #$00
spron   sta spriteon,x
        inx
        cpx #$07
        bne spron
        jmp waitfirebut2
waitfirebut
        lda $dc00
        sta joyvar
        lda #%0010000 
        bit joyvar
        bne waitfirebut
        lda #$01
        sta enpat4on
        rts
waitfirebut2
        lda #$00
        sta enpat4on
        sta enpat5on
        sta enpat6on
        sta enpat7on
        sta enpatcnts4
        sta enpatcnts5
        sta enpatcnts6
        sta enpatcnts7
        sta spriteon+4
        sta spriteon+5
        sta spriteon+6
        sta spriteon+7
        inc $d020
        lda $dc00
        sta joyvar
        lda #%0010000 
        bit joyvar
        bne waitfirebut2
        lda #$00
        sta $d020
        sta spritemsb+4
        sta spritemsb+5
        sta spritemsb+6
        sta spritemsb+7
        lda #$01
        sta enpat4on
        sta spriteon    
        sta spriteon+1
        ;sta spriteon+2 ; sprite 3 +2
        ;sta spriteon+3 ; sprite 4 +3
        sta spriteon+5
        sta spriteon+6
        sta spriteon+7
        jmp lockup


        
;-------------------------------------------------------------------
;intro splash
;-------------------------------------------------------------------
* = $5000

intro

        lda #$00
        sta $d020
        sta $d021
        jsr introwait
        lda #11
        sta $d020
        sta $d021
        jsr introwait
        lda #12
        sta $d020
        sta $d021
        jsr introwait
        lda #15
        sta $d020
        sta $d021
        jsr introwait
        lda #$01
        sta $d020
        sta $d021
        ldx #$00        
cycle   lda splash,x
        cmp #0  
        bne cont
        jmp outro
cont    sta $0541,x
        lda #$01
        sta $d941,x
        inx
        jmp cycle

outro   lda #$01
        sta $d020
        sta $d021
        jsr introwait
        jsr introwait
        jsr introwait
        lda #15
        sta $d020
        sta $d021
        jsr introwait
        lda #12
        sta $d020
        sta $d021
        jsr introwait
        lda #11
        sta $d020
        sta $d021
        jsr introwait
        lda #0
        sta $d021
        lda #0
        sta $d021
        rts

splash  text '      PROJECT MUTINY'
        text ' by '
        text 'NURD 2023'
        byte 0


introwait
        
        ldx #$00
loop
        ldy #$00
loop1   iny
        cpy #$7f
        bne loop1
        inx
        cpx #$bf
        bne loop
        rts


;-----------------------------------------------------------------
; sprite x y c poiter test routine
;-----------------------------------------------------------------
* = $8000

joyupdate
        jsr $ffe4
        cmp #$20
        bne joyin
        jmp start
joyin   lda #$80
        sta spritepointer ; space ship forward 
        lda $dc00 ;joyport 2
        sta joyvar
joyup   lda #1
        bit joyvar
        bne joydn
        ldx spritey
        cpx #$b0
        beq joydn
        dec spritey
joydn   lda #2
        bit joyvar
        bne joyle
        ldx spritey
        cpx spritemaxy
        beq joyle
        inc spritey
joyle   lda #4
        bit joyvar
        bne joyri
        lda spritemsb
        cmp #$01
        beq msbonl 
        ldx spritex
        cpx spriteminleft
        beq joybut
        dec spritex
        lda #$82
        sta spritepointer ; space ship left
        jmp joybut
msbonl  ldx spritex
        cpx #$ff
        bne msbon2
        lda #$00
        sta spritemsb
        dec spritex
        jmp joybut
msbon2  dec spritex
        jmp joybut
joyri   lda #8
        bit joyvar
        bne joybut
        lda #$81
        sta spritepointer ; space ship right
        lda spritemsb
        cmp #01
        beq msbon 
        lda spritex 
        cmp #$ff
        bne incx
        lda #$01
        sta spritemsb
        inc spritex
        jmp joybut
incx    inc spritex
        jmp joybut
msbon   lda spritex
        cmp spritemaxright
        beq joybut
        inc spritex
joybut  lda #%0010000 
        bit joyvar
        beq joyce
        lda spritebullit
        cmp #$01
        beq movebullit
        rts
movebullit
        ldy bullitspeed ; default = $03
        lda #$01        
        sta spriteon+1
bullitspeedloop 
        ldx spritey+1
        cpx #$30
        beq offbullit
        dex 
        stx spritey+1
        dey
        cpy #$00
        bne bullitspeedloop
        rts
joyce   lda spritebullit
        cmp #$00
        beq initbullit
        jmp movebullit
offbullit
        lda #$00
        sta spritebullit
        sta spriteon+1
        sta spritex+1
        sta spritey+1
        lda $d01e
        jmp joyexit
initbullit 
        lda spritemsb
        sta spritemsb+1
        lda spritex ; load spaceship x-pos +10 set to bullit x-pos
        sbc #10
        sta spritex+1 ; bullit x
        lda spritey  ; load spaceship y-pos -18 set to bullit y-pos
        sec          ; or remove sec &
        sbc #20       ; eor #20 spritey -#20 Will never be negative! m
        sta spritey+1 ;
        inc spritebullit
joyexit rts


;----------------------------------------------------------------
; enemy spritepattern move
;
;-----------------------------------------------------------------

* = $8200

enspritemove0
        lda enpat4on
        cmp #$00
        beq enproff
        lda spritemsb+4
        cmp #$01
        beq enmsb
        ldx enpatcnts4
        cpx #$ff
        beq enmsbon
        lda enpatbase,x
        sta spritey+4
        stx spritex+4
        inc enpatcnts4 
        rts
enmsb   inc spritex+4
        lda spritex+4
        cmp #$50
        beq enproff
        rts
enmsbon inc spritemsb+4
        rts
enproff 
        lda #$00
        sta enpatcnts4
        sta enpat4on
        sta spritey+4
        sta spritex+4
        sta spritemsb+4
        rts
* = $8300

enspritemove1
        lda enpatcnts4
        cmp #$20
        bne spr5waitspr4
        inc enpat5on
spr5waitspr4        
        lda enpat5on
        cmp #$00
        beq enproff1
        lda spritemsb+5
        cmp #$01
        beq enmsb1
        ldx enpatcnts5
        cpx #$ff
        beq enmsbon1
        lda enpatbase,x
        sta spritey+5
        stx spritex+5
        inc enpatcnts5 
        rts
enmsb1  inc spritex+5
        lda spritex+5
        cmp #$50
        beq enproff1
        rts
enmsbon1
        inc spritemsb+5
        rts
enproff1 
        lda #$00
        sta enpatcnts5
        sta enpat5on
        ldx #$70
        stx spritey+5
        sta spritex+5
        sta spritemsb+5
        rts

enspritemove2
        lda enpatcnts5
        cmp #$20
        bne spr5waitspr5
        inc enpat6on
spr5waitspr5        
        lda enpat6on
        cmp #$00
        beq enproff2
        lda spritemsb+6
        cmp #$01
        beq enmsb2
        ldx enpatcnts6
        cpx #$ff
        beq enmsbon2
        lda enpatbase,x
        sta spritey+6
        stx spritex+6
        inc enpatcnts6 
        rts
enmsb2  inc spritex+6
        lda spritex+6
        cmp #$50
        beq enproff2
        rts
enmsbon2
        inc spritemsb+6
        rts
enproff2 
        lda #$00
        sta enpatcnts6
        sta enpat6on
        ldx #$a0
        stx spritey+6
        sta spritex+6
        sta spritemsb+6
        rts

enspritemove3
        lda enpatcnts6
        cmp #$20
        bne spr5waitspr6
        inc enpat7on
spr5waitspr6        
        lda enpat7on
        cmp #$00
        beq enproff3
        lda spritemsb+7
        cmp #$01
        beq enmsb3
        ldx enpatcnts7
        cpx #$ff
        beq enmsbon3
        lda enpatbase,x
        sta spritey+7
        stx spritex+7
        inc enpatcnts7 
        rts
enmsb3  inc spritex+7
        lda spritex+7
        cmp #$50
        beq enproff3
        rts
enmsbon3
        inc spritemsb+7
        rts
enproff3 
        lda #$00
        sta enpatcnts7
        sta enpat7on
        ldx #$e0
        stx spritey+7
        sta spritex+7
        sta spritemsb+7
        rts

;-----------------------------------------------------------------
;bullit enemy collision detect
;-----------------------------------------------------------------


* = $a000

spritecollision
        lda $d01e
        ;sta colvar
                     ;%00001010 ; check sprite 2 & 4
        cmp #%00001010
        bne spr1n4
        jsr flash
spr1n4  cmp #%00010010 ;%00010010 ; check sprite 2 & 5 collision bits
        bne spr1n5
        jsr flash1
spr1n5            ;%00100010 ; check sprite 2 & 6
        cmp #%00100010
        bne spr1n6
        jsr flash2
spr1n6             ;%01000010 ; check sprtie 2 & 7
        cmp #%01000010
        bne spr1n7
        jsr flash3
spr1n7  ;lda #130            ;%10000010 ; check sprite 2 & 8
        cmp #%10000010
        bne nocol
        jsr flash4        
nocol   lda #$00
        sta $d01e
        rts
flash   lda #$00
        sta spriteon+3
        inc spritehit
        lda spritehit
        sta $0403
        jmp nocol
flash1  lda #$00
        sta spriteon+4
        inc spritehit
        lda spritehit
        sta $0404
        jmp nocol
flash2  lda #$00
        sta spriteon+5
        inc spritehit
        lda spritehit
        sta $0405
        jmp nocol
flash3  lda #$00
        sta spriteon+6
        inc spritehit
        lda spritehit
        sta $0406
        jmp nocol
flash4  lda #$00
        sta spriteon+7
        inc spritehit
        lda spritehit
        sta $0407
        jmp nocol
;-----------------------------------------------------------------
;enemy sprite patterns 256 bytes so enpatbase+enpat*256
;-----------------------------------------------------------------

* = $9300


;pattern0

      byte $B7,$B7,$B7,$B7,$B7,$B7
      byte $B7,$B7,$B7,$B7,$B7,$B7
      byte $B7,$B7,$B7,$B7,$B7,$B7
      byte $B7,$B7,$B7,$B6,$B6,$B6
      byte $B6,$B6,$B6,$B5,$B5,$B5
      byte $B5,$B5,$B4,$B4,$B4,$B3
      byte $B3,$B3,$B3,$B2,$B2,$B2
      byte $B1,$B1,$B1,$B0,$B0,$AF
      byte $AF,$AF,$AE,$AE,$AD,$AD
      byte $AC,$AC,$AB,$AB,$AB,$AA
      byte $AA,$A9,$A9,$A8,$A8,$A7
      byte $A6,$A6,$A5,$A5,$A4,$A4
      byte $A3,$A3,$A2,$A1,$A1,$A0
      byte $A0,$9F,$9E,$9E,$9D,$9D
      byte $9C,$9B,$9B,$9A,$99,$99
      byte $98,$97,$97,$96,$96,$95
      byte $94,$94,$93,$92,$92,$91
      byte $90,$90,$8F,$8E,$8E,$8D
      byte $8C,$8C,$8B,$8A,$8A,$89
      byte $88,$88,$87,$86,$86,$85
      byte $84,$84,$83,$82,$82,$81
      byte $80,$80,$7F,$7E,$7E,$7D
      byte $7C,$7C,$7B,$7B,$7A,$79
      byte $79,$78,$77,$77,$76,$76
      byte $75,$74,$75,$76,$77,$77
      byte $78,$79,$7A,$7A,$7B,$7C
      byte $7C,$7D,$7E,$7F,$7F,$80
      byte $81,$81,$82,$83,$83,$84
      byte $85,$85,$86,$87,$87,$88
      byte $88,$89,$8A,$8A,$8B,$8B
      byte $8C,$8C,$8D,$8E,$8E,$8F
      byte $8F,$90,$90,$91,$91,$92
      byte $92,$93,$93,$93,$94,$94
      byte $95,$95,$96,$96,$96,$97
      byte $97,$98,$98,$98,$99,$99
      byte $99,$9A,$9A,$9A,$9B,$9B
      byte $9B,$9C,$9C,$9C,$9C,$9D
      byte $9D,$9D,$9D,$9E,$9E,$9E
      byte $9E,$9E,$9F,$9F,$9F,$9F
      byte $9F,$A0,$A0,$A0,$A0,$A0
      byte $A0,$A0,$A0,$A0,$A1,$A1
      byte $A1,$A1,$A1,$A1,$A1,$A1
      byte $A1,$A1,$A1,$A1,$FF,$FF

;-----------------------------------------------------------------
; sprite msb via raster irq 
;-----------------------------------------------------------------
* = $9100

spritemove

        ldx #$07
        ldy #$0e
lomsb   lda spritey,x
        sta $d001,y
        lda spritex,x
        sta $d000,y
        lda spritemsb,x
        cmp #$01       
        rol $d010
        lda spriteon,x
        cmp #$01
        rol $d015
        lda spritec,x
        sta $d027,x
        lda spritepointer,x
        sta $07f8,x
        lda spritemc1
        sta $d025
        lda spritemc2
        sta $d026 
        dey
        dey
        dex
        bpl lomsb
        rts

;-----------------------------------------------------------------
;Sprites $2000
;-----------------------------------------------------------------

* = $2000


;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$14,$00,$00,$7D
 BYTE $00,$01,$EB,$40,$01,$FF,$40,$01,$EB,$40,$01,$AA,$40,$07,$AA,$D0,$1F,$AA,$F4,$7F
 BYTE $AA,$FD,$7F,$EB,$FD,$7F,$FF,$FD,$03,$00,$C0,$0D,$C3,$70,$06,$41,$90,$02,$00,$80
 BYTE $02,$00,$80,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$14,$00,$00,$7D
 BYTE $00,$01,$EB,$40,$01,$FF,$40,$01,$EB,$40,$01,$AA,$40,$07,$AA,$D0,$1F,$AA,$F4,$7F
 BYTE $AA,$FD,$7F,$EB,$FD,$7F,$FF,$FD,$03,$00,$C0,$0D,$C3,$70,$06,$41,$90,$0A,$02,$80
 BYTE $28,$0A,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$14,$00,$00,$7D
 BYTE $00,$01,$EB,$40,$01,$FF,$40,$01,$EB,$40,$01,$AA,$40,$07,$AA,$D0,$1F,$AA,$F4,$7F
 BYTE $AA,$FD,$7F,$EB,$FD,$7F,$FF,$FD,$03,$00,$C0,$0D,$C3,$70,$06,$41,$90,$02,$80,$A0
 BYTE $00,$A0,$28,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$10,$00,$00,$30,$00,$00,$64,$00,$01
 BYTE $ED,$00,$01,$ED,$00,$01,$ED,$00,$01,$A9,$00,$01,$ED,$00,$01,$ED,$00,$01,$ED,$00
 BYTE $01,$65,$00,$00,$64,$00,$00,$74,$00,$00,$10,$00,$00,$64,$00,$01,$31,$00,$01,$01
 BYTE $00,$01,$01,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$10,$00,$00,$30,$00,$00,$64,$00,$01
 BYTE $ED,$00,$01,$ED,$00,$01,$A9,$00,$01,$A9,$00,$01,$A9,$00,$01,$ED,$00,$01,$ED,$00
 BYTE $01,$65,$00,$00,$64,$00,$00,$64,$00,$00,$10,$00,$00,$64,$00,$01,$B9,$00,$01,$21
 BYTE $00,$01,$01,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$10,$00,$00,$20,$00,$00,$64,$00,$01
 BYTE $A9,$00,$01,$A9,$00,$01,$A9,$00,$01,$A9,$00,$01,$A9,$00,$01,$A9,$00,$01,$A9,$00
 BYTE $01,$65,$00,$00,$64,$00,$00,$64,$00,$00,$10,$00,$00,$64,$00,$01,$B9,$00,$01,$A9
 BYTE $00,$01,$A9,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$43,$40,$07,$C0,$D0,$1C,$3C
 BYTE $34,$1C,$BE,$34,$1C,$AA,$34,$07,$AA,$D0,$01,$BB,$40,$07,$AA,$D0,$1C,$AA,$34,$1C
 BYTE $BE,$34,$1C,$3C,$34,$07,$03,$D0,$01,$C1,$40,$00,$40,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$41,$40,$07,$C3,$D0,$1C,$3C
 BYTE $34,$1C,$3C,$34,$1C,$28,$34,$07,$AA,$D0,$01,$EE,$40,$07,$AA,$D0,$1C,$28,$34,$1C
 BYTE $3C,$34,$1C,$3C,$34,$07,$C3,$D0,$01,$41,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$07,$0D,$00,$07,$0F,$40,$1C,$00,$D0,$1C,$00,$34,$1C,$AA,$34,$1C,$BE
 BYTE $34,$1C,$BE,$34,$1C,$AA,$34,$07,$AA,$D0,$01,$BE,$40,$07,$AA,$D0,$1C,$AA,$34,$1C
 BYTE $BE,$34,$1C,$BE,$34,$1C,$28,$34,$1C,$00,$34,$07,$00,$34,$01,$C0,$D0,$00,$03,$40
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$28,$00,$00,$AA,$00,$02,$AA,$80,$06,$AA,$90,$12,$AA,$84,$12,$AA,$84,$12
 BYTE $AA,$84,$12,$AA,$84,$05,$55,$50,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$55
 BYTE $40,$04,$28,$10,$10,$AA,$04,$12,$AA,$84,$12,$AA,$84,$12,$AA,$84,$12,$AA,$84,$12
 BYTE $AA,$84,$12,$AA,$84,$10,$AA,$04,$04,$28,$10,$01,$55,$40,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$28,$00,$01,$55,$40,$04,$AA,$10,$12,$AA,$84,$12,$AA,$84,$12,$AA,$84,$12
 BYTE $AA,$84,$10,$AA,$04,$05,$AA,$50,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$28,$00,$00,$AA,$00,$02,$AA,$80,$05,$55,$50,$12,$AA,$84,$16,$AA,$94,$02
 BYTE $AA,$80,$02,$AA,$80,$00,$AA,$00,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$28,$00,$05,$AA,$50,$12,$AA,$84,$12,$AA,$84,$12,$AA,$84,$12,$AA,$84,$06
 BYTE $AA,$90,$02,$AA,$80,$00,$AA,$00,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$55
 BYTE $40,$04,$28,$10,$10,$AA,$04,$12,$AA,$84,$12,$AA,$84,$12,$AA,$84,$12,$AA,$84,$06
 BYTE $AA,$90,$02,$AA,$80,$00,$AA,$00,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$28,$00,$05,$55,$50,$12,$AA,$84,$12,$AA,$84,$12,$AA,$84,$12,$AA,$84,$06
 BYTE $AA,$90,$02,$AA,$80,$00,$AA,$00,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$28,$00,$00,$AA,$00,$05,$55,$50,$12,$AA,$84,$12,$AA,$84,$12,$AA,$84,$06
 BYTE $AA,$90,$02,$AA,$80,$00,$AA,$00,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$28,$00,$00,$AA,$00,$02,$AA,$80,$02,$AA,$80,$15,$55,$54,$02,$AA,$80,$02
 BYTE $AA,$80,$02,$AA,$80,$00,$AA,$00,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$28,$00,$00,$AA,$00,$02,$AA,$80,$02,$AA,$80,$06,$AA,$90,$12,$AA,$84,$05
 BYTE $55,$50,$02,$AA,$80,$00,$AA,$00,$00,$28,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00

;
 BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$01,$43,$40,$07,$C0,$D0,$1C,$3C
 BYTE $34,$1C,$BE,$34,$1C,$AA,$34,$07,$AA,$D0,$01,$BB,$40,$07,$AA,$D0,$1C,$AA,$34,$1C
 BYTE $BE,$34,$1C,$3C,$34,$07,$03,$D0,$01,$C1,$40,$00,$40,$00,$00,$00,$00,$00,$00,$00
 BYTE $00,$00,$00,$00




;incbin "sprite.bin"



;-----------------------------------------------------------------
; CHARSET $3000
;-----------------------------------------------------------------

* = $3000

incbin "char.bin"

;-----------------------------------------------------------------
; TOS MUSIC $C000
;-----------------------------------------------------------------

* = $c000

          BYTE $4C,$A2,$CE,$4C,$A8,$CE,$4C,$AE,$CE,$4C,$B4,$CE,$4C,$50,$C3,$4C,$BC,$CE,$EE,$9D
          BYTE $C4,$2C,$97,$C4,$30,$1E,$50,$36,$A9,$00,$8D,$9D,$C4,$A2,$02,$9D,$6D,$C4,$9D,$70
          BYTE $C4,$9D,$73,$C4,$9D,$7C,$C4,$CA,$10,$F1,$8D,$97,$C4,$4C,$52,$C0,$50,$15,$A9,$00
          BYTE $8D,$04,$D4,$8D,$0B,$D4,$8D,$12,$D4,$A9,$0F,$8D,$18,$D4,$A9,$80,$8D,$97,$C4,$4C
          BYTE $26,$C3,$A2,$02,$CE,$94,$C4,$10,$06,$AD,$95,$C4,$8D,$94,$C4,$BD,$69,$C4,$8D,$6C
          BYTE $C4,$A8,$AD,$94,$C4,$CD,$95,$C4,$D0,$15,$BD,$09,$C5,$85,$02,$BD,$0C,$C5,$85,$03
          BYTE $DE,$73,$C4,$30,$09,$4C,$6B,$C1,$4C,$10,$C3,$4C,$92,$C1,$BC,$6D,$C4,$B1,$02,$C9
          BYTE $FF,$D0,$11,$A9,$00,$9D,$73,$C4,$9D,$6D,$C4,$9D,$70,$C4,$4C,$86,$C0,$4C,$10,$C3
          BYTE $A8,$B9,$0F,$C5,$85,$04,$B9,$33,$C5,$85,$05,$BC,$70,$C4,$A9,$FF,$8D,$82,$C4,$B1
          BYTE $04,$9D,$76,$C4,$8D,$83,$C4,$29,$1F,$9D,$73,$C4,$A9,$47,$38,$ED,$6F,$C4,$C9,$0F
          BYTE $90,$02,$A9,$0F,$8D,$18,$D4,$2C,$83,$C4,$70,$3B,$FE,$70,$C4,$AD,$83,$C4,$10,$0B
          BYTE $C8,$B1,$04,$29,$1F,$9D,$7F,$C4,$FE,$70,$C4,$C8,$B1,$04,$9D,$7C,$C4,$0A,$A8,$AD
          BYTE $A0,$C4,$10,$1E,$B9,$A9,$C3,$8D,$84,$C4,$B9,$AA,$C3,$AC,$6C,$C4,$99,$01,$D4,$9D
          BYTE $98,$C4,$AD,$84,$C4,$99,$00,$D4,$4C,$12,$C1,$CE,$82,$C4,$AC,$6C,$C4,$BD,$7F,$C4
          BYTE $8E,$85,$C4,$0A,$0A,$0A,$AA,$BD,$2C,$CD,$8D,$86,$C4,$AD,$A0,$C4,$10,$21,$BD,$2C
          BYTE $CD,$2D,$82,$C4,$99,$04,$D4,$BD,$2A,$CD,$99,$02,$D4,$BD,$2B,$CD,$99,$03,$D4,$BD
          BYTE $2D,$CD,$99,$05,$D4,$BD,$2E,$CD,$99,$06,$D4,$AE,$85,$C4,$AD,$86,$C4,$9D,$79,$C4
          BYTE $FE,$70,$C4,$BC,$70,$C4,$B1,$04,$C9,$FF,$D0,$08,$A9,$00,$9D,$70,$C4,$FE,$6D,$C4
          BYTE $4C,$10,$C3,$AD,$A0,$C4,$30,$03,$4C,$10,$C3,$AC,$6C,$C4,$BD,$76,$C4,$29,$20,$D0
          BYTE $15,$BD,$73,$C4,$D0,$10,$BD,$79,$C4,$29,$FE,$99,$04,$D4,$A9,$00,$99,$05,$D4,$99
          BYTE $06,$D4,$AD,$A0,$C4,$30,$03,$4C,$10,$C3,$BD,$7F,$C4,$0A,$0A,$0A,$A8,$8C,$96,$C4
          BYTE $B9,$31,$CD,$8D,$9B,$C4,$B9,$30,$CD,$8D,$88,$C4,$B9,$2F,$CD,$8D,$87,$C4,$F0,$6F
          BYTE $AD,$9D,$C4,$29,$07,$C9,$04,$90,$02,$49,$07,$8D,$8D,$C4,$BD,$7C,$C4,$0A,$A8,$38
          BYTE $B9,$AB,$C3,$F9,$A9,$C3,$8D,$89,$C4,$B9,$AC,$C3,$F9,$AA,$C3,$4A,$6E,$89,$C4,$CE
          BYTE $87,$C4,$10,$F7,$8D,$8A,$C4,$B9,$A9,$C3,$8D,$8B,$C4,$B9,$AA,$C3,$8D,$8C,$C4,$BD
          BYTE $76,$C4,$29,$1F,$C9,$08,$90,$1C,$AC,$8D,$C4,$88,$30,$16,$18,$AD,$8B,$C4,$6D,$89
          BYTE $C4,$8D,$8B,$C4,$AD,$8C,$C4,$6D,$8A,$C4,$8D,$8C,$C4,$4C,$FF,$C1,$AC,$6C,$C4,$AD
          BYTE $8B,$C4,$99,$00,$D4,$AD,$8C,$C4,$99,$01,$D4,$AD,$88,$C4,$F0,$62,$AC,$96,$C4,$29
          BYTE $1F,$DE,$8E,$C4,$10,$58,$9D,$8E,$C4,$AD,$88,$C4,$29,$E0,$8D,$9C,$C4,$BD,$91,$C4
          BYTE $D0,$1A,$AD,$9C,$C4,$18,$79,$2A,$CD,$48,$B9,$2B,$CD,$69,$00,$29,$0F,$48,$C9,$0E
          BYTE $D0,$1D,$FE,$91,$C4,$4C,$77,$C2,$38,$B9,$2A,$CD,$ED,$9C,$C4,$48,$B9,$2B,$CD,$E9
          BYTE $00,$29,$0F,$48,$C9,$08,$D0,$03,$DE,$91,$C4,$8E,$85,$C4,$AE,$6C,$C4,$68,$99,$2B
          BYTE $CD,$9D,$03,$D4,$68,$99,$2A,$CD,$9D,$02,$D4,$AE,$85,$C4,$AD,$9B,$C4,$29,$01,$F0
          BYTE $35,$BD,$98,$C4,$F0,$30,$BD,$73,$C4,$F0,$2B,$BD,$76,$C4,$29,$1F,$38,$E9,$01,$DD
          BYTE $73,$C4,$AC,$6C,$C4,$90,$10,$BD,$98,$C4,$DE,$98,$C4,$99,$01,$D4,$BD,$79,$C4,$29
          BYTE $FE,$D0,$08,$BD,$98,$C4,$99,$01,$D4,$A9,$80,$99,$04,$D4,$AD,$9B,$C4,$29,$02,$F0
          BYTE $0E,$BD,$98,$C4,$F0,$09,$FE,$98,$C4,$AC,$6C,$C4,$99,$01,$D4,$AD,$9B,$C4,$29,$04
          BYTE $F0,$2A,$AD,$9D,$C4,$29,$01,$F0,$09,$BD,$7C,$C4,$18,$69,$18,$4C,$F9,$C2,$BD,$7C
          BYTE $C4,$0A,$A8,$B9,$A9,$C3,$8D,$84,$C4,$B9,$AA,$C3,$AC,$6C,$C4,$99,$01,$D4,$AD,$84
          BYTE $C4,$99,$00,$D4,$A0,$FF,$AD,$9E,$C4,$D0,$06,$AD,$9F,$C4,$30,$01,$C8,$8C,$A0,$C4
          BYTE $CA,$30,$03,$4C,$5F,$C0,$A9,$FF,$8D,$A0,$C4,$AD,$9E,$C4,$D0,$05,$2C,$9F,$C4,$10
          BYTE $01,$60,$50,$03,$20,$A9,$C4,$CE,$A2,$C4,$10,$F5,$AD,$A8,$C4,$29,$0F,$8D,$A2,$C4
          BYTE $AD,$A1,$C4,$CD,$A3,$C4,$D0,$0F,$A2,$00,$8E,$04,$D4,$8E,$0B,$D4,$CA,$8E,$9F,$C4
          BYTE $4C,$35,$C3,$CE,$A1,$C4,$0A,$A8,$2C,$A8,$C4,$30,$20,$70,$0C,$B9,$A9,$C3,$8D,$00
          BYTE $D4,$B9,$AA,$C3,$8D,$01,$D4,$98,$38,$ED,$A4,$C4,$A8,$B9,$A9,$C3,$8D,$07,$D4,$B9
          BYTE $AA,$C3,$8D,$08,$D4,$2C,$A5,$C4,$10,$0B,$AD,$A6,$C4,$49,$01,$8D,$04,$D4,$8D,$A6
          BYTE $C4,$50,$0B,$AD,$A7,$C4,$49,$01,$8D,$0B,$D4,$8D,$A7,$C4,$4C,$35,$C3,$16,$01,$27
          BYTE $01,$38,$01,$4B,$01,$5F,$01,$73,$01,$8A,$01,$A1,$01,$BA,$01,$D4,$01,$F0,$01,$0E
          BYTE $02,$2D,$02,$4E,$02,$71,$02,$96,$02,$BD,$02,$E7,$02,$13,$03,$42,$03,$74,$03,$A9
          BYTE $03,$E0,$03,$1B,$04,$5A,$04,$9B,$04,$E2,$04,$2C,$05,$7B,$05,$CE,$05,$27,$06,$85
          BYTE $06,$E8,$06,$51,$07,$C1,$07,$37,$08,$B4,$08,$37,$09,$C4,$09,$57,$0A,$F5,$0A,$9C
          BYTE $0B,$4E,$0C,$09,$0D,$D0,$0D,$A3,$0E,$82,$0F,$6E,$10,$68,$11,$6E,$12,$88,$13,$AF
          BYTE $14,$EB,$15,$39,$17,$9C,$18,$13,$1A,$A1,$1B,$46,$1D,$04,$1F,$DC,$20,$D0,$22,$DC
          BYTE $24,$10,$27,$5E,$29,$D6,$2B,$72,$2E,$38,$31,$26,$34,$42,$37,$8C,$3A,$08,$3E,$B8
          BYTE $41,$A0,$45,$B8,$49,$20,$4E,$BC,$52,$AC,$57,$E4,$5C,$70,$62,$4C,$68,$84,$6E,$18
          BYTE $75,$10,$7C,$70,$83,$40,$8B,$70,$93,$40,$9C,$78,$A5,$58,$AF,$C8,$B9,$E0,$C4,$98
          BYTE $D0,$08,$DD,$30,$EA,$20,$F8,$2E,$FD,$00,$07,$0E,$00,$0D,$24,$0D,$04,$4F,$0F,$01
          BYTE $01,$01,$02,$02,$05,$41,$41,$41,$45,$3D,$23,$0A,$0B,$02,$FF,$02,$8C,$02,$41,$FF
          BYTE $00,$6F,$00,$8C,$3A,$01,$01,$00,$00,$00,$01,$00,$00,$01,$50,$00,$3A,$24,$08,$00
          BYTE $40,$79,$00,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$A9,$00,$8D,$04,$D4,$8D,$0B
          BYTE $D4,$8D,$A2,$C4,$AD,$9F,$C4,$29,$0F,$8D,$9F,$C4,$0A,$0A,$0A,$0A,$A8,$B9,$A2,$CD
          BYTE $8D,$A8,$C4,$B9,$A3,$CD,$8D,$A1,$C4,$B9,$B1,$CD,$8D,$A3,$C4,$B9,$AA,$CD,$8D,$A5
          BYTE $C4,$29,$3F,$8D,$A4,$C4,$B9,$A7,$CD,$8D,$A6,$C4,$B9,$AE,$CD,$8D,$A7,$C4,$A2,$00
          BYTE $B9,$A3,$CD,$9D,$00,$D4,$C8,$E8,$E0,$0E,$D0,$F4,$AD,$A8,$C4,$29,$30,$A0,$EE,$C9
          BYTE $20,$F0,$02,$A0,$CE,$8C,$5F,$C3,$60,$57,$9D,$FE,$C5,$C5,$C5,$4F,$C8,$0F,$58,$97
          BYTE $BA,$0F,$62,$BB,$E7,$5B,$0E,$82,$91,$FE,$53,$9C,$3B,$AC,$52,$8E,$F6,$0B,$27,$C2
          BYTE $74,$59,$76,$98,$15,$1D,$38,$7E,$87,$44,$5F,$C6,$C6,$C7,$C7,$C7,$CA,$CB,$CB,$CB
          BYTE $C7,$C8,$CC,$CC,$C8,$C8,$C9,$CC,$CA,$CC,$CA,$C9,$C9,$CA,$CA,$C9,$CA,$CA,$CA,$CA
          BYTE $CA,$C8,$C8,$C8,$C8,$CC,$CC,$00,$01,$02,$03,$04,$09,$1E,$09,$1F,$00,$0A,$20,$0A
          BYTE $21,$0A,$20,$0A,$21,$0A,$20,$0A,$21,$0D,$01,$02,$03,$04,$09,$1E,$09,$1F,$00,$0A
          BYTE $20,$0A,$21,$0A,$20,$0A,$21,$14,$19,$19,$19,$19,$15,$15,$15,$1A,$0A,$20,$0A,$21
          BYTE $14,$14,$0A,$20,$0A,$21,$14,$14,$19,$19,$19,$19,$19,$19,$19,$19,$FF,$1B,$1B,$1B
          BYTE $1B,$1B,$13,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$1B
          BYTE $1B,$1B,$1B,$1B,$1B,$1B,$1A,$1C,$1C,$1C,$1C,$1C,$13,$0E,$0E,$0E,$0F,$1D,$1D,$11
          BYTE $11,$11,$11,$11,$11,$11,$15,$11,$11,$11,$11,$11,$11,$11,$1D,$1D,$1B,$1B,$1B,$1B
          BYTE $1B,$1B,$1B,$1A,$1C,$1C,$1C,$1C,$1C,$13,$0E,$0E,$18,$17,$17,$17,$17,$17,$17,$0E
          BYTE $18,$18,$0E,$18,$18,$19,$19,$19,$19,$19,$19,$19,$19,$FF,$12,$05,$06,$07,$08,$0B
          BYTE $22,$0B,$23,$12,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$10,$05
          BYTE $06,$07,$08,$0B,$22,$0B,$23,$12,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$16,$16
          BYTE $16,$16,$16,$16,$1D,$1D,$1D,$1D,$1D,$1D,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C,$0C
          BYTE $0C,$0C,$0C,$0C,$0C,$0C,$19,$19,$19,$19,$19,$19,$19,$19,$FF,$85,$05,$58,$05,$4C
          BYTE $05,$58,$02,$4C,$05,$58,$02,$4C,$05,$58,$05,$4C,$05,$58,$05,$58,$05,$4C,$05,$58
          BYTE $02,$4C,$02,$58,$4B,$8B,$0D,$1B,$85,$05,$58,$05,$4C,$05,$58,$02,$4C,$05,$58,$02
          BYTE $4C,$05,$58,$05,$4C,$05,$58,$05,$58,$05,$4C,$05,$58,$02,$4C,$02,$58,$4B,$8B,$0D
          BYTE $17,$85,$05,$58,$05,$4C,$05,$58,$02,$4C,$05,$58,$02,$4C,$05,$58,$05,$4C,$05,$58
          BYTE $05,$58,$05,$4C,$05,$58,$02,$4C,$02,$58,$82,$00,$29,$05,$29,$02,$29,$02,$2A,$05
          BYTE $2A,$02,$2A,$02,$2B,$05,$2B,$02,$2B,$02,$2C,$05,$2C,$02,$2C,$FF,$02,$3C,$02,$3D
          BYTE $05,$40,$05,$40,$02,$3D,$02,$40,$02,$42,$02,$40,$05,$3C,$02,$3B,$08,$39,$02,$3C
          BYTE $02,$3D,$05,$40,$02,$40,$02,$40,$02,$3D,$02,$40,$02,$45,$02,$45,$05,$43,$08,$40
          BYTE $02,$40,$02,$40,$02,$42,$05,$45,$05,$45,$05,$45,$05,$42,$05,$40,$05,$3C,$02,$3B
          BYTE $22,$39,$37,$39,$17,$39,$FF,$02,$3C,$02,$3D,$05,$40,$05,$40,$02,$3D,$02,$40,$02
          BYTE $42,$02,$40,$05,$3C,$02,$3B,$08,$39,$02,$3C,$02,$3D,$05,$40,$05,$42,$02,$3D,$02
          BYTE $40,$4B,$42,$02,$40,$02,$42,$02,$43,$02,$44,$02,$44,$02,$43,$02,$42,$05,$40,$02
          BYTE $3D,$05,$40,$02,$40,$02,$3F,$02,$3E,$05,$3C,$02,$3B,$22,$39,$37,$39,$17,$39,$FF
          BYTE $88,$07,$40,$02,$40,$02,$42,$02,$40,$02,$43,$0B,$44,$42,$05,$42,$05,$3D,$05,$40
          BYTE $05,$40,$02,$41,$02,$42,$02,$3D,$17,$40,$42,$08,$40,$02,$40,$02,$42,$02,$40,$02
          BYTE $43,$0B,$44,$42,$05,$42,$05,$41,$05,$42,$05,$42,$02,$42,$02,$40,$02,$3D,$0B,$39
          BYTE $42,$4B,$FF,$02,$39,$02,$3A,$05,$3D,$05,$3D,$02,$3A,$02,$3D,$02,$44,$02,$43,$05
          BYTE $42,$02,$42,$02,$41,$05,$40,$05,$3F,$05,$3F,$02,$3F,$02,$3E,$02,$3F,$17,$3B,$42
          BYTE $02,$37,$02,$38,$05,$3B,$05,$3B,$02,$38,$02,$3B,$02,$40,$02,$3F,$05,$3E,$02,$40
          BYTE $02,$41,$02,$42,$02,$44,$05,$45,$05,$45,$02,$42,$02,$40,$05,$3D,$05,$40,$8B,$0D
          BYTE $40,$45,$FF,$85,$04,$2D,$05,$2D,$02,$40,$02,$3F,$02,$40,$02,$3F,$02,$40,$05,$42
          BYTE $08,$40,$05,$3E,$05,$2D,$05,$2D,$02,$3D,$02,$3C,$02,$3D,$02,$3C,$02,$3D,$05,$3E
          BYTE $08,$3D,$05,$3C,$05,$2F,$05,$2F,$02,$3B,$02,$3A,$02,$3B,$02,$3A,$FF,$02,$3B,$05
          BYTE $3D,$05,$3B,$02,$3A,$05,$39,$05,$38,$05,$38,$02,$39,$08,$39,$05,$3A,$05,$3A,$02
          BYTE $3B,$08,$3B,$FF,$02,$3B,$05,$3D,$05,$3B,$02,$3A,$05,$3B,$02,$3C,$05,$3C,$02,$3C
          BYTE $02,$3D,$05,$3D,$02,$3D,$02,$3E,$05,$3E,$02,$3E,$02,$3F,$05,$3F,$02,$3F,$FF,$82
          BYTE $0A,$3C,$02,$3D,$05,$40,$05,$40,$02,$3D,$02,$40,$02,$42,$02,$42,$02,$41,$02,$40
          BYTE $4B,$02,$3C,$02,$3D,$05,$40,$05,$40,$02,$3D,$02,$40,$FF,$42,$08,$3C,$02,$3B,$05
          BYTE $39,$42,$FF,$02,$45,$02,$45,$02,$42,$0B,$45,$42,$FF,$02,$3C,$02,$3D,$05,$40,$05
          BYTE $40,$02,$3D,$02,$40,$02,$42,$02,$42,$02,$41,$05,$40,$02,$40,$02,$42,$02,$45,$02
          BYTE $45,$02,$45,$02,$44,$08,$43,$02,$42,$02,$42,$02,$41,$08,$40,$02,$3F,$02,$3F,$02
          BYTE $3E,$08,$3D,$02,$3C,$02,$3C,$02,$3B,$05,$39,$42,$81,$0E,$34,$01,$33,$01,$32,$01
          BYTE $31,$01,$30,$01,$2F,$01,$2E,$01,$2D,$01,$2C,$01,$2B,$01,$2A,$01,$29,$01,$28,$01
          BYTE $29,$01,$2A,$01,$2B,$01,$2C,$01,$2D,$01,$2E,$01,$2F,$01,$30,$01,$31,$01,$32,$01
          BYTE $33,$FF,$82,$0B,$33,$02,$34,$05,$37,$05,$37,$02,$34,$02,$37,$02,$39,$02,$39,$02
          BYTE $38,$02,$37,$4B,$02,$33,$02,$34,$05,$37,$05,$37,$02,$34,$02,$37,$42,$08,$33,$02
          BYTE $32,$05,$31,$42,$02,$33,$02,$34,$05,$37,$05,$37,$02,$34,$02,$37,$02,$39,$02,$39
          BYTE $02,$38,$02,$37,$4B,$02,$33,$02,$34,$05,$37,$05,$37,$02,$34,$02,$37,$02,$3D,$02
          BYTE $3D,$02,$39,$0B,$2D,$42,$FF,$02,$33,$02,$34,$05,$37,$05,$37,$02,$34,$02,$37,$02
          BYTE $39,$02,$39,$02,$38,$05,$37,$02,$37,$02,$39,$02,$3D,$02,$3D,$02,$3D,$02,$3C,$08
          BYTE $3B,$02,$39,$02,$39,$02,$38,$08,$37,$02,$36,$02,$36,$02,$35,$08,$34,$02,$33,$02
          BYTE $33,$02,$32,$08,$31,$FF,$82,$0E,$45,$02,$45,$05,$42,$02,$45,$02,$40,$02,$42,$02
          BYTE $45,$02,$48,$02,$45,$02,$47,$02,$42,$05,$45,$02,$40,$05,$43,$02,$40,$02,$42,$02
          BYTE $3D,$05,$40,$02,$39,$05,$3C,$02,$39,$02,$3B,$02,$36,$02,$39,$08,$45,$FF,$82,$00
          BYTE $3D,$02,$3D,$05,$39,$02,$3D,$02,$37,$02,$39,$02,$3D,$02,$3F,$02,$3D,$02,$3E,$02
          BYTE $39,$05,$3D,$02,$37,$05,$3B,$02,$37,$02,$39,$02,$34,$05,$37,$02,$31,$05,$33,$02
          BYTE $31,$02,$32,$02,$2D,$02,$31,$08,$39,$FF,$45,$85,$06,$33,$02,$30,$02,$33,$05,$30
          BYTE $05,$33,$05,$30,$02,$30,$02,$33,$05,$30,$FF,$85,$06,$28,$05,$28,$05,$28,$05,$28
          BYTE $FF,$82,$06,$28,$02,$28,$02,$28,$02,$28,$02,$28,$02,$28,$02,$28,$02,$28,$FF,$85
          BYTE $05,$2D,$05,$58,$05,$58,$02,$4C,$05,$58,$02,$4C,$05,$58,$05,$4C,$05,$58,$FF,$85
          BYTE $01,$25,$45,$8B,$03,$3A,$85,$01,$25,$45,$8B,$03,$3A,$FF,$83,$03,$30,$01,$30,$01
          BYTE $30,$FF,$85,$01,$25,$45,$8B,$03,$3A,$82,$06,$33,$05,$33,$02,$33,$02,$30,$05,$30
          BYTE $02,$30,$82,$06,$2E,$05,$2E,$02,$2E,$02,$2A,$05,$2A,$02,$2A,$FF,$57,$FF,$82,$08
          BYTE $50,$02,$60,$02,$40,$02,$50,$02,$60,$02,$40,$02,$50,$02,$60,$02,$50,$02,$60,$02
          BYTE $40,$02,$50,$02,$60,$02,$40,$02,$50,$02,$60,$FF,$82,$0C,$64,$02,$58,$02,$40,$02
          BYTE $64,$02,$58,$02,$40,$02,$64,$02,$58,$02,$40,$02,$64,$02,$58,$02,$40,$02,$64,$02
          BYTE $58,$02,$40,$02,$64,$FF,$05,$21,$05,$21,$02,$24,$02,$25,$02,$2A,$02,$28,$05,$21
          BYTE $05,$21,$02,$1E,$02,$1F,$05,$20,$05,$21,$05,$21,$02,$24,$02,$25,$02,$2A,$02,$28
          BYTE $05,$21,$05,$21,$02,$2A,$02,$29,$02,$28,$02,$20,$05,$21,$05,$21,$02,$24,$02,$25
          BYTE $02,$2A,$02,$28,$05,$21,$05,$21,$02,$1E,$02,$1F,$05,$20,$05,$21,$05,$2D,$05,$1E
          BYTE $05,$2A,$05,$1F,$05,$2B,$05,$20,$05,$2C,$FF,$05,$21,$05,$21,$02,$24,$02,$25,$02
          BYTE $2A,$02,$28,$05,$21,$05,$21,$02,$1E,$02,$1F,$05,$20,$05,$21,$05,$21,$02,$24,$02
          BYTE $25,$02,$2A,$02,$28,$05,$21,$05,$21,$02,$2A,$02,$29,$02,$28,$02,$20,$05,$1C,$05
          BYTE $1C,$02,$1F,$02,$20,$02,$25,$02,$23,$05,$1C,$05,$1C,$02,$19,$02,$1A,$05,$1C,$05
          BYTE $21,$05,$21,$02,$24,$02,$25,$02,$2A,$02,$28,$17,$21,$FF,$05,$1C,$05,$1C,$02,$1F
          BYTE $02,$20,$02,$25,$02,$23,$05,$1C,$05,$1C,$02,$19,$02,$1A,$05,$1C,$05,$21,$05,$21
          BYTE $02,$24,$02,$25,$02,$2A,$02,$28,$05,$21,$05,$21,$02,$2A,$02,$29,$02,$28,$02,$20
          BYTE $05,$1C,$05,$1C,$02,$1F,$02,$20,$02,$25,$02,$23,$05,$1C,$05,$1C,$02,$19,$02,$1A
          BYTE $05,$1C,$05,$21,$05,$21,$02,$24,$02,$25,$02,$2A,$02,$28,$05,$21,$05,$21,$05,$20
          BYTE $05,$1F,$FF,$05,$1E,$05,$1E,$02,$21,$02,$22,$02,$27,$02,$25,$05,$1E,$05,$1E,$02
          BYTE $27,$02,$26,$05,$25,$05,$23,$05,$23,$02,$26,$02,$27,$02,$2C,$02,$2A,$05,$23,$05
          BYTE $23,$02,$2C,$02,$2B,$05,$2A,$05,$1C,$05,$1C,$02,$1F,$02,$20,$02,$25,$02,$23,$05
          BYTE $1C,$05,$1C,$02,$25,$02,$24,$05,$23,$05,$21,$05,$21,$02,$24,$02,$25,$05,$1E,$05
          BYTE $1C,$8B,$0D,$1C,$45,$FF,$85,$02,$21,$05,$21,$02,$3D,$02,$3C,$02,$3D,$02,$3C,$02
          BYTE $3D,$05,$3E,$08,$3D,$05,$3B,$05,$21,$05,$21,$02,$39,$02,$38,$02,$39,$02,$38,$02
          BYTE $39,$05,$3B,$08,$39,$05,$39,$05,$23,$05,$23,$02,$38,$02,$37,$02,$38,$02,$37,$FF
          BYTE $02,$38,$05,$39,$05,$38,$02,$37,$05,$36,$05,$28,$05,$28,$02,$2A,$08,$2A,$05,$2B
          BYTE $05,$2B,$02,$2C,$08,$2C,$FF,$02,$38,$05,$39,$05,$38,$02,$37,$05,$38,$02,$24,$05
          BYTE $24,$02,$24,$02,$25,$05,$25,$02,$25,$02,$26,$05,$26,$02,$26,$02,$27,$05,$27,$02
          BYTE $27,$FF,$85,$02,$21,$05,$21,$82,$09,$38,$82,$02,$24,$05,$25,$05,$23,$05,$21,$85
          BYTE $09,$38,$82,$02,$1C,$02,$1E,$FF,$85,$02,$21,$05,$21,$82,$09,$38,$88,$02,$21,$37
          BYTE $1C,$17,$1C,$FF,$85,$02,$1C,$05,$1C,$02,$1F,$02,$20,$05,$1C,$05,$1C,$05,$1A,$05
          BYTE $19,$05,$1C,$05,$1C,$05,$1C,$02,$1F,$02,$20,$02,$1A,$02,$1C,$4B,$8B,$0D,$17,$85
          BYTE $02,$1C,$05,$1C,$02,$1F,$02,$20,$05,$1C,$05,$1C,$05,$1A,$05,$19,$05,$1C,$05,$1C
          BYTE $05,$1C,$02,$1F,$02,$20,$02,$1A,$02,$1C,$4B,$8B,$0D,$23,$85,$02,$1C,$05,$1C,$02
          BYTE $1F,$02,$20,$05,$1C,$05,$1C,$05,$1A,$05,$19,$05,$1C,$05,$1C,$05,$1C,$02,$1F,$02
          BYTE $20,$02,$1A,$02,$1C,$02,$1D,$05,$1D,$02,$1D,$02,$1E,$05,$1E,$02,$1E,$02,$1F,$05
          BYTE $1F,$02,$1F,$02,$20,$05,$20,$02,$20,$FF,$00,$0B,$41,$17,$65,$02,$41,$00,$00,$08
          BYTE $41,$08,$08,$00,$00,$01,$40,$0B,$41,$09,$39,$00,$41,$00,$00,$02,$81,$08,$0A,$00
          BYTE $00,$01,$40,$01,$41,$49,$87,$02,$00,$00,$00,$08,$41,$02,$00,$00,$00,$00,$00,$08
          BYTE $41,$03,$0A,$00,$00,$01,$80,$08,$41,$56,$87,$03,$00,$00,$80,$06,$81,$02,$09,$00
          BYTE $00,$01,$00,$02,$81,$09,$A9,$00,$00,$01,$80,$03,$41,$49,$39,$02,$00,$00,$80,$07
          BYTE $41,$47,$29,$02,$00,$00,$80,$06,$11,$06,$7B,$00,$00,$01,$C0,$0B,$41,$90,$F0,$01
          BYTE $E8,$02,$00,$09,$41,$17,$65,$02,$41,$04,$AA,$21,$04,$80,$00,$41,$0A,$00,$00,$0D
          BYTE $00,$02,$57,$0C,$00,$28,$13,$60,$10,$00,$04,$11,$0F,$00,$08,$07,$80,$00,$11,$0F
          BYTE $00,$16,$A4,$10,$24,$00,$04,$00,$0A,$00,$08,$09,$00,$02,$15,$0B,$00,$20,$23,$2A
          BYTE $F9,$80,$00,$81,$95,$00,$28,$00,$00,$02,$81,$AD,$40,$5F,$62,$40,$06,$80,$00,$41
          BYTE $0C,$00,$02,$14,$14,$02,$13,$0D,$00,$60,$91,$70,$33,$80,$02,$41,$0B,$00,$00,$32
          BYTE $80,$02,$41,$0B,$00,$40,$A0,$32,$54,$00,$08,$11,$04,$F0,$C2,$49,$00,$08,$11,$04
          BYTE $F0,$59,$A0,$1A,$06,$40,$00,$81,$0A,$00,$00,$18,$00,$08,$81,$09,$00,$32,$14,$20
          BYTE $00,$00,$04,$51,$0F,$70,$00,$27,$00,$08,$51,$0F,$70,$00,$A6,$10,$3D,$80,$08,$41
          BYTE $88,$17,$C0,$36,$00,$08,$41,$88,$17,$2F,$20,$20,$00,$00,$00,$81,$0B,$00,$3F,$27
          BYTE $40,$08,$13,$0B,$00,$5F,$A1,$40,$44,$00,$05,$41,$05,$F0,$3A,$3C,$00,$08,$41,$09
          BYTE $80,$41,$26,$1A,$30,$00,$08,$43,$03,$F0,$C1,$27,$40,$08,$15,$05,$F0,$5F,$A2,$4E
          BYTE $34,$80,$04,$41,$03,$F0,$00,$3C,$00,$03,$41,$03,$F0,$51,$6F,$05,$44,$00,$07,$11
          BYTE $0A,$00,$84,$1D,$00,$02,$15,$0C,$D0,$4F,$96,$51,$44,$00,$01,$41,$0A,$00,$C0,$1D
          BYTE $00,$02,$15,$0C,$00,$38,$A9,$40,$8D,$97,$C4,$60,$A9,$C0,$8D,$97,$C4,$60,$A9,$00
          BYTE $8D,$9E,$C4,$60,$A9,$FF,$8D,$9E,$C4,$4C,$50,$C3,$AE,$9E,$C4,$F0,$04,$8E,$9F,$C4
          BYTE $60,$09,$40,$8D,$9F,$C4,$60,$08,$40,$53,$3A,$54,$48,$49,$4E,$47,$20,$53,$43,$4F
          BYTE $52,$45,$53,$2F,$52,$45,$4D,$78,$20,$A3,$FD,$A2,$00,$8A,$9D,$34,$03,$E8,$E0,$CC
          BYTE $D0,$F8,$A9,$1B,$8D,$11,$D0,$58,$4C,$00,$40,$00,$00,$00,$00,$00,$00,$00,$00,$00
          BYTE $0A,$82,$A0,$00,$28,$00,$0A,$AA,$A0,$55,$69,$55,$55,$69,$55,$5F,$69,$F5,$5F,$69
          BYTE $F5,$5F,$69,$F5,$3A,$AA,$AC,$3A,$AA,$AC,$3A,$AA,$AC,$0E,$AA,$B0,$03,$AA,$C0,$00
          BYTE $EB,$00,$30,$EB,$0C,$EC,$EB,$3B,$BB,$EB,$EE,$CE,$EB,$B3,$03,$BE,$C0,$00,$EB,$00
          BYTE $00,$3C,$00,$00,$00,$28,$00,$00,$28,$00,$0A,$AA,$A0,$7D,$69,$7D,$7D,$69,$7D,$7D
          BYTE $69,$7D,$55,$69,$55,$55,$69,$55,$3A,$AA,$AC,$3A,$AA,$AC,$3A,$AA,$AC,$0E,$AA,$B0
          BYTE $03,$AA,$C0,$00,$EB,$00,$00,$EB,$00,$03,$BE,$C0,$CE,$EB,$B3,$BB,$EB,$EE,$EC,$EB
          BYTE $3B,$30,$EB,$0C,$00,$3C,$00,$00,$55,$41,$55,$55,$41,$55,$7F,$41,$FD,$55,$69,$55
          BYTE $00,$EB,$00,$03,$AA,$C0,$0E,$AA,$B0,$0E,$AA,$B0,$3A,$AA,$AC,$3A,$AA,$AC,$EA,$AA
          BYTE $AB,$EA,$AA,$AB,$EA,$AA,$AB,$3A,$AA,$AC,$3A,$AA,$AC,$0E,$AA,$B0,$0E,$AA,$B0,$03
          BYTE $AA,$C0,$00,$EB,$00,$14,$EB,$14,$0F,$FF,$F0,$00,$7F,$41,$FD,$7F,$41,$FD,$55,$41
          BYTE $55,$55,$69,$55,$00,$EB,$00,$03,$AA,$C0,$0E,$AA,$B0,$0E,$AA,$B0,$3A,$AA,$AC,$3A
          BYTE $AA,$AC,$3A,$AA,$AC,$3A,$AA,$AC,$0E,$AA,$B0,$0E,$AA,$B0,$03,$AA,$C0,$00,$EB,$00
          BYTE $00,$EB,$00,$0F,$FF,$F0,$14,$00,$14,$00,$00,$00,$00,$00,$00
