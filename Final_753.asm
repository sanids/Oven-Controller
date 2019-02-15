; ISR_example_DE1SoC.asm:
; a) Increments/decrements a BCD variable every half second using
;    an ISR for timer 2.  Uses SW0 to decide.  Also 'blinks' LEDR0 every
;    half a second.
; b) Generates a 2kHz square wave at pin P1.0 using an ISR for timer 0.
; c) In the 'main' loop it displays the variable incremented/decremented
;    using the ISR for timer 2 on the LCD and the 7-segment displays.
;    Also resets it to zero if the KEY1 pushbutton  is pressed.
; d) Controls the LCD using general purpose pins P0.0 to P0.6.  Pins P0.0
;    to P0.6 are configured as outputs.
;
$NOLIST
$MODDE1SOC
$LIST

LTC2308_MISO bit 0xF8 ; Read only bit
LTC2308_MOSI bit 0xF9 ; Write only bit
LTC2308_SCLK bit 0xFA ; Write only bit
LTC2308_ENN  bit 0xFB ; Write only bit

CLK           EQU 33333333 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/(12*TIMER0_RATE)))) ; The prescaler in the CV-8052 is 12 unlike the AT89LP51RC2 where is 1.
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/(12*TIMER2_RATE))))

BAUD           EQU 57600
TIMER_1_RELOAD EQU (256-((2*CLK)/(12*32*BAUD)))
TIMER_10ms     EQU (65536-(CLK/(12*100)))

SOUND_OUT     equ P1.0
UPDOWN        equ SWA.0

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
; These register definitions needed by 'math32.inc'

Count1ms:       ds 2 ; Used to determine when half second has passed
BCD_counter:    ds 2 ; The BCD counter incrememted in the ISR and displayed in the main loop
Soak_counter:   ds 2
Buzzer_counter: ds 1 

; Each FSM has its own timer
FSM2_timer:     ds 1
FSM3_timer:     ds 1
FSM4_timer:     ds 1
; Each FSM has its own state counter
FSM2_state:     ds 1
FSM3_state:     ds 1
FSM4_state:     ds 1
state:          ds 1
pwm:            ds 1

; Interface for interacting with LCD
Mode_sel:          ds 1 
Soak_temp:         ds 2 
Soak_time:         ds 2 
Reflow_temp:       ds 2 
Reflow_time:       ds 2 
Total_Soak_time:   ds 2
Total_Reflow_time: ds 2

; General variables for multiple use
Temperature:       ds 2  
Current_state:     ds 1 
Room_temp:         ds 2

State2Switch:      ds 2
Satte4Switch:      ds 2

loop_incrementer:  ds 4

y:   ds 4
bcd: ds 5
x:   ds 4

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
Reset_flag:   dbit 1 
Process_flag: dbit 1 

; For each pushbutton we have a flag.  The corresponding FSM will set this
; flags to one when a valid press of the pushbutton is detected.
Key1_flag:    dbit 1
Key2_flag:    dbit 1
Key3_flag:    dbit 1
Buzzer_flag:  dbit 1
PWM_flag:     dbit 1
mf:           dbit 1      ; math32.inc uses this too
short_beep_flag: dbit 1
long_beep_flag: dbit 1

cseg
; These 'equ' must match the wiring between the DE1-SoC board and the LCD!
; P0 is in connector JP2.  Check "CV-8052 Soft Processor in the DE1-SoC Board: Getting
; Started Guide" for the details.
ELCD_RS equ P0.4
ELCD_RW equ P0.5
ELCD_E  equ P0.6
ELCD_D4 equ P0.0
ELCD_D5 equ P0.1
ELCD_D6 equ P0.2
ELCD_D7 equ P0.3
; This 'equ' must match the hardware connection
FT93C66_CE   EQU P2.0 ; Connect to pin 1 of 93C66
FT93C66_MOSI EQU P2.1 ; Connect to pin 3 of 93C66
FT93C66_MISO EQU P2.2 ; Connect to pin 4 of 93C66
FT93C66_SCLK EQU P2.3 ; Connect to pin 2 of 93C66
PWMPIN       EQU P2.4


Configure_pin_as_output mac
orl %0MOD, #(1<<%1) ; Configure %0.%1 as output
endmac

$NOLIST
$include(FT93C66_DE1SoC.inc)
$include(LCD_4bit_DE1SoC.inc) ; A library of LCD related functions and utility macros
$include(math32.inc)
$LIST

;                       1234567890123456    <- This helps determine the location of the counter
Default_time:       db 'Run-time:   ', 0
SPACE:				db ' ',0
Default_state:      db 'State:', 0
Soak_tempS:         db 'Soak Temp:  ', 0
Soak_timeS:         db 'Soak Time:  ', 0
Reflow_tempS:       db 'Reflow Temp:', 0
Reflow_timeS:       db 'Reflow Time:', 0
READY:			    db 'READY', 0
RAMP2SOAK:			db 'RAMP2SOAK',0
SOAK:				db 'SOAK',0
RAMP2PEAK:			db 'RAMP2PEAK',0
REFLOW:				db 'REFLOW',0
COOLING:		    db 'COOLING',0
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
;Timer0_ISR:
;	;clr TF0  ; According to the data sheet this is done for us already.
;	mov TH0, #high(TIMER0_RELOAD) ; Timer 0 doesn't have autoreload in the CV-8052
;	mov TL0, #low(TIMER0_RELOAD)
;	cpl SOUND_OUT ; Connect speaker to P3.7!
;	reti
Timer0_ISR:
   jb PWM_flag, HIGH_DONE
LOW_DONE:
    setb PWM_flag
    setb PWMPIN
    mov TH0, pwm
    clr TF0
    clr a
    reti
HIGH_DONE:
    clr PWMPIN  
    clr PWM_flag
    mov a, #0FFH
    clr c
    subb a, pwm
    mov TH0, a
    clr TF0
    clr a
    reti
    ;clr TF0  ; According to the data sheet this is done for us already.
	;mov TH0, #high(TIMER0_RELOAD) ; Timer 0 doesn't have autoreload in the CV-8052
	;mov TL0, #low(TIMER0_RELOAD)
;	cpl SOUND_OUT ; Connect speaker to P3.7!;
;	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P1.1 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.

	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
    ; Increment the BCD counter
	inc FSM2_timer 
	inc FSM3_timer 
	inc FSM4_timer 
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1
Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know half second had passed
	; Toggle LEDR0 so it blinks
	cpl LEDRA.0
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	inc Buzzer_counter
	
	inc Soak_counter+0
	mov a, Soak_counter+0
	da a
	cjne a, #0x00, branch
	inc Soak_counter+1
	sjmp branch
branch:	
	inc BCD_counter+0
	mov a, BCD_counter+0
	da a 
	cjne a, #0x00, Timer2_ISR_da
	inc BCD_counter+1
	sjmp Timer2_ISR_da
Timer2_ISR_da:
Timer2_ISR_done:
	pop psw
	pop acc
	reti
; Look-up table for the 7-seg displays. (Segments are turn on with zero) 
T_7seg:
    DB 40H, 79H, 24H, 30H, 19H, 12H, 02H, 78H, 00H, 10H
;---------------------------------;
; Stuff for Serial Port           ;
;---------------------------------;
Initialize_Serial_Port:
	; Configure serial port and baud rate
	clr TR1 ; Disable timer 1
	anl TMOD, #0x0f ; Mask the bits for timer 1
	orl TMOD, #0x20 ; Set timer 1 in 8-bit auto reload mode
    orl PCON, #80H ; Set SMOD to 1
	mov TH1, #low(TIMER_1_RELOAD)
	mov TL1, #low(TIMER_1_RELOAD) 
	setb TR1 ; Enable timer 1
	mov SCON, #52H
	ret
putchar:
	jbc	TI,putchar_L1
	sjmp putchar
putchar_L1:
	mov	SBUF,a
	ret
SendString:
    clr a
    movc a, @a+dptr
    jz SendString_L1
    lcall putchar
    inc dptr
    sjmp SendString  
SendString_L1:
	ret
; Displays a BCD number in 1-HEX0
; Displays a BCD number pased in R0 in HEX1-HEX0
Display_BCD_7_Seg_HEX10:
	mov dptr, #T_7seg
	mov a, R0
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX1, a
	mov a, R0
	anl a, #0FH
	movc a, @a+dptr
	mov HEX0, a
	ret
; Displays a BCD number pased in R0 in HEX3-HEX2
Display_BCD_7_Seg_HEX32:
	mov dptr, #T_7seg
	mov a, R0
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX3, a
	mov a, R0
	anl a, #0FH
	movc a, @a+dptr
	mov HEX2, a
	ret
; Displays a BCD number pased in R0 in HEX5-HEX4
Display_BCD_7_Seg_HEX54:
	mov dptr, #T_7seg
	mov a, R0
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX5, a
	mov a, R0
	anl a, #0FH
	movc a, @a+dptr
	mov HEX4, a
	ret
; The 8-bit hex number passed in the accumulator is converted to
; BCD and stored in [R1, R0]
Hex_to_bcd_8bit:
	mov b, #100
	div ab
	mov R1, a   ; After dividing, a has the 100s
	mov a, b    ; Remainder is in register b
	mov b, #10
	div ab ; The tens are stored in a, the units are stored in b 
	swap a
	anl a, #0xf0
	orl a, b
	mov R0, a
	ret
Write_to_Memory:
	;mov Soak_temp+0,   #0x30
	;mov Soak_temp+1,   #0x01
    ;mov Soak_time+0,   #0x80
    ;mov Soak_time+1,   #0x00	
    ;mov Reflow_temp+0, #0x01
    ;mov Reflow_temp+1, #0x02
    ;mov Reflow_time+0, #0x30
    ;mov Reflow_time+1, #0x00
    
   	;0x10 Soak_temp+0
	;0x14 Soak_temp+1
	;0x20 Soak_time+0
	;0x24 Soak_time+1
	;0x30 Reflow_temp+0
	;0x34 Reflow_temp+1
	;0x40 Reflow_time+0
	;0x44 Reflow_time+1
	mov dptr, #0x10
	mov a, Soak_temp+0
	lcall FT93C66_Write
	mov dptr, #0x14
	mov a, Soak_temp+1
	lcall FT93C66_Write
	mov dptr, #0x20
	mov a, Soak_time+0
	lcall FT93C66_Write
	mov dptr, #0x24
	mov a, Soak_time+1
	lcall FT93C66_Write
	mov dptr, #0x30
	mov a, Reflow_temp+0
	lcall FT93C66_Write
	mov dptr, #0x34
	mov a, Reflow_temp+1
	lcall FT93C66_Write
	mov dptr, #0x40
	mov a, Reflow_time+0
	lcall FT93C66_Write
	mov dptr, #0x44
	mov a, Reflow_time+1
	lcall FT93C66_Write
	ret
	
; Displays a BCD number in HEX1-HEX0
Display_BCD_7_Seg:
    
    mov dptr, #T_7seg

    ; Display the channel in HEX5
    mov a, b
    anl a, #0x0f
    movc a, @a+dptr
    mov HEX5, a
    
    ; Display [R3,R2] in HEX3, HEX2, HEX1, HEX0
    mov a, R3
    swap a
    anl a, #0x0f
    movc a, @a+dptr
    mov HEX3, a
    
    mov a, R3
    anl a, #0x0f
    movc a, @a+dptr
    mov HEX2, a
    
    mov a, R2
    swap a
    anl a, #0x0f
    movc a, @a+dptr
    mov HEX1, a
    
    mov a, R2
    anl a, #0x0f
    movc a, @a+dptr
    mov HEX0, a
    ret

hex2bcd16:
    clr a
    mov R4, a ; Initialize BCD to 00-00-00 
    mov R3, a
    mov R2, a
    mov R5, #16  ; Loop counter.

hex2bcd16_L1:
    ; Shift binary left 
    mov a, R1
    mov c, acc.7 ; This way [R1,R0] remains unchanged!
    mov a, R0
    rlc a
    mov R0, a
    mov a, R1
    rlc a
    mov R1, a
    
    ; Perform bcd + bcd + carry using BCD arithmetic
    mov a, R2
    addc a, R2
    da a
    mov R2, a
    mov bcd+0,a
    
    mov a, R3
    addc a, R3
    da a
    mov R3, a
    mov bcd+1,a
    
    mov a, R4
    addc a, R4
    da a
    mov R4, a
    mov bcd+2,a

    djnz R5, hex2bcd16_L1
    
    ret

;Converts Voltage reading to Celsius and displays it on HEX Display
Voltage2Temp:
    ;Store Result, CH0 reading to x
    mov x+0, R0     ;Mov result
    mov x+1, R1
    mov x+2, #0
    mov x+3, #0
    
    ;The operation
    Load_y(804878)
    lcall mul32
    Load_y(10000000)
    lcall div32
   	Load_y(23)
    lcall add32 ;;;;;;;;;
    
    mov R0, x+0 ; x contains result of calculations
    mov R1, x+1 ; We need to put back result to R0 and R1 because those are the registers hex2bcd16 uses.
    ;mov a, R0
    ;add a, #23
    ;mov R0, a
    ;clr c
    ;clr a
    ret
ReadChannel:

    push b
    mov b, #3 ; ADC channel we want to read pased in register b
    lcall LTC2308_RW
    Wait_Milli_Seconds(#5)
    mov b, #3
    lcall LTC2308_RW ; [R1,R0] has the 12-bits from the converter now
    pop b
    ret
    
;Read Temperature from Channel 3 of ADC,converts the voltage reading to temperature,
;then displays it on HEX display, LCD display, and Putty
ReadConvert:

    lcall ReadChannel        ; Reads voltage on Channel 3
    lcall Voltage2Temp       ; Converts voltage reading to celsius
    lcall hex2bcd16          ; Convert to bcd
    lcall SendNumber         ; Send to serial port 
    ret
    
Display:
	lcall Display_BCD_7_Seg  ; Display using the 7-segment displays
	ret
    
Initialize_ADC:
    ; Initialize SPI pins connected to LTC2308
    clr LTC2308_MOSI
    clr LTC2308_SCLK
    setb LTC2308_ENN
    ret
    
;This routine uses bitbang communication to read voltage on channel 3
LTC2308_RW:
    clr a 
    clr LTC2308_ENN ; Enable ADC

    ; Send 'S/D', get bit 11
    setb c ; S/D=1 for single ended conversion
    lcall LTC2308_Toggle_Pins
    mov acc.3, c
    ; Send channel bit 0, get bit 10
    mov c, b.2 ; O/S odd channel select
    lcall LTC2308_Toggle_Pins
    mov acc.2, c 
    ; Send channel bit 1, get bit 9
    mov c, b.0 ; S1
    lcall LTC2308_Toggle_Pins
    mov acc.1, c
    ; Send channel bit 2, get bit 8
    mov c, b.1 ; S0
    lcall LTC2308_Toggle_Pins
    mov acc.0, c
    mov R1, a
    
    ; Now receive the lest significant eight bits
    clr a 
    ; Send 'UNI', get bit 7
    setb c ; UNI=1 for unipolar output mode
    lcall LTC2308_Toggle_Pins
    mov acc.7, c
    ; Send 'SLP', get bit 6
    clr c ; SLP=0 for NAP mode
    lcall LTC2308_Toggle_Pins
    mov acc.6, c
    ; Send '0', get bit 5
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.5, c
    ; Send '0', get bit 4
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.4, c
    ; Send '0', get bit 3
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.3, c
    ; Send '0', get bit 2
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.2, c
    ; Send '0', get bit 1
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.1, c
    ; Send '0', get bit 0
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.0, c
    mov R0, a

    setb LTC2308_ENN ; Disable ADC

    ret
    
LTC2308_Toggle_Pins:
    mov LTC2308_MOSI, c
    setb LTC2308_SCLK
    mov c, LTC2308_MISO
    clr LTC2308_SCLK
    ret
    
Left_blank mac
    mov a, %0
    anl a, #0xf0
    swap a
    jz Left_blank_%M_a
    ljmp %1
Left_blank_%M_a:
    Display_char(#' ')
    mov a, %0
    anl a, #0x0f
    jz Left_blank_%M_b
    ljmp %1
Left_blank_%M_b:
    Display_char(#' ')
endmac

;This is for Displaying arithmetic results in LCD
;It hides the zeros to the left of the Least significant byte of bcd data
;eg, instead of displaying '0025' we only see '25' on LCD display.

; Send a 4-digit BCD number stored in [R3,R2] to the serial port    
SendNumber:

    ;Indents the characters on putty to the left

    
    mov a, #'\r'
    lcall putchar
    
    mov a, #'\n'
    lcall putchar
        
    mov a, R3       
    anl a, #0x0f
    orl a, #'0'
    lcall putchar
    
    mov a, R2
    swap a
    anl a, #0x0f
    orl a, #'0'
    lcall putchar
    
    mov a, R2
    anl a, #0x0f
    orl a, #'0'
    lcall putchar
    
    ret

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; We use the pins of P0 to control the LCD.  Configure as outputs.
    mov P0MOD, #11111111b ; P0.0 to P0.7 are outputs.  ('1' makes the pin output)
	mov P2MOD, #00011111b
    ; We use pins P1.0 and P1.1 as outputs also.  Configure accordingly.
    mov P1MOD, #00000011b ; P1.0 and P1.0 are outputs
    ; Turn off all the LEDs
    mov LEDRA, #0 ; LEDRA is bit addressable
    mov LEDRB, #0 ; LEDRB is NOT bit addresable
    setb EA   ; Enable Global interrupts
    lcall ELCD_4BIT ; Configure LCD in four bit mode
    ; For convenience a few handy macros are included in 'LCD_4bit_DE1SoC.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Default_time)
    Set_Cursor(2, 1)
    Send_Constant_String(#Default_state)
	;Serial Port Initialization
   	lcall Initialize_Serial_Port
	;ADC Initialization
	lcall Initialize_ADC 
	
	;lcall Write_to_Memory
	
	mov dptr, #0x10
	lcall FT93C66_Read
	mov Soak_temp+0,a
	mov dptr, #0x14
	lcall FT93C66_Read
	mov Soak_temp+1,a
	mov dptr, #0x20
	lcall FT93C66_Read
	mov Soak_time+0,a
	mov dptr, #0x24
	lcall FT93C66_Read
	mov Soak_time+1,a
	mov dptr, #0x30
	lcall FT93C66_Read
	mov Reflow_temp+0,a
	mov dptr, #0x34
	lcall FT93C66_Read
	mov Reflow_temp+1,a
	mov dptr, #0x40
	lcall FT93C66_Read
	mov Reflow_time+0,a
	mov dptr, #0x44
	lcall FT93C66_Read
	mov Reflow_time+1,a
	
	;
	mov Soak_temp+0,   #0x50
	mov Soak_temp+1,   #0x00
    mov Soak_time+0,   #0x80
    mov Soak_time+1,   #0x00	
    mov Reflow_temp+0, #0x01
    mov Reflow_temp+1, #0x02
    mov Reflow_time+0, #0x30
    mov Reflow_time+1, #0x00
	;
	
	; Initialize variables
    mov FSM2_state, #0
    mov FSM3_state, #0
    mov FSM4_state, #0
    mov Mode_sel,   #1      

    
    mov Temperature,   #0
    mov Current_state, #0
    mov state, #0 
    clr PWMPIN
    mov pwm, #255
    clr PWM_flag
    setb seconds_flag
    clr Process_flag
    setb Reset_flag
    mov BCD_counter+1, #0x00 ; Initialize counter to zero
    mov BCD_counter+0, #0x75 ; Initialize counter to zero
    mov loop_incrementer, #0
    clr SOUND_OUT
	
	; After initialization the program stays in this 'forever' loop
loop:
	mov a, loop_incrementer
	cjne a, #255, main_loop
	lcall Display_mode
	lcall ReadConvert
	lcall Display
	mov Temperature+0, R0
    mov Temperature+1, R1
    mov loop_incrementer, #0
    sjmp main_loop
    
    ;add Temperature+0, #23
    ;mov a, Temperature
    ;add a, #0x17
    ;mov Temperature,a 
    
    ; Note: This beginning code (FSM1-FSM4) was provided to us. They only are used to 
    ; detect (with debounce) when either KEY1, KEY2, or KEY3 are pressed.
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; non-blocking state machine for KEY1 starts here
main_loop:
	mov a, FSM2_state
FSM2_state0:
	cjne a, #0, FSM2_state1
	jb KEY.1, FSM2_done
	mov FSM2_timer, #0
	inc FSM2_state
	sjmp FSM2_done
FSM2_state1:
	cjne a, #1, FSM2_state2
	; this is the debounce state
	inc FSM2_state
	sjmp FSM2_done	
FSM2_state2:
	cjne a, #2, FSM2_state3
	jb KEY.1, FSM2_state2b
	inc FSM2_state
	sjmp FSM2_done	
FSM2_state2b:
	mov FSM2_state, #0
	sjmp FSM2_done
FSM2_state3:
	cjne a, #3, FSM2_done
	jnb KEY.1, FSM2_done
	setb Key1_flag ; Suscesfully detected a valid KEY1 press/release
	mov FSM2_state, #0	
FSM2_done:
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; non-blocking state machine for KEY2 starts here
	mov a, FSM3_state
FSM3_state0:
	cjne a, #0, FSM3_state1
	jb KEY.2, FSM3_done
	mov FSM3_timer, #0
	inc FSM3_state
	sjmp FSM3_done
FSM3_state1:
	cjne a, #1, FSM3_state2
	; this is the debounce state
	inc FSM3_state
	sjmp FSM3_done	
FSM3_state2:
	cjne a, #2, FSM3_state3
	jb KEY.2, FSM3_state2b
	inc FSM3_state
	sjmp FSM3_done	
FSM3_state2b:
	mov FSM3_state, #0
	sjmp FSM3_done
FSM3_state3:
	cjne a, #3, FSM3_done
	jnb KEY.2, FSM3_done
	setb Key2_flag ; Suscesfully detected a valid KEY2 press/release
	mov FSM3_state, #0	
FSM3_done:
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; non-blocking state machine for KEY3 starts here
	mov a, FSM4_state
FSM4_state0:
	cjne a, #0, FSM4_state1
	jb KEY.3, FSM4_done
	mov FSM4_timer, #0
	inc FSM4_state
	sjmp FSM4_done
FSM4_state1:
	cjne a, #1, FSM4_state2
	; this is the debounce state
	inc FSM4_state
	sjmp FSM4_done	
FSM4_state2:
	cjne a, #2, FSM4_state3
	jb KEY.3, FSM4_state2b
	inc FSM4_state
	sjmp FSM4_done	
FSM4_state2b:
	mov FSM4_state, #0
	sjmp FSM4_done
FSM4_state3:
	cjne a, #3, FSM4_done
	jnb KEY.3, FSM4_done
	setb Key3_flag ; Suscesfully detected a valid KEY3 press/release
	mov FSM4_state, #0	
FSM4_done:
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; If KEY0 was detected, change the mode of interface (1 for default, 2 for Soak, 3 for Reflow)
	inc loop_incrementer
	jbc Key1_flag, Change_mode
	ljmp Skip_Change_mode
Change_mode:
	mov a, Mode_sel
	add a, #1
	cjne a, #4, Restore_mode
	mov a, #1
Restore_mode:
	mov Mode_sel, a
	;lcall Display_mode
	clr a 
    ljmp loop
;                        1234567890123456    <- This helps determine the location of the counter
;Default_time:       db 'Run-time:   ', 0
;SPACE:				 db ' ',0
;Default_state:      db 'State:', 0
;Soak_tempS:         db 'Soak Temp:', 0
;Soak_timeS:         db 'Soak Time:', 0
;Reflow_tempS:       db 'Reflow Temp:', 0
;Reflow_timeS:       db 'Reflow Time: ', 0
;READY:				 db 'READY', 0
;RAMP2SOAK:			 db 'RAMP2SOAK',0
;SOAK:				 db 'SOAK',0
;RAMP2PEAK:			 db 'RAMP2PEAK',0
;REFLOW:			 db 'REFLOW',0
;COOLING:		     db 'COOLING',0

;Displays the mode depending on the value of Mode_sel
;Displays the mode depending on the value of Mode_sel
Not_mode_1jump:
    ljmp Not_mode_1
Display_mode:
	mov a, Mode_sel
	cjne a, #1, Not_mode_1jump
	Set_Cursor(1, 1)
	Send_Constant_String(#Default_time)
	Set_Cursor(2, 1)
	Send_Constant_String(#Default_state)
	mov a, BCD_counter+0
	da a
	mov BCD_counter+0, a
	Set_Cursor(1, 15)
	Display_BCD(BCD_counter+0)
	mov a, BCD_counter+1
	da a
	mov BCD_counter+1, a
	Set_Cursor(1, 13)
	Display_BCD(BCD_counter+1)
	Set_Cursor(2, 8)
	Set_Cursor(1, 13)
	Send_Constant_String(#SPACE)
	lcall Display_state
	ret 
Not_mode_2jump:
    ljmp Not_mode_2
Not_mode_1:
	mov a, Mode_sel
	cjne a, #2, Not_mode_2jump
	Set_Cursor(1, 1)
	Send_Constant_String(#Soak_tempS)
	Set_Cursor(2, 1)
	Send_Constant_String(#Soak_timeS)
	mov a, Soak_temp+0
	da a
	mov Soak_temp+0, a
	Set_Cursor(1, 15)
	Display_BCD(Soak_temp+0)
	mov a, Soak_temp+1
	da a
	mov Soak_temp+1, a
	Set_Cursor(1, 13)
	Display_BCD(Soak_temp+1)
	mov a, Soak_time+0
	da a
	mov Soak_time+0, a
	Set_Cursor(2, 15)
	Display_BCD(Soak_time+0)
	mov a, Soak_time+1
	da a
	mov Soak_time+1, a
	Set_Cursor(2, 13)
	Display_BCD(Soak_time+1)
	Set_Cursor(1, 13)
	Send_Constant_String(#SPACE)
	Set_Cursor(2, 13)
	Send_Constant_String(#SPACE)
	clr a
	ret 
returnjump:
    ljmp Return
Not_mode_2:
	mov a, Mode_sel
	cjne a, #3, returnjump
	Set_Cursor(1, 1)
	Send_Constant_String(#Reflow_tempS)
	Set_Cursor(2, 1)
	Send_Constant_String(#Reflow_timeS)
	mov a, Reflow_temp+0
	da a
	mov Reflow_temp+0, a
	Set_Cursor(1, 15)
	Display_BCD(Reflow_temp+0)
	mov a, Reflow_temp+1
	da a
	mov Reflow_temp+1, a
	Set_Cursor(1, 13)
	Display_BCD(Reflow_temp+1)
	mov a, Reflow_time+0
	da a
	mov Reflow_time+0, a
	Set_Cursor(2, 15)
	Display_BCD(Reflow_time+0)
	mov a, Reflow_time+1
	da a
	mov Reflow_time+1, a
	Set_Cursor(2, 13)
	Display_BCD(Reflow_time+1)
	Set_Cursor(1, 13)
	Send_Constant_String(#SPACE)
	Set_Cursor(2, 13)
	Send_Constant_String(#SPACE)
	clr a
Return:
	ret

;Displays the current state depending on the value of state 
;(0 for inactive, 1 for Ramp-to-Soak, 2 for Soak, 3 for Ramp-to-Peak, 4 for Reflow, 5 for Cooling)
;NOTE: might need to remove this from the interface controller and move it into FSM for the actual
;reflow control, since it won't update after it enters that loop
Display_state:
	mov a, state
	Set_Cursor(2, 7)
	cjne a, #0, Not_state_0
	Send_Constant_String(#READY)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	clr a
	ret
Not_state_0:
	mov a, state
	Set_Cursor(2, 7)
	cjne a, #1, Not_state_1
	Send_Constant_String(#RAMP2SOAK)
	Send_Constant_String(#Space)
	clr a
	ret
Not_state_2_jumper:
	ljmp Not_state_2
Not_state_1:
	mov a, state
	Set_Cursor(2, 7)
	cjne a, #2, Not_state_2_jumper
	Send_Constant_String(#SOAK)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	clr a
	ret
 Not_state_2:
	mov a, state
	Set_Cursor(2, 7)
	cjne a, #3, Not_state_3
	Send_Constant_String(#RAMP2PEAK)
	clr a
	ret
Not_state_3:
	mov a, state
	Set_Cursor(2, 7)
	cjne a, #4, Not_state_4
	Send_Constant_String(#REFLOW   )
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	clr a
	ret
Not_state_4:
	mov a, state
	Set_Cursor(2, 7)
	Send_Constant_String(#COOLING  )
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	Send_Constant_String(#Space)
	clr a
	ret
loop_jumper:
	ljmp loop
;------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------
; If KEY2 was detected, change the value of temperature or time (still dependant on the mode)
Change_time_jumper:
	ljmp Change_time
Skip_Change_mode:
    jbc Key2_flag, Check_switch
	ljmp Skip_Change_time
Check_switch:
	jb SWA.1, Change_time_jumper
	ljmp Change_temp
Change_temp:
    mov a, Mode_sel
	cjne a, #1, Change_temp_soak
    ljmp loop
Change_temp_soak:
    mov a, Mode_sel
	cjne a, #2, Change_temp_reflow
    jb SWA.2, Decrement_temp_soak
	mov a, Soak_temp+0
	cjne a, #0x99, Soak_temp_inc
	inc Soak_temp+1
	mov Soak_temp+0, #0
	lcall Write_to_Memory
	ljmp loop_jumper
Soak_temp_inc:
	inc Soak_temp+0
	lcall Write_to_Memory
    ljmp loop_jumper
Change_temp_reflow:
    mov a, Mode_sel
	cjne a, #3, loop_jumper
    jb SWA.2, Decrement_temp_reflow
	mov a, Reflow_temp+0
	cjne a, #0x99, Reflow_temp_inc
	inc Reflow_temp+1
	mov Reflow_temp+0, #0
	lcall Write_to_Memory
	ljmp loop_jumper
Reflow_temp_inc:
	inc Reflow_temp+0
	lcall Write_to_Memory
    ljmp loop_jumper
Decrement_temp_soak:
    mov a, Soak_temp+0
    cjne a, #0x01, Soak_temp_dec
    dec Soak_temp+1
    mov Soak_temp+0, #0
    lcall Write_to_Memory
    ljmp loop_jumper
Soak_temp_dec:
    dec Soak_temp+0
    lcall Write_to_Memory
    ljmp loop_jumper
Decrement_temp_reflow:
    mov a, Reflow_temp+0
    cjne a, #0x01, Reflow_temp_dec
    dec Reflow_temp+1
    clr a
    mov a, Reflow_temp+1
    da a 
    mov Reflow_temp+1, a
    mov Reflow_temp+0, #0
    lcall Write_to_Memory
    ljmp loop_jumper
Reflow_temp_dec:
    dec Reflow_temp+0
    clr a
    mov a, Reflow_temp+0
    da a 
    mov Reflow_temp+0, a
    lcall Write_to_Memory
    ljmp loop_jumper
Change_time:
    mov a, Mode_sel
	cjne a, #1, Change_time_soak
    ljmp loop_jumper
Change_time_soak:
    mov a, Mode_sel
	cjne a, #2, Change_time_reflow
    jb SWA.2, Decrement_time_soak
	mov a, Soak_time+0
	cjne a, #0x99, Soak_time_inc
	inc Soak_time+1
	mov Soak_time+0, #0
	lcall Write_to_Memory
	ljmp loop_jumper
Soak_time_inc:
	inc Soak_time+0
	lcall Write_to_Memory
    ljmp loop_jumper
loop_jumper2:
	ljmp loop_jumper
Change_time_reflow:
    mov a, Mode_sel
	cjne a, #3, loop_jumper2
    jb SWA.2, Decrement_time_reflow
    mov a, Reflow_time+0
	cjne a, #0x99, Reflow_time_inc
	inc Reflow_time+1
	mov Reflow_time+0, #0
	lcall Write_to_Memory
	ljmp loop_jumper
Reflow_time_inc:
	inc Reflow_time+0
	lcall Write_to_Memory
    ljmp loop_jumper
Decrement_time_soak:
    mov a, Soak_time+0
    cjne a, #0x01, Soak_time_dec
    dec Soak_time+1
    clr a
    mov a, Soak_time+1
    da a 
    mov Soak_time+1, a
    mov Soak_time+0, #0
    lcall Write_to_Memory
    ljmp loop_jumper
Soak_time_dec:
    dec Soak_time+0
    mov a, Soak_time+0
    da a 
    mov Soak_time+0, a
    lcall Write_to_Memory
    ljmp loop_jumper
Decrement_time_reflow:
    mov a, Reflow_time+0
    cjne a, #0x01, Reflow_time_dec
    dec Reflow_time+1
    clr a
    mov a, Reflow_time+1
    da a 
    mov Reflow_time+1, a
    mov Reflow_time+0, #0
    lcall Write_to_Memory
    ljmp loop_jumper
Reflow_time_dec:
    dec Reflow_time+0
    mov a, Reflow_time+0
    da a
    mov Reflow_time+0, a
    lcall Write_to_Memory
    ljmp loop_jumper
;------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------
; If KEY3 was detected, we invert the reset flag and decide what to do (execute the 
; reflow process or reset all variables and stop proccess)
loop_jmp_s:
	;Buzzer off
	jb short_beep_flag, short_beep
	jb long_beep_flag, long_beep
	ljmp loop
short_beep:
	mov a, Buzzer_counter
	cjne a, #0x01, lp_jper
	clr SOUND_OUT
	clr short_beep_flag
	ljmp loop
long_beep:
	mov a, Buzzer_counter
	cjne a, #0x04, lp_jper
	clr SOUND_OUT
	clr long_beep_flag
	ljmp loop
lp_jper:
	ljmp loop
	
Skip_Change_time:
    jbc Key3_flag, Invert_reset
	ljmp Check_proccess_start
Invert_reset:
    cpl Reset_flag
Check_reset:
    jnb Reset_flag, Start_timer
    ljmp Reset_process
Check_proccess_start:
    jnb Process_flag, loop_jmp_s
    ljmp Begin_process
Start_timer:
	;Now clear the BCD counter
	mov BCD_counter+0, #0x01
	mov BCD_counter+1, #0x00
	mov state, #1
    setb Process_flag
    setb short_beep_flag
    mov Buzzer_counter, #0x00
    cpl SOUND_OUT
Begin_process:
    mov a, state
state1:
	;setb LEDRA.1
	mov pwm, #0
	mov a, state
	cjne a, #1, state2
	mov a, Temperature+0
	cjne a, #50, loop_jmp_s
	
	inc state
	setb LEDRA.1
	mov Soak_counter+0, #0x01
	mov Soak_counter+1, #0x00
	mov Buzzer_counter, #0x00
	setb short_beep_flag
	cpl SOUND_OUT
state2:
	; short beep
	mov pwm, #204
	mov a, state
	cjne a, #2, state3
	mov a, Soak_counter+0
	cjne a, #0x10, loop_jmp_s
	
	inc state
	setb LEDRA.2
	mov Buzzer_counter, #0x00
	setb short_beep_flag
	cpl SOUND_OUT
state3:
	; short beep
	mov pwm, #0
	mov a, state
	cjne a, #3, state4
	mov a, Temperature+0
	cjne a, #110, loop_jmp_K
	inc state
	setb LEDRA.3
	mov Soak_counter+0, #0x01
	mov Soak_counter+1, #0x00
	mov Buzzer_counter, #0x00
	setb short_beep_flag
	cpl SOUND_OUT
state4:
	; short beep
	mov pwm, #204
	mov a, state
	cjne a, #4, state5
	mov a, Soak_counter+0
	cjne a, #0x10, loop_jmp_K
	
	inc state
	setb LEDRA.4
	mov Buzzer_counter, #0x00
	setb short_beep_flag
	cpl SOUND_OUT
state5:
	; long beep
	mov pwm, #255
	mov a, state
	cjne a, #5, state6
	mov a, Temperature+0
	cjne a, #60, loop_jmp_K
	
	inc state
	setb LEDRA.5
	mov Buzzer_counter, #0x00
	setb long_beep_flag
	cpl SOUND_OUT
state6: 
	; six beeps
	mov pwm, #255
	mov a, state
	cjne a, #6, loop_jmp_K
	mov Buzzer_counter, #0x00
	setb long_beep_flag
	cpl SOUND_OUT
loop_jmp_K:
	ljmp loop_jmp_s
Reset_process:
;	clr TR2 ; Stop timer 2
;	clr Process_flag
;	mov pwm, #255
    ;ljmp loop

END
;    cjne a, #1, state2
  ;  mov pwm, #255
;    mov a, Soak_temp+0
;    clr c
;    subb a, Temperature+0
;    jnc state1_done
;    mov a, Soak_temp+1
;    clr c
;    subb a, Temperature+1
;    jnc state1_done
;    mov state, #2
;    mov a, BCD_counter
;    add a, Soak_time
;    mov Total_Soak_time, a
;state1_done:
;    ljmp loop
;state2:
;    cjne a, #2, state3
; ;   mov pwm, #255
;    mov a, Total_Soak_time
;    clr c
;    subb a, BCD_counter
;    jnc state2_done
;    mov state, #3
;state2_done:
;    ljmp loop
;state3:
;    cjne a, #3, state4
; ;   mov pwm, #0
;    mov a, Reflow_temp
 ;   clr c
;    subb a, Temperature
 ;   jnc state3_done
 ;   mov state, #4
;    mov a, BCD_counter
;    add a, Reflow_time
;    mov Total_Reflow_time, a
;state3_done:
;    ljmp loop
;state4:
;    cjne a, #4, state5
;  ;  mov pwm, #204
;    mov a, Total_Reflow_time
;    clr c
;    subb a, BCD_counter
;    jnc state4_done
;    mov state, #5
;state4_done:
;    ljmp loop
;state5:
;    cjne a, #5, state6
; ;   mov pwm, #255
;    mov a, Room_temp
;    clr c
;    subb a, Temperature
;    jnc state5_done
;    mov state, #6
;state5_done:
;    ljmp loop
;state6:
;    ;Reached room temp
;    setb Buzzer_flag
;	ljmp loop