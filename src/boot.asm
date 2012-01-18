;*************************************************************************
;Name: R.P.I.T. Somasiri
;E-mail: ishanthilina@gmail.com
;-------------------------------------------------------------------------
;the ultimate boot-strap loader
;to load a file from a DOS FAT32 Disk as the OS
;*************************************************************************
[BITS 16]
[ORG 0x0000]
 jmp     START
    
     OEM_ID                db 		"QUASI-OS"
     BytesPerSector        dw 		0x0200
     SectorsPerCluster     db 		0x08
     ReservedSectors       dw 		0x0020
     TotalFATs             db 		0x02
     MaxRootEntries        dw 		0x0000
     NumberOfSectors       dw 		0x0000
     MediaDescriptor       db 		0xF8
     SectorsPerFAT         dw 		0x0000
     SectorsPerTrack       dw 		0x003D
     SectorsPerHead        dw 		0x0002
     HiddenSectors         dd 		0x00000000
     TotalSectors     	   dd 		0x00FE3B1F		
     BigSectorsPerFAT      dd 		0x00000778
     Flags                 dw 		0x0000
     FSVersion             dw 		0x0000
     RootDirectoryStart    dd 		0x00000002
     FSInfoSector          dw 		0x0001
     BackupBootSector      dw 		0x0006

     TIMES 13 DB 0 ;jumping to next offset

     DriveNumber           db 		0x00
     Signature             db 		0x29
     VolumeID              dd 		0xFFFFFFFF
     VolumeLabel           db 		"QUASI  BOOT"
     SystemID              db 		"FAT32   "
     
     START:
     ; code located at 0000:7C00, adjust segment registers
          cli
          mov     ax, 0x07C0
          mov     ds, ax
          mov     es, ax
          mov     fs, ax
          mov     gs, ax
     ; create stack
          mov     ax, 0x0000
          mov     ss, ax
          mov     sp, 0xFFFF
          sti



	  

      ; size of a cluster in sectors is stored in cx 
          mov    cx,  WORD[SectorsPerCluster]


     ; compute location of the begining of the Data area and store in ax
          mov     al, BYTE [TotalFATs]                ; Total number of FATs
          mul     WORD[BigSectorsPerFAT]                ; Number of sectors for a FAT
          add     ax, WORD [ReservedSectors]          ; Find the start of the Data area
          mov     WORD [datasector], ax               ; Store the begining of the Data area



     ; read 1st data cluster into memory (7C00:0200)
          mov     ax, WORD[RootDirectoryStart]
          call ClusterLBA
          mov     bx, 0x0200                          ; copy 1st data cluter above bootcode
          call    ReadSectors

   
     ; Point Index register to 1st File Entry
          mov     di, 0x0200 + 0x20                         

     
     

          mov     si, msgCRLF
          call    DisplayMessage
          
      ;Point to the offset where the file location information contains
      
          mov     dx, WORD [di + 0x001A]
          mov     WORD [cluster], dx                  




	   
          
	;Set up the segments where the kernel needs to be loaded
	
            mov ax, 0100h       ; set ES:BX = 0100:0000
            mov es, ax          
            mov bx, 0           
	   
	;Read the cluster which contains the kernel
            mov cx, 0x0008	
            mov ax, WORD[cluster]
            call ClusterLBA
            call ReadSectors
            
            
            


          mov     si, msgCRLF
          call    DisplayMessage
	
	;Jump to the location where the kernel was loded
	
          push    WORD 0x0100
          push    WORD 0x0000
          retf




	;An error has occured if this part is executed
          mov     si, msgFailure
          call    DisplayMessage
          mov     ah, 0x00
          int     0x16                                ; await keypress
          int     0x19                                ; warm boot computer







     ;*************************************************************************
     ; PROCEDURE ReadSectors
     ; reads cx sectors from disk starting at ax into
     ;memory location es:bx
     ;*************************************************************************
     ReadSectors:
     .MAIN
          mov     di, 0x0005                          ; five retries for error
     .SECTORLOOP
          push    ax
          push    bx
          push    cx
          call    LBACHS
          mov     ah, 0x02                            ; BIOS read sector
          mov     al, 0x01                            ; read one sector
          mov     ch, BYTE [absoluteTrack]            ; track
          mov     cl, BYTE [absoluteSector]           ; sector
          mov     dh, BYTE [absoluteHead]             ; head
          mov     dl, BYTE [DriveNumber]              ; drive
          int     0x13                                ; invoke BIOS
          jnc     .SUCCESS                            ; test for read error
          xor     ax, ax                              ; BIOS reset disk
          int     0x13                                ; invoke BIOS
          dec     di                                  ; decrement error counter
          pop     cx
          pop     bx
          pop     ax
          jnz     .SECTORLOOP                         ; attempt to read again
          int     0x18
     .SUCCESS
          mov     si, msgProgress
          call    DisplayMessage
          pop     cx
          pop     bx
          pop     ax
          add     bx, WORD [BytesPerSector]           ; queue next buffer
          inc     ax                                  ; queue next sector
          loop    .MAIN                               ; read next sector
          ret

     
     ;*************************************************************************
     ; PROCEDURE DisplayMessage
     ; display ASCIIZ string at ds:si via BIOS
     ;*************************************************************************
     DisplayMessage:
          lodsb                                       ; load next character
          or      al, al                              ; test for NUL character
          jz      .DONE
          mov     ah, 0x0E                            ; BIOS teletype
          mov     bh, 0x00                            ; display page 0
          mov     bl, 0x07                            ; text attribute
          int     0x10                                ; invoke BIOS
          jmp     DisplayMessage
     .DONE:
          ret
     
     ;*************************************************************************



     ;*************************************************************************
     ; PROCEDURE ClusterLBA
     ; convert FAT cluster into LBA addressing scheme
     ; FileStartSector = ((X − 2) * SectorsPerCluster(0x08))
     ;*************************************************************************
     ClusterLBA:
          sub     ax, 0x0002                          ; zero base cluster number
          xor     cx, cx
          mov     cl, BYTE [SectorsPerCluster]        ; convert byte to word
          mul     cx
          add     ax, WORD [datasector]               ; base data sector
          ret


     ;*************************************************************************
     ; PROCEDURE LBACHS
     ; convert ax LBA addressing scheme to CHS addressing scheme
     ; absolute sector = (logical sector / sectors per track) + 1
     ; absolute head   = (logical sector / sectors per track) MOD number of heads
     ; absolute track  = logical sector / (sectors per track * number of heads)
     ;*************************************************************************
     LBACHS:
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [SectorsPerTrack]              ; calculate
          inc     dl                                  ; adjust for sector 0
          mov     BYTE [absoluteSector], dl
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [SectorsPerHead]                     ; calculate
          mov     BYTE [absoluteHead], dl
          mov     BYTE [absoluteTrack], al
          ret
      ;************************************************************************

     absoluteSector db 0x00
     absoluteHead   db 0x00
     absoluteTrack  db 0x00

     cluster     dw 0x0000
     datasector  dw 0x0000

;*******************************************************************************
;messages that needs to be shown

     msgProgress db ".", 0x00
     msgFailure  db 0x0D, 0x0A, "Kernel loading failed...", 0x0D, 0x0A, 0x00
     msgCRLF     db 0x0D, 0x0A, 0x00
     
          TIMES 510-($-$$) DB 0
          DW 0xAA55
     ;*************************************************************************
