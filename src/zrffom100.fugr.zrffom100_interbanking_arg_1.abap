FUNCTION ZRFFOM100_INTERBANKING_ARG_1.
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_FORMAT) LIKE  T042OFI-FORMT
*"     VALUE(I_REGUH) LIKE  REGUH STRUCTURE  REGUH
*"     VALUE(I_DTAM100S) LIKE  DTAM100S STRUCTURE  DTAM100S
*"     VALUE(I_DTAM100H) LIKE  DTAM100H STRUCTURE  DTAM100H
*"     VALUE(I_CBXX) LIKE  RFPDO2-FORDCBXX
*"  EXPORTING
*"     VALUE(E_DTAM100H) LIKE  DTAM100H STRUCTURE  DTAM100H
*"  TABLES
*"      T_SUM_PER_CURRENCY STRUCTURE  DTAM100C
*"--------------------------------------------------------------------
  CASE I_FORMAT.

     WHEN 'MT100_CHASE'.
        CALL FUNCTION 'FI_MT100_0101'
             EXPORTING
                  I_REGUH    =  I_REGUH
                  I_DTAM100S =  I_DTAM100S
                  I_DTAM100H =  I_DTAM100H
            IMPORTING
                 E_DTAM100H =  E_DTAM100H
             EXCEPTIONS
                  OTHERS     = 0.

     WHEN 'MT101'.
        CALL FUNCTION 'FI_MT101_1'
             EXPORTING
                  I_REGUH    =  I_REGUH
                  I_DTAM100S =  I_DTAM100S
                  I_DTAM100H =  I_DTAM100H
            IMPORTING
                 E_DTAM100H =  E_DTAM100H
             EXCEPTIONS
                  OTHERS     = 0.

     WHEN 'ORBIAN'.
        CALL FUNCTION 'FI_ORBIAN_HEADER_1'
          EXPORTING
            I_REGUH          = I_REGUH
            I_DTAM100S       = I_DTAM100S
            I_DTAM100H       = I_DTAM100H
          IMPORTING
            E_DTAM100H       = E_DTAM100H
          EXCEPTIONS
            OTHERS           = 0.

     WHEN 'ORBIAN_DISCOUNT'.
        CALL FUNCTION 'FI_ORBIAN_HEADER_1'
          EXPORTING
            I_REGUH          = I_REGUH
            I_DTAM100S       = I_DTAM100S
            I_DTAM100H       = I_DTAM100H
          IMPORTING
            E_DTAM100H       = E_DTAM100H
          EXCEPTIONS
            OTHERS           = 0.

     WHEN 'INTERBANKING_ARGENTINA'.
       CALL FUNCTION 'ZRFFOM_HEADER_INTERBANKING_ARG'
          EXPORTING
            I_REGUH          = I_REGUH
            I_DTAM100S       = I_DTAM100S
            I_DTAM100H       = I_DTAM100H
          IMPORTING
            E_DTAM100H       = E_DTAM100H
          EXCEPTIONS
            OTHERS           = 0.

     WHEN OTHERS.
       E_DTAM100H = I_DTAM100H.

   ENDCASE.

ENDFUNCTION.
