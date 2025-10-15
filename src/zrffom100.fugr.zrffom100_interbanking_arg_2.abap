FUNCTION ZRFFOM100_INTERBANKING_ARG_2 .
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_FORMAT) LIKE  T042OFI-FORMT
*"     VALUE(I_REGUH) LIKE  REGUH STRUCTURE  REGUH
*"     VALUE(I_DTAM100) LIKE  DTAM100 STRUCTURE  DTAM100
*"  EXPORTING
*"     VALUE(E_DTAM100) LIKE  DTAM100 STRUCTURE  DTAM100
*"  TABLES
*"      T_REGUP STRUCTURE  REGUP
*"      T_DTAM100V STRUCTURE  DTAM100V
*"--------------------------------------------------------------------

  CASE I_FORMAT.
     WHEN 'HR_GALICIA'.
          CALL FUNCTION 'DME_AR_HR_GALICIA_2'
               EXPORTING
                    I_REGUH    = I_REGUH
                    I_DTAM100  = I_DTAM100
               IMPORTING
                    E_DTAM100  = E_DTAM100
               TABLES
                    T_REGUP    = T_REGUP
                    T_DTAM100V = T_DTAM100V.

     WHEN 'INTERBANKING_ARGENTINA'.
          CALL FUNCTION 'ZRFFOM_ITEM_INTERBANKING_ARG'
               EXPORTING
                    I_REGUH    = I_REGUH
                    I_DTAM100  = I_DTAM100
               IMPORTING
                    E_DTAM100  = E_DTAM100
               TABLES
                    T_REGUP    = T_REGUP
                    T_DTAM100V = T_DTAM100V.
     WHEN OTHERS.
          E_DTAM100 = I_DTAM100.
    ENDCASE.

ENDFUNCTION.
