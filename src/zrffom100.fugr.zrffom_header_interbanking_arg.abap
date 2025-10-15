FUNCTION ZRFFOM_HEADER_INTERBANKING_ARG.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_REGUH) LIKE  REGUH STRUCTURE  REGUH
*"     VALUE(I_DTAM100S) LIKE  DTAM100S STRUCTURE  DTAM100S
*"     VALUE(I_DTAM100H) LIKE  DTAM100H STRUCTURE  DTAM100H
*"  EXPORTING
*"     VALUE(E_DTAM100H) LIKE  DTAM100H STRUCTURE  DTAM100H
*"----------------------------------------------------------------------

  DATA: BEGIN OF ls_interbanking,
          tipo(3)      TYPE c VALUE '*U*',
          cod(3)       TYPE n,
          tcta(2)      TYPE n VALUE '01', "CC $
          nrocta(17)   TYPE c,
          ind(1)       TYPE c VALUE 'D',  "Débito
          cdate(8)     TYPE d,
          marca(1)     TYPE c VALUE 'N',  "No Consolidado
          obs(61)      TYPE c VALUE space,
          zeros(3)     TYPE n VALUE '000',
          nroctad(2)   TYPE n VALUE '00',
          fdate(8)     TYPE c,
          nroseq(8)    TYPE c,
          espacio(123) TYPE c,
        END OF ls_interbanking.

  MOVE i_reguh-ubnkl      TO ls_interbanking-cod.
  MOVE i_reguh-ubknt      TO ls_interbanking-nrocta.
  MOVE i_reguh-zaldt      TO ls_interbanking-cdate.

  e_dtam100h     = ls_interbanking.
  e_dtam100h-h01 = strlen( ls_interbanking ).

ENDFUNCTION.
