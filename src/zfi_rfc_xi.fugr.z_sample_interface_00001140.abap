FUNCTION Z_SAMPLE_INTERFACE_00001140.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_PARKED) TYPE  XFELD OPTIONAL
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"      T_EXCTAB STRUCTURE  EXCLTAB_LINE
*"----------------------------------------------------------------------

*  export buk = t_bkpf-bukrs
*         gja = t_bkpf-gjahr to memory id 'OUT-SIG'.
*
*  break brxs_basis.
*  clear wa_testetxt.
*  read table t_bseg index 1.
*  read table t_bkpf index 1.
*  concatenate   sy-uname 'BSEG:' t_bseg-bukrs t_bseg-belnr t_bseg-buzei
*   t_bseg-gjahr 'BKPF:' t_bkpf-bukrs t_bkpf-belnr t_bkpf-gjahr
*   sy-tcode sy-ucomm into wa_testetxt separated by space.
*
*  call function 'Z_GERALDOTESTE'
*    exporting
*      registro = wa_testetxt
*      funcao   = '00001140_E'.
*

ENDFUNCTION.
