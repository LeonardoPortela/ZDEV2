FUNCTION ZLOG_SM20N_003.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ERDAT) TYPE  ERDAT
*"----------------------------------------------------------------------


  PERFORM f_email_aviso_inatividade USING i_erdat.



ENDFUNCTION.
