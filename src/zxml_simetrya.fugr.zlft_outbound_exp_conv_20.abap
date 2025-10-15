FUNCTION ZLFT_OUTBOUND_EXP_CONV_20.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(FLAG) LIKE  ZLFT_SAIDA-IND_INTERFACE
*"     VALUE(RECORDS) LIKE  SOID-ACCNT
*"  TABLES
*"      FIELDS STRUCTURE  RFC_DB_FLD
*"      DATA STRUCTURE  ZLFT_EXP_CONV_20
*"      T_FILIAIS STRUCTURE  ZLFE_RFC_FILTER
*"----------------------------------------------------------------------

* Verifica autorização para uso da Função
  call function 'AUTHORITY_CHECK_RFC'
       exporting
         userid           = sy-uname
         functiongroup    = 'ZSATI_RFC'
       exceptions
         user_dont_exist  = 1
         rfc_no_authority = 2
         others           = 3.

  if sy-subrc <> 0.
     exit.
  endif.
* --------------------------------------------------------------------

  if flag = 'S'.
*    Get table structure
     call function 'Z_SATI_RFC_TRX_INIT'
          exporting
            table_name = 'ZLFT_EXP_CONV_20'
          tables
            tabfields  = fields.

*    Read data from the database to internal table
     select *
       into table DATA
       from ZLFT_EXP_CONV_20.
  else.
     delete from ZLFT_EXP_CONV_20.
     commit work.
  endif.


ENDFUNCTION.
