FUNCTION zpm_atualiza_nota.
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_NOTIFHEADER) TYPE  BAPI2080_NOTHDRI OPTIONAL
*"     VALUE(I_NOTIFHEADER_X) TYPE  BAPI2080_NOTHDRI_X OPTIONAL
*"     VALUE(I_NOTA) TYPE  QMEL-QMNUM OPTIONAL
*"  TABLES
*"      T_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------


  IF i_nota IS NOT INITIAL.

    CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
      EXPORTING
        number        = i_nota
        notifheader   = i_notifheader
        notifheader_x = i_notifheader_x
      TABLES
        return        = t_return.

    IF NOT line_exists( t_return[ type = 'E' ] ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.

  ENDIF.


ENDFUNCTION.
