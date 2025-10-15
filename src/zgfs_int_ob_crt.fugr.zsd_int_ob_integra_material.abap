FUNCTION zsd_int_ob_integra_material.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_MATNR) TYPE  MATNR
*"----------------------------------------------------------------------

  ls_material-matnr      = i_matnr.
  ls_material-metodo     = 'GET'.

*-----------------------------------------------------------------------
* na criacao do material, verifica se ja foi criado
*-----------------------------------------------------------------------
  DO 10 TIMES.
    SELECT SINGLE matnr
      INTO @DATA(_matnr)
      FROM mara
     WHERE matnr = @i_matnr.

    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO 5 SECONDS.
    ENDIF.
  ENDDO.

*-----------------------------------------------------------------------
* integrar material - SAFRA - valida se material existe no SAFRA.
* Se nao existe, metodo = POST, senao metodo = PUT
*-----------------------------------------------------------------------
  TRY.
      zcl_int_ob_safra_crt_product=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = ls_material ).
      ls_material-metodo = 'PUT'.
    CATCH zcx_integracao INTO DATA(ex_integra).
      ls_material-metodo = 'POST'.
    CATCH zcx_error      INTO DATA(ex_error).
      ls_material-metodo = 'POST'.
  ENDTRY.

  TRY.
      zcl_int_ob_safra_crt_product=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = ls_material ).
    CATCH zcx_integracao INTO ex_integra.
    CATCH zcx_error      INTO ex_error.
  ENDTRY.

ENDFUNCTION.
