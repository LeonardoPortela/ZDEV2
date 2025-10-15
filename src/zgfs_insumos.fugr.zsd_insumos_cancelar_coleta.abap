FUNCTION zsd_insumos_cancelar_coleta.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ID_DOCUMENTO) TYPE  ZID_DOCUMENTO
*"     VALUE(I_CANCELAR_DOCUMENTO) TYPE  CHAR1 OPTIONAL
*"----------------------------------------------------------------------

*-----------------------------
* qual tipo de documento
*-----------------------------
  SELECT SINGLE *
    FROM zsdt0310
    INTO @DATA(w_0310)
   WHERE id_documento = @i_id_documento.

  CHECK sy-subrc = 0.

*-----------------------------
* Cancelamento documento no SIGAM
*-----------------------------
  IF w_0310-tipo_doc = 'CTR'.
    TRY .
        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
           )->set_cancelar_doc_sigam( EXPORTING i_id_documento    = i_id_documento ).

      CATCH zcx_integracao INTO DATA(ex_integra).
      CATCH zcx_error INTO DATA(ex_error).
    ENDTRY.
  ENDIF.

*-----------------------------
* Cancelamento documento na Bry
*-----------------------------
  IF w_0310-tipo_doc = 'CTR' OR w_0310-tipo_doc = 'OVD'.
    TRY .
        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
           )->set_cancelar_coleta( EXPORTING i_id_documento       = i_id_documento
                                             i_cancelar_documento = i_cancelar_documento ).

      CATCH zcx_integracao INTO ex_integra.
      CATCH zcx_error INTO ex_error.
    ENDTRY.
  ENDIF.

ENDFUNCTION.
