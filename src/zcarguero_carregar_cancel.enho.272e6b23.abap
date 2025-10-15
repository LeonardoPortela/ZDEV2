"Name: \PR:SAPLV56I_BAPI\FO:MAP_BAPI_CHANGE_TO_ACTIVITIES\SE:BEGIN\EI
ENHANCEMENT 0 ZCARGUERO_CARREGAR_CANCEL.

  TRY .
      ZCL_INTEGRACAO_VIAGEM_CARRCAN=>ZIF_INTEGRACAO_VIAGEM_CARRCAN~GET_INSTANCE(
        )->SET_VIAGEM_CARRCAN(
        EXPORTING
          I_TKNUM                 = is_hdr-shipment_num    " Nº transporte
      ).
    CATCH ZCX_INTEGRACAO into data(ex_integracao).    "


      IF NOT ( EX_INTEGRACAO->MSGID EQ ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID AND
               EX_INTEGRACAO->MSGNO EQ ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO ).
         gf_flag_rollback = abap_true.
         MESSAGE ID ex_integracao->MSGID TYPE 'E' NUMBER ex_integracao->MSGNO WITH ex_integracao->MSGV1 ex_integracao->MSGV2 ex_integracao->MSGV3 ex_integracao->MSGV4
         RAISING PROCESSING_FAILED.
      ENDIF.

    CATCH ZCX_ERROR into data(ex_error).    " .
      gf_flag_rollback = abap_true.
      MESSAGE ID ex_error->MSGID TYPE 'E' NUMBER ex_error->MSGNO WITH ex_error->MSGV1 ex_error->MSGV2 ex_error->MSGV3 ex_error->MSGV4
      RAISING PROCESSING_FAILED.
  ENDTRY.

ENDENHANCEMENT.
