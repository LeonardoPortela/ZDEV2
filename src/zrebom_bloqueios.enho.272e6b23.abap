"Name: \PR:SAPLV56I_BAPI\FO:MAP_BAPI_CHANGE_TO_ACTIVITIES\SE:BEGIN\EI
ENHANCEMENT 0 ZREBOM_BLOQUEIOS.
*
  data: lc_ID_PROC_CLIENTE TYPE zde_ID_PROC_CLIENTE,
        ck_autorizado      type char01,
        ck_cancelou        type char01,
        ck_excluiu         type char01,
        obj_pedagio        type REF TO ZCL_REPOM_VIAGEM_VPR,
        E_ERROS	           TYPE ZDE_REPOM_ERROS_T,
        w_ERROS	           TYPE ZDE_REPOM_ERROS.

  if is_hdr_action-shipment_num = gc_chg_delete .

    CALL METHOD ZCL_REPOM_VIAGEM_VPR=>GET_ID_PROC_CLIENTE_VT
      EXPORTING
        I_TKNUM           = is_hdr-shipment_num
      RECEIVING
        E_ID_PROC_CLIENTE = lc_ID_PROC_CLIENTE
      EXCEPTIONS
        NAO_ENCONTRADO    = 1
        OTHERS            = 2.

    if sy-subrc is INITIAL.

      CALL METHOD ZCL_REPOM_VIAGEM_VPR=>GET_AUTORIZADO
        EXPORTING
          I_ID_PROC_CLIENTE = lc_ID_PROC_CLIENTE
        RECEIVING
          E_AUTORIZOU       = ck_autorizado.

      CREATE OBJECT OBJ_PEDAGIO
        EXPORTING
          I_ID_PROC_CLIENTE = lc_ID_PROC_CLIENTE.

      "Se estivar autorizado cancela
      "Senão Exclui
      CASE ck_autorizado.
        WHEN abap_true.

          CALL METHOD OBJ_PEDAGIO->SOLICITAR_CANCELAMENTO
            IMPORTING
              E_ERROS                   = E_ERROS
            RECEIVING
              I_CANCELOU                 = ck_cancelou
            EXCEPTIONS
              SERVICO_NAO_ENCONTRADO     = 1
              HTTP_COMMUNICATION_FAILURE = 2
              HTTP_INVALID_STATE         = 3
              HTTP_PROCESSING_FAILED     = 4
              HTTP_INVALID_TIMEOUT       = 5
              ERRO                       = 6
              OTHERS                     = 7.

          IF SY-SUBRC is NOT INITIAL.
            gf_flag_rollback = abap_true.
            clear: OBJ_PEDAGIO.
            IF E_ERROS IS NOT INITIAL.
              LOOP AT E_ERROS into w_ERROS.
                MESSAGE E017(zrepom) WITH w_ERROS-ERRO_CODIGO w_ERROS-ERRO_DESCRICAO RAISING PROCESSING_FAILED.
              ENDLOOP.
            ELSE.
              MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING PROCESSING_FAILED.
            ENDIF.
          ELSEIF ck_cancelou EQ ABAP_FALSE.
            gf_flag_rollback = abap_true.
            CLEAR: OBJ_PEDAGIO.
            MESSAGE i064(zrepom) WITH lc_ID_PROC_CLIENTE.
          ENDIF.

        WHEN abap_false.

          CALL METHOD OBJ_PEDAGIO->ZIF_CADASTRO~EXCLUIR_REGISTRO
            RECEIVING
              I_EXCLUIU = ck_excluiu.

          if ck_excluiu eq abap_false.
            CLEAR: OBJ_PEDAGIO.
            gf_flag_rollback = abap_true.
            MESSAGE E064(zrepom) WITH lc_ID_PROC_CLIENTE RAISING PROCESSING_FAILED.
          endif.
      ENDCASE.

      CLEAR: OBJ_PEDAGIO.
    endif.
  endif.

ENDENHANCEMENT.
