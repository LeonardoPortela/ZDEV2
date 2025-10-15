*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
* Cliente....: Maggi                                                   *
* Autor......: Daniela Machado                                         *
* Data.......: 21.07.2010                                              *
* Descrição  : Criação / Cancelamento da MIGO no SAP via SIGAM         *
* Projeto....: Maggi - Projeto Evoluir                                 *
* Cód Espec..: GAP_MM02                                                *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*
FUNCTION z_mm_inbound_eezgr_migo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IB_MOVIMENTO_ESTOQUE TYPE  ZMMT001
*"      IB_ESTORNO_ENTRADA TYPE  ZMMT003
*"----------------------------------------------------------------------

  data: wa_zmmt_ee_zgr like zmmt_ee_zgr.

  REFRESH: it_zmmt_ee_zgr,
           it_zmmt_eee_zgr.

  CLEAR: wa_mov_estq,
         wa_estorno.

  IF NOT ib_movimento_estoque[] IS INITIAL.
    LOOP AT ib_movimento_estoque into wa_mov_estq.
      move-corresponding wa_mov_estq to wa_zmmt_ee_zgr.
      append wa_zmmt_ee_zgr to it_zmmt_ee_zgr.
    ENDLOOP.
    "MOVE ib_movimento_estoque[] TO it_zmmt_ee_zgr[].
    PERFORM yf_mov_estoque_estorno TABLES ib_movimento_estoque
                                   USING  wa_mov_estq.
    MODIFY zmmt_ee_zgr FROM TABLE it_zmmt_ee_zgr.
  ENDIF.

  IF NOT ib_estorno_entrada[] IS INITIAL.
    MOVE ib_estorno_entrada[] TO it_zmmt_eee_zgr[].
    PERFORM yf_mov_estoque_estorno TABLES ib_estorno_entrada
                                   USING  wa_estorno.
    MODIFY zmmt_eee_zgr FROM TABLE it_zmmt_eee_zgr.
  ENDIF.

* Chamar função assíncrona de retorno, confirmando a gravação
* de dados
  IF NOT it_outreturn[] IS INITIAL.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = it_outreturn.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = it_outreturn.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = it_outreturn.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

  ENDIF.

  COMMIT WORK.

ENDFUNCTION.
