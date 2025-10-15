FUNCTION z_ov_check_liberacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_SIMU) TYPE  ZSDED003
*"     REFERENCE(IV_VBELN) TYPE  VBELN_VA
*"     REFERENCE(IV_KWERT) TYPE  NETWR_AK
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"     REFERENCE(EV_SEM_LIMITE) TYPE  FLAG
*"     REFERENCE(EV_SALDO_ORIGEM) TYPE  ZSDE_SALDO_ORIGEM
*"     REFERENCE(ET_MESSAGE) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------

  DATA lv_msgx TYPE c LENGTH 50.
  DATA lv_saldo TYPE netwr_ak.
  DATA lv_sem_sap.
  DATA lv_ok.

  CHECK iv_simu IS NOT INITIAL AND iv_vbeln IS NOT INITIAL.

  PERFORM f_existe_checklist
    USING iv_simu
          iv_vbeln
   CHANGING lv_ok.

  IF lv_ok = abap_false.

    lv_msgx = `Simulador ` && iv_simu && ` não possui checklist ativo`.
    et_message = VALUE bapiret2_tab(  ( type = 'E' id = 'DS' number = '016' message = lv_msgx ) ).
    ev_erro = abap_true.
    EXIT.

  ENDIF.

  SELECT SINGLE * FROM zsd_in_est_limite_ov_01
    INTO @DATA(ls_limite)
      WHERE doc_simulacao = @iv_simu
        AND vbeln = @iv_vbeln.

  IF sy-subrc NE 0.

    ev_erro = abap_true.
    et_message = VALUE bapiret2_tab(  ( type = 'E' id = 'DS' number = '016' message = 'Sem saldo cadastrado' ) ).
    EXIT.

  ENDIF.

  " verifica se o limite do sap é maior ou igual ao valor que se deseja liberar
  IF ls_limite-valor_limite_disponivel_sap >= iv_kwert.

    " se sim, vai ser usado limite do sap
    ev_saldo_origem = 'S'. "<- origem SAP
    ev_erro = abap_false.
    EXIT.

  ENDIF.

  " verifica se o limite do OPUS é maior ou igual ao valor que se deseja liberar
  IF ls_limite-valor_limite_disponivel_opus >= iv_kwert.

    " se sim, vai ser usado limite do OPUS
    ev_saldo_origem = 'O'. "<- origem OPUS
    ev_erro = abap_false.
    EXIT.

  ENDIF.

  " se não entrou em nenhum, então está sem limite
  ev_sem_limite = abap_true.

  " sem erro
  ev_erro = abap_false.

  " mensagem sem limite
  et_message = VALUE bapiret2_tab(  ( type = 'E' id = 'DS' number = '016' message = 'Sem limite disponivel' ) ).


ENDFUNCTION.
