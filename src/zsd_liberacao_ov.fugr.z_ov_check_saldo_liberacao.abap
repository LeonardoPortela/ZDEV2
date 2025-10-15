FUNCTION z_ov_check_saldo_liberacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_VBELN) TYPE  VBELN_VA
*"     REFERENCE(IV_POSNR) TYPE  POSNR_VA
*"     REFERENCE(IV_QTDE) TYPE  KWMENG
*"     REFERENCE(IV_PROC_NOVO) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"     REFERENCE(EV_MSGX) TYPE  CHAR255
*"----------------------------------------------------------------------

  DATA lv_qtd TYPE kwmeng.
  DATA lv_vlr TYPE netwr_ap.
  DATA lv_msg1 TYPE sy-msgv1.
  DATA lv_msg2 TYPE sy-msgv2.
  DATA lv_tole TYPE netwr_ap.
  DATA lv_valor_total TYPE netwr_ap.


  DATA ls_saldo TYPE zi_sd_zsdt0082_saldo.
  DATA ls_limite TYPE zsd_in_est_saldo_01.

*-US190444-10.09.2025-#190444-JT-inicio
*---------------------------------------------
* check se solicitacao desta OV esta bloqueada/pendente na distribuicao (zsdt0081)
*---------------------------------------------
  SELECT SINGLE nro_sol, bloqueio
    INTO @DATA(_zsdt0082)
    FROM zsdt0082
   WHERE vbeln    = @iv_vbeln
     AND posnr    = @iv_posnr
     AND seq      = '001'
     AND status  <> '3'
     AND bloqueio = @abap_true.

  IF sy-subrc = 0.
    ev_erro = abap_true.
    ev_msgx = 'Ordem de Venda está Pendente na Distribuicao da Solicitação: ' && _zsdt0082-nro_sol.
    RETURN.
  ENDIF.
*-US190444-10.09.2025-#190444-JT-fim

  "PERFORM f_read_saldo USING iv_vbeln iv_posnr CHANGING ls_saldo.

  SELECT * FROM zi_sd_zsdt0082_saldo
    INTO TABLE @DATA(lt_saldo)
      WHERE vbeln = @iv_vbeln.

  PERFORM f_read_limite USING iv_vbeln CHANGING ls_limite.

  LOOP AT lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

    IF <fs_saldo>-posnr = iv_posnr.

      lv_qtd = iv_qtde + <fs_saldo>-qtesol.

      IF lv_qtd > <fs_saldo>-qtdmax.

        WRITE lv_qtd TO lv_msg1 LEFT-JUSTIFIED.
        WRITE <fs_saldo>-qtdmax TO lv_msg2 LEFT-JUSTIFIED.

        ev_erro = abap_true.
        ev_msgx = `Qtde total: ` && lv_msg1 && ` é maior que o maximo do item ` && lv_msg2.

        EXIT.

      ENDIF.

    ELSE.
      lv_qtd = <fs_saldo>-qtesol.
    ENDIF.

    lv_vlr = lv_qtd * <fs_saldo>-vlrunit.

    ADD lv_vlr TO lv_valor_total.

  ENDLOOP.

  CHECK ev_erro IS INITIAL.

  lv_tole = lv_valor_total - ls_limite-vlrtotalautorizado_m.

  IF lv_tole BETWEEN '1.00-' AND '1.00'.
    lv_valor_total = lv_valor_total - lv_tole.
  ENDIF.

  IF iv_proc_novo = abap_true.

    IF lv_valor_total > ls_limite-vlrtotalautorizado_m.

      WRITE lv_valor_total TO lv_msg1 LEFT-JUSTIFIED.
      WRITE ls_limite-vlrtotalautorizado_m TO lv_msg2 LEFT-JUSTIFIED.

      ev_erro = abap_true.
      ev_msgx = `Valor a liberar: ` && lv_msg1 && ` é maior que o limite ` && lv_msg2 && ` aprovado`.

      EXIT.

    ENDIF.

  ENDIF.

ENDFUNCTION.
