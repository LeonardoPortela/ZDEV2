FUNCTION zsdmf_estorna_ov_agrupamento .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_DOC_SIMULACAO) TYPE  ZSDED003
*"     REFERENCE(IV_VBELN) TYPE  VBELN
*"     REFERENCE(IV_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"  TABLES
*"      ET_RET STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  DATA lt_travas TYPE TABLE OF zsdt0090.
  DATA lv_tp_fat TYPE zsde_status_faturamento.

  CHECK iv_vbeln IS NOT INITIAL.

  PERFORM f_check_status_fat
    IN PROGRAM saplzgfsd003
    USING iv_vbeln
 CHANGING lv_tp_fat.

  IF lv_tp_fat NE ''.

    PERFORM f_mensagem_insere_txt
      USING 'E' 'Impossivel estornar com faturamento lançado'
   CHANGING et_ret[].

    EXIT.

  ENDIF.

  PERFORM f_delete_ov USING iv_vbeln iv_commit CHANGING ev_erro et_ret[] .

  CHECK ev_erro IS INITIAL.

  SELECT SINGLE * FROM zsdt0090
    INTO @DATA(ls_0090)
    WHERE doc_simulacao = @iv_doc_simulacao
      AND vbeln = @iv_vbeln
      AND categoria = 'H'.

  IF ls_0090-estorno = abap_true.

    PERFORM f_mensagem_insere_txt
      USING 'E' 'Já estornado anteriormente'
   CHANGING et_ret[].

    EXIT.

  ENDIF.

  ls_0090-estorno = 'X'.
  ls_0090-usnam_e = sy-uname.
  ls_0090-data_atual_e = sy-datum.
  ls_0090-hora_atual_e = sy-uzeit.

  CALL FUNCTION 'ZSDMF_ESTORNAR_TRV_CMB'
    EXPORTING
      iv_doc_simulacao = iv_doc_simulacao
      iv_vbeln         = iv_vbeln
    IMPORTING
      ev_erro          = ev_erro
    TABLES
      et_travas        = lt_travas.

  IF ev_erro = abap_true.
    EXIT.
  ENDIF.

  APPEND ls_0090 TO lt_travas.

  PERFORM f_upd_seq_0090 CHANGING lt_travas.

  IF iv_commit = abap_true.
    MODIFY zsdt0090 FROM TABLE lt_travas.
  ENDIF.

  SELECT * FROM zsdt0041
     INTO TABLE @DATA(lt_0041)
        WHERE vbelv_agp = @iv_vbeln.

  IF sy-subrc EQ 0.

    LOOP AT lt_0041 ASSIGNING FIELD-SYMBOL(<fs_0041>).
      <fs_0041>-vbelv_agp = space.
    ENDLOOP.

    IF iv_commit = abap_true.
      MODIFY zsdt0041 FROM TABLE lt_0041.
    ENDIF.

  ENDIF.

  PERFORM f_mensagem_insere_txt
    USING 'S' 'Estornado com sucesso'
 CHANGING et_ret[].

ENDFUNCTION.
