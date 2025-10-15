FUNCTION zsdmf_tela_responder_checklist.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_CHECKLISTID) TYPE  ZSDCHECKLISTID DEFAULT '13'
*"     REFERENCE(IV_SIMULADOR) TYPE  ZSDED003 DEFAULT '1511'
*"     REFERENCE(IV_BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(IV_VKBUR) TYPE  VKBUR OPTIONAL
*"     REFERENCE(IV_EDIT) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(IV_MOSTRA_INCONF) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_CANC) TYPE  FLAG
*"----------------------------------------------------------------------

  PERFORM f_refresh_7001.

  zsds0006-checklistid = iv_checklistid.
  zsds0006-doc_simulacao = iv_simulador.
  zsds0006-edit = iv_edit.

  IF iv_bukrs IS INITIAL AND iv_vkbur IS INITIAL.

    SELECT SINGLE vkorg vkbur FROM zsdt0040
      INTO (zsds0006-bukrs,zsds0006-vkbur)
        WHERE doc_simulacao = iv_simulador.

  ELSE.

    zsds0006-bukrs = iv_bukrs.
    zsds0006-vkbur = iv_vkbur.

  ENDIF.

  SELECT * FROM zi_in_chklist_sim_item
    INTO CORRESPONDING FIELDS OF TABLE @gt_alv_7001_full
      WHERE doc_simulacao = @iv_simulador
        ORDER BY checkid ASCENDING.

  IF gt_alv_7001_full IS NOT INITIAL.

    SELECT * FROM zsdt0380_r
      INTO TABLE gt_zsdt0380_r
        FOR ALL ENTRIES IN gt_alv_7001_full
          WHERE checklistid = gt_alv_7001_full-checklistid
            AND checkid = gt_alv_7001_full-checkid.

  ENDIF.

  gt_alv_7001 = gt_alv_7001_full.

  DELETE gt_alv_7001 WHERE tpcheck = 'T'.

  LOOP AT gt_alv_7001 ASSIGNING FIELD-SYMBOL(<fs_alv2>).

    IF zsds0006-checklistid IS INITIAL.
      zsds0006-checklistid = <fs_alv2>-checklistid.
    ENDIF.

    IF zsds0006-edit = abap_true.

      PERFORM f_check_dependencia
        USING <fs_alv2>-checklistid
              <fs_alv2>-checkid
     CHANGING <fs_alv2>-hide.

    ELSE.

      PERFORM f_check_hide USING <fs_alv2>.

    ENDIF.

    PERFORM f_alv_2_icons CHANGING <fs_alv2>.

  ENDLOOP.

  PERFORM f_check_inconformidade.

  IF iv_mostra_inconf = abap_true.
    PERFORM f_check_inconformidade_item.
  ENDIF.



  "IF zsds0006-edit = abap_true.
  PERFORM f_filtro_7001 USING 'X'.
  "ENDIF.

  CALL SCREEN 7001.

  IF gv_ucomm_7001 <> 'SAVE'.
    ev_canc = abap_true.
  ENDIF.

  PERFORM f_refresh_7001.

ENDFUNCTION.
