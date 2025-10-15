FUNCTION zsdmf_tela_checklist_perguntas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_CHECKLISTID) TYPE  ZSDCHECKLISTID
*"     REFERENCE(IV_BUKRS) TYPE  BUKRS
*"     REFERENCE(IV_DESCR) TYPE  TEXT200
*"     REFERENCE(IV_EDITAVEL) TYPE  FLAG
*"  EXPORTING
*"     REFERENCE(EV_CANC) TYPE  CHAR1
*"     REFERENCE(ET_CHECKLIST_ITEMS) TYPE  ZSDC380
*"----------------------------------------------------------------------

  PERFORM f_refresh_8001.

  zsds379-checklistid = iv_checklistid.
  zsds379-bukrs = iv_bukrs.
  zsds379-descr = iv_descr.
  zsds379-desativar = iv_editavel.

  SELECT * FROM zsdt0380
    INTO TABLE gt_zsdt0380
      WHERE checklistid = iv_checklistid.

  SELECT * FROM zsdt0380_r
      INTO TABLE gt_zsdt0380_r
        WHERE checklistid = iv_checklistid.

  LOOP AT gt_zsdt0380 ASSIGNING FIELD-SYMBOL(<fs_zsdt0380>).

    APPEND INITIAL LINE TO gt_alv_8001 ASSIGNING FIELD-SYMBOL(<fs_alv>).

    MOVE-CORRESPONDING <fs_zsdt0380> TO <fs_alv>.

    PERFORM f_alv_status_change USING <fs_alv>.

  ENDLOOP.

  SORT gt_alv_8001 BY CHECKID ASCENDING.

  CALL SCREEN 8001.

  IF gv_ucomm_8001 <> 'SAVE'.
    ev_canc = abap_true.
  ENDIF.

  PERFORM f_refresh_8001.

ENDFUNCTION.
