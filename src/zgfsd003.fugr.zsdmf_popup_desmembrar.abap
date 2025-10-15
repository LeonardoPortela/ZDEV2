FUNCTION zsdmf_popup_desmembrar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_EDIT) TYPE  FLAG DEFAULT SPACE
*"     REFERENCE(IV_OPT_FAT) TYPE  FLAG DEFAULT SPACE
*"  EXPORTING
*"     REFERENCE(EV_CANC) TYPE  FLAG
*"  TABLES
*"      CT_DESMEMBRA STRUCTURE  ZSDS081
*"  CHANGING
*"     REFERENCE(CS_POPUP_VALUES) TYPE  ZSDS078
*"----------------------------------------------------------------------

  DATA lv_numc TYPE c LENGTH 4.

  PERFORM f_refresh_9000.

  gv_edit_9000 = iv_edit.
  gv_fatu_9000 = iv_opt_fat.

  zsds078 = cs_popup_values.

  LOOP AT ct_desmembra ASSIGNING FIELD-SYMBOL(<fs_travas>).

    APPEND INITIAL LINE TO gt_alv_7000 ASSIGNING FIELD-SYMBOL(<fs_alv>).

    <fs_alv>-vbeln = <fs_travas>-vbeln.
    <fs_alv>-vbeln2 = <fs_travas>-vbeln2.
    <fs_alv>-posnr = <fs_travas>-posnr.
    <fs_alv>-matnr = <fs_travas>-matnr.
    <fs_alv>-matnr2 = <fs_travas>-matnr2.
    <fs_alv>-arktx = <fs_travas>-arktx.
    <fs_alv>-kwmeng = <fs_travas>-kwmeng.
    <fs_alv>-vrkme = <fs_travas>-vrkme.
    <fs_alv>-sd_disp = <fs_travas>-sd_disp.
    <fs_alv>-qt_tran = <fs_travas>-qt_tran.
    <fs_alv>-rfmng = <fs_travas>-rfmng.
    <fs_alv>-spart = <fs_travas>-spart.

  ENDLOOP.

  SORT gt_alv_7000.

  zsds078-exec_01 = 'X'. "<- primeira execução

  CALL SCREEN 7000 STARTING AT 20 1.

  IF gv_ucomm_7000 = 'CANC'.

    ev_canc = 'X'.

    CLEAR cs_popup_values.

  ELSE.

    cs_popup_values = zsds078.

  ENDIF.

ENDFUNCTION.
