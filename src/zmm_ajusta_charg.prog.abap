*&---------------------------------------------------------------------*
*& Report ZMM_AJUSTA_CHARG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_ajusta_charg.

TABLES: zppt0002.

DATA: t_0002   TYPE TABLE OF zppt0002,
      w_0002   TYPE zppt0002,
      t_0006   TYPE TABLE OF zmmt0006,
      w_0006   TYPE zmmt0006,
      t_mseg   TYPE TABLE OF mseg,
      w_mseg   TYPE mseg,
      t_ximfbf TYPE TABLE OF zpps_ximfbf_log,
      w_ximfbf TYPE zpps_ximfbf_log.

DATA: vmsg(50),
lines          TYPE sy-tabix.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS:  p_idcot FOR zppt0002-id_cotton.
  SELECT-OPTIONS:  p_data  FOR sy-datum.
SELECTION-SCREEN END   OF BLOCK b1.

START-OF-SELECTION.

  SELECT *
    FROM zppt0002
    INTO TABLE t_0002
   WHERE id_cotton IN p_idcot
     AND laeda     IN p_data
     AND charg      = abap_off.

  CHECK t_0002[] IS NOT INITIAL.

  SELECT *
    FROM zpps_ximfbf_log
    INTO TABLE t_ximfbf
     FOR ALL ENTRIES IN t_0002
   WHERE id_cotton  = t_0002-id_cotton
     AND processado = 'S'.

  SELECT *
    FROM zmmt0006 AS m1
    INTO TABLE t_0006
     FOR ALL ENTRIES IN t_0002
   WHERE id_cotton = t_0002-id_cotton
     AND ch_referencia = ( SELECT MAX( m2~ch_referencia ) FROM zmmt0006 AS m2 WHERE m2~id_cotton = m1~id_cotton ).

  "Ajuste 23/10/2023 / aoenning.
  SELECT * FROM mseg
    INTO TABLE t_mseg
    FOR ALL ENTRIES IN t_0002
    WHERE xblnr_mkpf EQ t_0002-id_cotton
      AND smbln EQ space
      AND xauto EQ abap_true.
  "Ajuste 23/10/2023 / aoenning.

  DESCRIBE TABLE t_0002 LINES lines.

  LOOP AT t_0002 INTO w_0002.
    CLEAR: w_mseg, w_ximfbf, w_0006, vmsg.

    vmsg = |Executando: { sy-tabix } de um total: { lines }|.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = vmsg.

    READ TABLE t_ximfbf INTO w_ximfbf WITH KEY id_cotton = w_0002-id_cotton.
    IF sy-subrc = 0 AND w_ximfbf-charg IS NOT INITIAL.
      w_0002-charg   = w_ximfbf-charg.
    ELSE.
      READ TABLE t_0006 INTO w_0006 WITH KEY id_cotton = w_0002-id_cotton.
      IF sy-subrc = 0 AND w_0006-batch_d IS NOT INITIAL.
        w_0002-charg = w_0006-batch_d.
      ELSE.
        READ TABLE t_mseg INTO w_mseg WITH KEY xblnr_mkpf = w_0002-id_cotton. "Ajuste 23/10/2023 / aoenning.
        IF sy-subrc EQ 0 AND w_mseg-charg IS NOT INITIAL.
          w_0002-charg = w_mseg-charg.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY zppt0002 FROM w_0002.
  ENDLOOP.

  COMMIT WORK.
