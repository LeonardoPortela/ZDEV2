FUNCTION zsd_log_envio_trace_cotton.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ZSEQ_INST) TYPE  ZSEQ_INST OPTIONAL
*"     REFERENCE(I_OBJEK) TYPE  OBJNUM OPTIONAL
*"     REFERENCE(I_OBJECTTABLE) TYPE  TABELLE OPTIONAL
*"     REFERENCE(I_ID_CONTRATO) TYPE  VBELN_VA OPTIONAL
*"     REFERENCE(I_NRO_SOL_OV) TYPE  ZSDED013 OPTIONAL
*"     REFERENCE(I_POSNR) TYPE  POSNR_VA OPTIONAL
*"     REFERENCE(I_ID_CARGA) TYPE  ZID_CARGA OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_LGORT) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(I_ACHARG) TYPE  CHARG_D OPTIONAL
*"     REFERENCE(I_SAFRA) TYPE  CHAR4 OPTIONAL
*"     REFERENCE(I_TIPO_INTEGRA) TYPE  ZTIPO_INTEGRA
*"----------------------------------------------------------------------

  FREE: t_zsdt0327, t_det, it_fieldcat.

  CASE i_tipo_integra.
    WHEN 'CO'.
      SELECT *
        FROM zsdt0327
        INTO TABLE t_zsdt0327
       WHERE id_contrato = i_id_contrato.

    WHEN 'IN'.
      SELECT *
        FROM zsdt0327
        INTO TABLE t_zsdt0327
       WHERE zseq_inst   = i_zseq_inst
         AND objek       = i_objek
         AND objecttable = i_objecttable.

    WHEN 'OV'.
      SELECT *
        FROM zsdt0327
        INTO TABLE t_zsdt0327
       WHERE nro_sol_ov   = i_nro_sol_ov
         AND posnr        = i_posnr
         AND id_contrato  = i_id_contrato
         AND tipo_integra = 'OV'.

      IF sy-subrc <> 0.
        SELECT *
          FROM zsdt0327
          INTO TABLE t_zsdt0327
         WHERE nro_sol_ov   = i_nro_sol_ov
           AND posnr        = i_posnr
           AND tipo_integra = 'OV'.
      ENDIF.

    WHEN 'FB'.
      SELECT *
        FROM zsdt0327
        INTO TABLE t_zsdt0327
       WHERE id_carga     = i_id_carga
         AND matnr        = i_matnr
         AND werks        = i_werks
         AND lgort        = i_lgort
         AND acharg       = i_acharg
         AND safra        = i_safra
         AND tipo_integra = 'FB'.

      SELECT *
        FROM zsdt0331
        INTO TABLE t_zsdt0331
       WHERE id_carga    = i_id_carga
         AND cancelado   = abap_off.

    WHEN 'ES'.
      SELECT *
        FROM zsdt0327
        INTO TABLE t_zsdt0327
       WHERE id_carga     = i_id_carga
         AND matnr        = i_matnr
         AND werks        = i_werks
         AND lgort        = i_lgort
         AND acharg       = i_acharg
         AND safra        = i_safra
         AND tipo_integra = 'ES'.

      SELECT *
        FROM zsdt0331
        INTO TABLE t_zsdt0331
       WHERE id_carga    = i_id_carga
         AND cancelado   = abap_off.

    WHEN 'GL'.
      SELECT *
        FROM zsdt0327
        INTO TABLE t_zsdt0327
       WHERE id_carga     = i_id_carga
         AND matnr        = i_matnr
         AND werks        = i_werks
         AND lgort        = i_lgort
         AND acharg       = i_acharg
         AND safra        = i_safra
         AND tipo_integra = 'GL'.

    WHEN 'NF'.
      SELECT *
        FROM zsdt0327
        INTO TABLE t_zsdt0327
       WHERE id_carga     = i_id_carga
         AND matnr        = i_matnr
         AND werks        = i_werks
         AND lgort        = i_lgort
         AND acharg       = i_acharg
         AND safra        = i_safra
         AND tipo_integra = 'NF'.

  ENDCASE.

  LOOP AT t_zsdt0327             INTO w_zsdt0327.

    CLEAR w_det.
    MOVE-CORRESPONDING w_zsdt0327  TO w_det.

    w_det-status = COND #( WHEN w_zsdt0327-tipo_msg = 'E' THEN icon_led_red
                                                          ELSE icon_led_green ).
    SPLIT w_det-mensagem AT '|' INTO TABLE t_text.

    LOOP AT t_text INTO w_text.
      CHECK w_text IS NOT INITIAL.
      w_det-mensagem  = w_text.
      APPEND w_det   TO t_det.
    ENDLOOP.
  ENDLOOP.

  IF t_zsdt0331[] IS NOT INITIAL.
    LOOP AT t_zsdt0331 INTO w_zsdt0331.
      CLEAR w_det.
      w_det-status        = icon_led_yellow.
      w_det-id_carga      = w_zsdt0331-id_carga.
      w_det-matnr         = w_zsdt0331-matnr.
      w_det-werks         = w_zsdt0331-werks.
      w_det-lgort         = w_zsdt0331-lgort.
      w_det-cd_sai        = w_zsdt0331-cd_sai.
      w_det-safra         = w_zsdt0331-safra.
      w_det-usname        = w_zsdt0331-usname.
      w_det-data          = w_zsdt0331-data.
      w_det-hora          = w_zsdt0331-hora.
      w_det-mensagem      = 'Retorno Trace Cotton: ' && w_zsdt0331-mensagem.
      APPEND w_det       TO t_det.

      IF w_zsdt0331-mensagem_trace IS NOT INITIAL.
        w_det-mensagem    = 'Retorno Trace Cotton: ' && w_zsdt0331-mensagem_trace.
        APPEND w_det     TO t_det.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CHECK t_det[] IS NOT INITIAL.

  PERFORM f_preenche_fcat USING :
   '01' ''          ''            'T_DET'  'STATUS'                 'Status'                 '05'     ''    ''     ''    '' '' 'X'.

  CASE i_tipo_integra.
    WHEN 'CO'.
      PERFORM f_preenche_fcat USING :
       '02' 'VBAK'      'VBELN'   'T_DET'  'ID_CONTRATO'            'ID Contrato'            '10'     ''    ''     ''    '' '' ''.

    WHEN 'IN'.
      PERFORM f_preenche_fcat USING :
       '02' ''          ''        'T_DET'  'ZSEQ_INST'              'Sequencia'              '10'     ''    ''     ''    '' '' ''.

    WHEN 'OV'.
      PERFORM f_preenche_fcat USING :
       '02' 'VBAK'      'VBELN'   'T_DET'  'NRO_SOL_OV'             'Solicitação'            '10'     ''    ''     ''    '' '' '',
       '03' 'VBAP'      'POSNR'   'T_DET'  'POSNR'                  'Item'                   '10'     ''    ''     ''    '' '' ''.

    WHEN 'FB' OR 'GL'.
      PERFORM f_preenche_fcat USING :
       '02' ''          ''        'T_DET'  'ID_CARGA'               'ID Carregamento'        '35'     ''    ''     ''    '' '' '',
       '03' 'MARA'      'MATNR'   'T_DET'  'MATNR'                  'Material'               '20'     ''    ''     ''    '' '' '',
       '04' ''          ''        'T_DET'  'WERKS'                  'Centro'                 '08'     ''    ''     ''    '' '' '',
       '05' ''          ''        'T_DET'  'LGORT'                  'Depósito'               '08'     ''    ''     ''    '' '' '',
       '06' ''          ''        'T_DET'  'ACHARG'                 'Fardinho'               '20'     ''    ''     ''    '' '' '',
       '07' ''          ''        'T_DET'  'CD_SAI'                 'Cd Sai'                 '20'     ''    ''     ''    '' '' '',
       '08' ''          ''        'T_DET'  'SAFRA'                  'Safra'                  '05'     ''    ''     ''    '' '' ''.

    WHEN 'NF'.
      PERFORM f_preenche_fcat USING :
       '02' ''          ''        'T_DET'  'ID_CARGA'               'ID Carregamento'        '35'     ''    ''     ''    '' '' '',
       '03' 'MARA'      'MATNR'   'T_DET'  'MATNR'                  'Material'               '20'     ''    ''     ''    '' '' '',
       '04' ''          ''        'T_DET'  'WERKS'                  'Centro'                 '08'     ''    ''     ''    '' '' '',
       '08' ''          ''        'T_DET'  'SAFRA'                  'Safra'                  '05'     ''    ''     ''    '' '' ''.

  ENDCASE.

  PERFORM f_preenche_fcat USING :
   '10' ''          ''            'T_DET'  'MENSAGEM'               'Mensagem'               '60'     ''    ''     ''    '' '' '',
   '11' ''          ''            'T_DET'  'USNAME'                 'Usuário'                '15'     ''    ''     ''    '' '' '',
   '12' ''          ''            'T_DET'  'DATA'                   'Data'                   '12'     ''    ''     ''    '' '' '',
   '13' ''          ''            'T_DET'  'HORA'                   'Hora'                   '12'     ''    ''     ''    '' '' ''.

  ls_variant-report = sy-repid && 'XXX'.
  l_grid_title      = 'Log de Integração Trace Cotton'.

  SORT t_det  BY data DESCENDING
                 hora DESCENDING.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = sy-repid
      it_fieldcat           = it_fieldcat[]
*     it_sort               = t_sort[]
*     i_callback_user_command = 'USER_COMMAND_COMPRO'
      i_grid_title          = l_grid_title
      i_save                = 'X'
      is_variant            = ls_variant
      i_screen_start_column = 35
      i_screen_start_line   = 08
      i_screen_end_column   = 140
      i_screen_end_line     = 18
    TABLES
      t_outtab              = t_det.

ENDFUNCTION.
