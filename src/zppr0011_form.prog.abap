*&---------------------------------------------------------------------*
*&  Include           ZPPR0011_FORM


*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Form  ZF_INSERE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_insere.

  DATA: p_respo TYPE c.

  IF wa_saida-clint     IS NOT INITIAL
 AND wa_saida-class     IS NOT INITIAL
 AND wa_saida-atinn     IS NOT INITIAL
 AND wa_saida-atnam     IS NOT INITIAL
 AND wa_saida-klart     IS NOT INITIAL
 AND wa_saida-valor_de  IS NOT INITIAL
 AND wa_saida-valor_ate IS NOT INITIAL.

    SELECT *
    FROM klah
    INTO TABLE it_klah
    WHERE clint EQ wa_saida-clint.

    IF it_klah[] IS INITIAL.
      MESSAGE i003 ."WITH WA_SAIDA-CLASS.
    ELSE.
      SELECT *
      FROM v_mkm_zu_kls_bez
      INTO TABLE it_v_mkm_zu_kls_bez
       WHERE atinn EQ wa_saida-atinn.

      IF it_v_mkm_zu_kls_bez[] IS INITIAL.
        MESSAGE i004 WITH wa_saida-class.
      ELSE.

        SELECT *
        FROM zppt0014
        INTO TABLE it_zppt0014
        WHERE clint EQ wa_saida-clint
          AND class EQ wa_saida-class
          AND atinn EQ wa_saida-atinn.

        IF it_zppt0014 IS NOT INITIAL.
*          MESSAGE TEXT-015 TYPE 'I' DISPLAY LIKE 'E'.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING        "TITLEBAR = 'Confirmar'
              text_question         = 'Caracteristica ja cadastrada, deseja modificar?'
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              display_cancel_button = ' '
            IMPORTING
              answer                = p_respo.


          IF p_respo = 1.

*            READ TABLE IT_S_REPORT INTO WA_S_REPORT WITH KEY CLINT = WA_SAIDA-CLINT
*                                                          CLASS = WA_SAIDA-CLASS
*                                                          ATINN = WA_SAIDA-ATINN
*                                                          ATNAM = WA_SAIDA-ATNAM.

            DELETE FROM zppt0014
            WHERE clint     =  wa_s_report-clint
            AND   class     =  wa_s_report-class
            AND   atinn     =  wa_s_report-atinn
            AND   atnam     =  wa_s_report-atnam
            AND   klart     =  wa_s_report-klart
            AND   valor_de  =  wa_s_report-valor_de
            AND   valor_ate =  wa_s_report-valor_ate.

            FREE it_s_report.
            SELECT *
            FROM zppt0014
            APPENDING TABLE it_s_report.

            wa_s_report-clint     = wa_saida-clint.
            wa_s_report-class     = wa_saida-class.
            wa_s_report-atinn     = wa_saida-atinn.
            wa_s_report-atnam     = wa_saida-atnam.
            wa_s_report-klart     = wa_saida-klart.
            wa_s_report-valor_de  = wa_saida-valor_de.
            wa_s_report-valor_ate = wa_saida-valor_ate.

            MODIFY zppt0014 FROM wa_s_report. "#EC CI_FLDEXT_OK[2215424]
            CLEAR wa_saida.
            CLEAR wa_s_report.
          ENDIF.
          PERFORM seleciona_dados.

        ELSE.

          READ TABLE it_s_report INTO wa_s_report WITH KEY clint = wa_saida-clint
                                                           class = wa_saida-class
                                                           atinn = wa_saida-atinn
                                                           atnam = wa_saida-atnam.

          IF wa_s_report-atinn = wa_saida-atinn
       AND wa_s_report-atnam = wa_saida-atnam.
            MESSAGE text-019 TYPE 'I' DISPLAY LIKE 'E'.
          ELSE.
            wa_s_report-clint     = wa_saida-clint.
            wa_s_report-class     = wa_saida-class.
            wa_s_report-atinn     = wa_saida-atinn.
            wa_s_report-atnam     = wa_saida-atnam.
            wa_s_report-klart     = wa_saida-klart.
            wa_s_report-valor_de  = wa_saida-valor_de.
            wa_s_report-valor_ate = wa_saida-valor_ate.

            APPEND wa_s_report TO it_s_report.
            CLEAR wa_saida.
            CLEAR wa_s_report.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSE.
    MESSAGE text-004 TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM seleciona_dados.

  FREE it_s_report.
  SELECT *
  FROM zppt0014
  APPENDING CORRESPONDING FIELDS OF TABLE it_s_report.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_gravar.

  DATA: l_erro TYPE c.

*----------------------
* validar referencia
*----------------------
  l_erro = abap_false.

  LOOP AT it_s_report INTO wa_s_report.
*-CS2022000332-#83225-26.07.2022-JT-inicio
    CASE wa_s_report-atnam.
      WHEN 'FARDINHOS_UHML'.
      WHEN 'FARDINHOS_UI'.
      WHEN 'FARDINHOS_STR'.
      WHEN 'FARDINHOS_ELG'.
      WHEN 'FARDINHOS_MIC'.
      WHEN 'FARDINHOS_RD'.
      WHEN 'FARDINHOS_+B'.
      WHEN 'FARDINHOS_CG'.
      WHEN 'FARDINHOS_T.CNT'.
      WHEN 'FARDINHOS_T.AREA'.
      WHEN 'FARDINHOS_LEAF'.
      WHEN 'FARDINHOS_MR'.
      WHEN 'FARDINHOS_SFI(W)'.
      WHEN 'FARDINHOS_SCI'.
      WHEN 'FARDINHOS_CSP'.
      WHEN 'VARIEDADE'.
      WHEN OTHERS.
        IF wa_s_report-clint = 367.
          l_erro = abap_true.
          MESSAGE i024(sd) WITH 'Não encontrado De/Para do Campo referencia!'.
          EXIT.
        ENDIF.
    ENDCASE.
*-CS2022000332-#83225-26.07.2022-JT-fim
  ENDLOOP.

  IF l_erro = abap_false.
    LOOP AT it_s_report INTO wa_s_report.
*-CS2022000332-#83225-26.07.2022-JT-inicio
      CLEAR: wa_s_report-referencia.

      IF wa_s_report-clint = 367.
        CASE wa_s_report-atnam.
          WHEN 'FARDINHOS_UHML'.
            wa_s_report-referencia = 'FAR_UHML'.
          WHEN 'FARDINHOS_UI'.
            wa_s_report-referencia = 'FAR_UI'.
          WHEN 'FARDINHOS_STR'.
            wa_s_report-referencia = 'FAR_STR'.
          WHEN 'FARDINHOS_ELG'.
            wa_s_report-referencia = 'FAR_ELG'.
          WHEN 'FARDINHOS_MIC'.
            wa_s_report-referencia = 'FAR_MIC'.
          WHEN 'FARDINHOS_RD'.
            wa_s_report-referencia = 'FAR_RD'.
          WHEN 'FARDINHOS_+B'.
            wa_s_report-referencia = 'FAR_B'.
          WHEN 'FARDINHOS_CG'.
            wa_s_report-referencia = 'FAR_CG'.
          WHEN 'FARDINHOS_T.CNT'.
            wa_s_report-referencia = 'FAR_TCNT'.
          WHEN 'FARDINHOS_T.AREA'.
            wa_s_report-referencia = 'FAR_TAREA'.
          WHEN 'FARDINHOS_LEAF'.
            wa_s_report-referencia = 'FAR_LEAF'.
          WHEN 'FARDINHOS_MR'.
            wa_s_report-referencia = 'FAR_MR'.
          WHEN 'FARDINHOS_SFI(W)'.
            wa_s_report-referencia = 'FAR_SFIW'.
          WHEN 'FARDINHOS_SCI'.
            wa_s_report-referencia = 'FAR_SCI'.
          WHEN 'FARDINHOS_CSP'.
            wa_s_report-referencia = 'FAR_CSP'.
          WHEN 'VARIEDADE'.
            wa_s_report-referencia = 'VARIEDADE'.
        ENDCASE.
      ENDIF.
*-CS2022000332-#83225-26.07.2022-JT-fim

      MOVE-CORRESPONDING wa_s_report TO zppt0014. "#EC CI_FLDEXT_OK[2215424]
      MODIFY zppt0014.
      CLEAR wa_saida.
      CLEAR wa_s_report.
    ENDLOOP.
  ENDIF.

  IF sy-subrc = 0.
*      MESSAGE I005.
  ENDIF.

  PERFORM seleciona_dados.
  PERFORM call_scrren.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_delete .

  DATA: p_resp,
          lv_msg TYPE bapi_msg.

  CLEAR: it_select_rows[], wa_select_rows.

  CALL METHOD wa_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_select_rows.

  IF it_select_rows[] IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING        "TITLEBAR = 'Confirmar'
        text_question         = 'Deseja realmente excluir a linha?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ' '
      IMPORTING
        answer                = p_resp.

    IF p_resp =< 1.

      LOOP AT it_select_rows INTO wa_select_rows.

        READ TABLE it_s_report INTO wa_s_report INDEX wa_select_rows-index.

        DELETE FROM zppt0014
        WHERE clint = wa_s_report-clint
        AND class =  wa_s_report-class
        AND atinn = wa_s_report-atinn
        AND atnam = wa_s_report-atnam
        AND klart = wa_s_report-klart
        AND valor_de = wa_s_report-valor_de
        AND valor_ate = wa_s_report-valor_ate.
      ENDLOOP.

      IF sy-subrc = 0.
        MESSAGE text-014 TYPE 'I' DISPLAY LIKE 'S'.

        PERFORM seleciona_dados.
        PERFORM call_scrren.

      ENDIF.
    ENDIF.

  ELSE.

    MESSAGE text-007 TYPE 'I' DISPLAY LIKE 'E'.
    PERFORM seleciona_dados.
    PERFORM call_scrren.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VERIFIC_CLASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verific_class .

  IF wa_saida-clint IS INITIAL.
    MESSAGE text-010 TYPE 'I' DISPLAY LIKE 'E'.

  ELSE.
    FREE it_klah[].
    SELECT *
    FROM klah
    INTO TABLE it_klah
    WHERE clint EQ wa_saida-clint.

    IF it_klah[] IS INITIAL.
      MESSAGE text-008 TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.

      READ TABLE it_klah WITH KEY clint = wa_saida-clint.
      wa_saida-klart = it_klah-klart.
      wa_saida-class = it_klah-class.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VERIFIC_CARACTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verific_caracts .

  DATA: p TYPE c.

  IF wa_saida-atinn IS INITIAL.
    MESSAGE text-013 TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.

    SELECT *
      FROM v_mkm_zu_kls_bez
      INTO TABLE it_v_mkm_zu_kls_bez
      WHERE clint EQ wa_saida-clint
        AND atinn EQ wa_saida-atinn.



    IF it_v_mkm_zu_kls_bez[] IS NOT INITIAL.

*      "Verifica se existe caracteristica cadastrada.
*      SELECT *
*       FROM ZMMT0025
*       INTO TABLE IT_ZMMT0025
*        FOR ALL ENTRIES IN IT_V_MKM_ZU_KLS_BEZ
*        WHERE ATINN EQ IT_V_MKM_ZU_KLS_BEZ-ATINN.
*
      IF it_v_mkm_zu_kls_bez[] IS NOT INITIAL.
        READ TABLE it_v_mkm_zu_kls_bez INTO wa_v_mkm_zu_kls_bez WITH KEY atinn = wa_saida-atinn.
        wa_saida-atnam = wa_v_mkm_zu_kls_bez-atnam.

      ELSE.
        MESSAGE i001 WITH wa_saida-class.
        CLEAR wa_saida-atinn.
      ENDIF.
    ELSE.
      MESSAGE text-011 TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALL_SCRREN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_scrren .
  CALL SCREEN 0100.
ENDFORM.



FORM z_fieldcat .
  PERFORM z_feed_fieldcat USING:
       1  'CLINT    '   'IT_SAIDA'  '30'   ' '  ' '  ' '  'Cod Classe        '  ''  ' '  'ZPPT0014'  ' ',
       2  'CLASS    '   'IT_SAIDA'  '30'   ' '  ' '  ' '  'Nome Classe       '  ''  ' '  'ZPPT0014'  ' ',
       3  'ATINN    '   'IT_SAIDA'  '15'   ' '  ' '  ' '  'Cod caracts       '  ''  ' '  'ZPPT0014'  ' ',
       4  'ATNAM    '   'IT_SAIDA'  '30'   ' '  ' '  ' '  'Nome Caracts      '  ''  ' '  'ZPPT0014'  ' ',
       5  'REFERENCIA'  'IT_SAIDA'  '30'   ' '  ' '  ' '  'Referência Interna'  ''  ' '  'ZPPT0014'  ' ', "*-CS2022000332-#83225-26.07.2022-JT-inicio
       6  'KLART    '   'IT_SAIDA'  '10'   ' '  ' '  ' '  'Tipo Classe       '  ''  ' '  'ZPPT0014'  ' ',
       7  'VALOR_DE '   'IT_SAIDA'  '10'   ' '  ' '  ' '  'Vlr padrão min    '  ''  ' '  'ZPPT0014'  ' ',
       8  'VALOR_ATE'   'IT_SAIDA'  '10'   ' '  ' '  ' '  'Vlr padrão max    '  ''  ' '  'ZPPT0014'  ' '.


ENDFORM.                    " Z_FILDCAT

FORM z_feed_fieldcat  USING       VALUE(p_colnum)
                                  VALUE(p_fieldname)
                                  VALUE(p_tabname)
                                  VALUE(p_len)
                                  VALUE(p_edit)
                                  VALUE(p_icon)
                                  VALUE(p_do_sum)
                                  VALUE(p_header)
                                  VALUE(p_emphasize)
                                  VALUE(p_hotspot)
                                  VALUE(p_ref_table)
                                  VALUE(p_ref_field).


  wa_fcat-col_pos     = p_colnum.
  wa_fcat-fieldname   = p_fieldname.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-outputlen   = p_len.
  wa_fcat-edit        = p_edit.
  wa_fcat-icon        = p_icon.
  wa_fcat-do_sum      = p_do_sum.
  wa_fcat-coltext     = p_header.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-ref_table   = p_ref_table.
  wa_fcat-ref_table   = p_ref_field.

  wa_layout-excp_conds    = 'X'.
  wa_layout-zebra         = 'X'.
  wa_layout-sel_mode      = 'A'.
  wa_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  wa_layout-totals_bef    = ' '.

  APPEND wa_fcat TO it_fcat.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_MOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM seleciona_dados_mod USING p_e_row p_e_column_fieldname.

  DATA: p_resp,
         lv_msg TYPE bapi_msg.

  CLEAR wa_saida.
  CLEAR wa_s_report.

*  CLEAR CLICKS.

  ADD 1 TO clicks.

  DATA(wa_s_report) = it_s_report[ p_e_row ].
  wa_saida-clint      = wa_s_report-clint    .
  wa_saida-class      = wa_s_report-class    .
  wa_saida-atinn      = wa_s_report-atinn    .
  wa_saida-atnam      = wa_s_report-atnam    .
  wa_saida-klart      = wa_s_report-klart    .
  wa_saida-valor_de   = wa_s_report-valor_de .
  wa_saida-valor_ate  = wa_s_report-valor_ate.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENT'
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

  FREE it_s_report.
  PERFORM seleciona_dados.
  PERFORM call_scrren.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  BUSCA_ATINN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_atinn INPUT.

  SELECT *
  FROM v_mkm_zu_kls_bez
  INTO  CORRESPONDING FIELDS OF TABLE it_v_mkm_zu_kls_bez
  WHERE clint EQ wa_saida-clint.

*  CHECK IT_V_MKM_ZU_KLS_BEZ IS NOT INITIAL.
*  SELECT *
*  FROM ZMMT0025
*  INTO TABLE IT_ZMMT0025
*  FOR ALL ENTRIES IN IT_V_MKM_ZU_KLS_BEZ
*  WHERE ATINN EQ IT_V_MKM_ZU_KLS_BEZ-ATINN.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ATINN'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ATINN'
      value_org       = 'S'
    TABLES
      value_tab       = it_v_mkm_zu_kls_bez
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.
