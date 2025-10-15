*&---------------------------------------------------------------------*
*&  Include           ZFIR0065_FORM
*&---------------------------------------------------------------------*

FORM criar_field_catalog.

  FREE: wa_fcat, it_fcat.

  PERFORM estrutura_alv USING:

      0  'ZFIT0109'  'CODIGO'        'IT_SAIDA' 'CODIGO'           'Código'              '06'  'X'    '' ' ' ' ' ' ' ' ' ' ',
      1  'ZFIT0109'  ''              'IT_SAIDA' 'DESCRICAO'        'Descrição'           '15'  'X'    '' ' ' ' ' ' ' ' ' ' ',
      2  'ZFIT0109'  ''              'IT_SAIDA' 'OCULTAR'          'Ocultar'             '07'  'X'    '' ' ' ' ' ' ' ' ' 'X',
      3  'ZFIT0109'  'CLAS_FLX'      'IT_SAIDA' 'CLAS_FLX'         'Classificação'       '13'  'X'    '' ' ' ' ' ' ' 'X' ' ',
      4  'ZFIT0109'  'ST_CALC_SDO'   'IT_SAIDA' 'ST_CALC_SDO'      'Calcula Saldo'       '13'  'X'    '' ' ' ' ' ' ' ' ' 'X',
      5  'ZFIT0109'  'TP_PREV'       'IT_SAIDA' 'TP_PREV'          'Previsão'            '08'  'X'    '' ' ' ' ' ' ' 'X' ' ',
      6  'ZFIT0109'  ''              'IT_SAIDA' 'SEQ'              'Ordenar Rel'         '11'  'X'    '' ' ' ' ' ' ' ' ' ' ',
      7  ''          ''              'IT_SAIDA' 'TIPO_DOC_1'       'Tipo Documento'      '14'  ' '    '' ' ' ' ' ' ' 'X' ' ',
      8  ''          ''              'IT_SAIDA' 'TIPO_PED_1'       'Tipo Pedido'         '11'  ' '    '' ' ' ' ' ' ' 'X' ' ',
      9  ''          ''              'IT_SAIDA' 'TIPO_OV_1'        'Tipo Ordem Venda'    '16'  ' '    '' ' ' ' ' ' ' 'X' ' ',
     10  ''          ''              'IT_SAIDA' 'BLOQ_PGTO_1'      'Blq. Pgto.'          '10'  ' '    '' ' ' ' ' ''  'X' ' ',
     11  ''          ''              'IT_SAIDA' 'FORMA_PGTO'       'Forma Pgto.'         '11'  'X'    '' ' ' ' ' ''  'X' ' ',
     12  'ZFIT0109'  'BCO_EMPRESA'   'IT_SAIDA' 'BCO_EMPRESA'      'Bco. Empresa'        '12'  'X'    '' ' ' ' ' ' ' ' ' 'X',
     13  'ZFIT0109'  'OPR_SOBRA_CXA' 'IT_SAIDA' 'OPR_SOBRA_CXA'    'Opr.Sob.Cxa'         '11'  'X'    '' ' ' ' ' ' ' ' ' ' ',
     14  'ZFIT0109'  ''              'IT_SAIDA' 'PROCESSO_ESP'     'Processo. Espec.'    '16'  'X'    '' ' ' ' ' ' ' ' ' ' ',
     15  'ZFIT0109'  ''              'IT_SAIDA' 'SISTEMA_ORIG'     'Origem Sistema'      '15'  'X'    '' ' ' ' ' ' ' ' ' ' ',
*-CS2022000133-#74201-05.05.2022-JT-inicio
     16  'ZFIT0109'  ''              'IT_SAIDA' 'SOC_PARCEIRA'     'Soc.Parceira'        '12'  'X'    '' ' ' ' ' ' ' ' ' ' '.
*-CS2022000133-#74201-05.05.2022-JT-fim

ENDFORM.                    " CRIAR_FIELD_CATALOG_XML

FORM estrutura_alv USING VALUE(p_col_pos)       TYPE i
                         VALUE(p_ref_tabname)   LIKE dd02d-tabname
                         VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                         VALUE(p_tabname)       LIKE dd02d-tabname
                         VALUE(p_field)         LIKE dd03d-fieldname
                         VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                         VALUE(p_outputlen)
                         VALUE(p_edit)
                         VALUE(p_sum)
                         VALUE(p_emphasize)
                         VALUE(p_just)
                         VALUE(p_hotspot)
                         VALUE(p_f4)
                         VALUE(p_check).

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
  wa_fcat-checkbox    = p_check.

*-CS2022000133-#74201-05.05.2022-JT-inicio
  IF p_field = 'SOC_PARCEIRA'.
    wa_fcat-drdn_hndl  = 1.
    wa_fcat-drdn_alias = abap_true.
  ENDIF.
*-CS2022000133-#74201-05.05.2022-JT-fim

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

*-CS2022000133-#74201-05.05.2022-JT-inicio
FORM build_dropdown .

  DATA: ls_dropdown TYPE lvc_s_dral,
        lt_dropdown TYPE lvc_t_dral,
        tl_0038     TYPE TABLE OF zsdt0038 WITH HEADER LINE.

  ls_dropdown-handle    = '1'.

  ls_dropdown-int_value = 'S'.
  ls_dropdown-value     = 'S Sim'.
  APPEND: ls_dropdown  TO lt_dropdown.

  ls_dropdown-int_value = 'N'.
  ls_dropdown-value     = 'N Não'.
  APPEND: ls_dropdown  TO lt_dropdown.

* ls_dropdown-int_value = 'X'.
* ls_dropdown-value     = 'X Ambos'.
* APPEND: ls_dropdown  TO lt_dropdown.

  ls_dropdown-int_value = ' '.
  ls_dropdown-value     = '       '.
  APPEND: ls_dropdown  TO lt_dropdown.

  CALL METHOD obj_alv->set_drop_down_table
    EXPORTING
      it_drop_down_alias = lt_dropdown.

ENDFORM.                    " BUILD_DROPDOWN
*-CS2022000133-#74201-05.05.2022-JT-fim

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados .

  DATA: vl_tam     TYPE i.

  SELECT *
    INTO TABLE it_zfit0109
    FROM zfit0109.

  LOOP AT it_zfit0109 INTO wa_zfit0109.
    CLEAR: wa_saida.
    MOVE-CORRESPONDING wa_zfit0109 TO wa_saida.

    wa_saida-tipo_doc_1   = wa_saida-tipo_doc.
    wa_saida-tipo_ped_1   = wa_saida-tipo_ped.
    wa_saida-tipo_ov_1    = wa_saida-tipo_ov.
    wa_saida-bloq_pgto_1  = wa_saida-bloq_pgto.

    TRANSLATE wa_saida-tipo_doc_1   USING '\,'.
    TRANSLATE wa_saida-tipo_ped_1   USING '\,'.
    TRANSLATE wa_saida-tipo_ov_1    USING '\,'.
    TRANSLATE wa_saida-bloq_pgto_1  USING '\,'.

    "Remove Primeiro e Ultimo Caractere do Campo que esta no Formato ,xx,xx,xx, -> xx,xx,xx
    IF wa_saida-tipo_doc_1 IS NOT INITIAL.
      wa_saida-tipo_doc_1(1) = ''.
      vl_tam = strlen( wa_saida-tipo_doc_1 ).
      vl_tam = vl_tam - 1.

      wa_saida-tipo_doc_1+vl_tam(1) = ''.
      CONDENSE wa_saida-tipo_doc_1 NO-GAPS.
    ENDIF.

    IF wa_saida-tipo_ped_1 IS NOT INITIAL.
      wa_saida-tipo_ped_1(1) = ''.
      vl_tam = strlen( wa_saida-tipo_ped_1 ).
      vl_tam = vl_tam - 1.

      wa_saida-tipo_ped_1+vl_tam(1) = ''.
      CONDENSE wa_saida-tipo_ped_1 NO-GAPS.
    ENDIF.

    IF wa_saida-tipo_ov_1 IS NOT INITIAL.
      wa_saida-tipo_ov_1(1) = ''.
      vl_tam = strlen( wa_saida-tipo_ov_1 ).
      vl_tam = vl_tam - 1.

      wa_saida-tipo_ov_1+vl_tam(1) = ''.
      CONDENSE wa_saida-tipo_ov_1 NO-GAPS.
    ENDIF.

    IF wa_saida-bloq_pgto_1 IS NOT INITIAL.
      wa_saida-bloq_pgto_1(1) = ''.
      vl_tam = strlen( wa_saida-bloq_pgto_1 ).
      vl_tam = vl_tam - 1.

      wa_saida-bloq_pgto_1+vl_tam(1) = ''.
      CONDENSE wa_saida-bloq_pgto_1 NO-GAPS.
    ENDIF.

    APPEND wa_saida TO it_saida.
  ENDLOOP.

*-CS2021000936  - 11.10.2021 - JT - inicio
  SORT it_saida BY codigo.
*-CS2021000936  - 11.10.2021 - JT - fim

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  DEL_TP_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM del_value USING field.

  DATA: vl_count      TYPE i,
        vl_last_pos   TYPE i,
        vl_tp_doc_aux TYPE zfit0109-tipo_doc,
        vl_tam_str    TYPE i.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  IF lines( it_sel_rows ) NE 1.
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
    RETURN.
  ENDIF.

  FIELD-SYMBOLS <l_saida> TYPE ty_saida.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida ASSIGNING <l_saida> INDEX wa_sel_rows-index.

  CASE field.
    WHEN 'TIPO_DOC'.
      PERFORM del_last_value CHANGING <l_saida>-tipo_doc_1.
    WHEN 'TIPO_PED'.
      PERFORM del_last_value CHANGING <l_saida>-tipo_ped_1.
    WHEN 'TIPO_OV'.
      PERFORM del_last_value CHANGING <l_saida>-tipo_ov_1.
    WHEN 'BLOQ_PGTO'.
      PERFORM del_last_value CHANGING <l_saida>-bloq_pgto_1.
  ENDCASE.

  CALL METHOD obj_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.                    " DEL_TP_DOC

FORM del_last_value CHANGING p_value.

  DATA: vl_count     TYPE i,
        vl_last_pos  TYPE i,
        vl_value_aux TYPE string,
        vl_tam_str   TYPE i.

  IF strlen( p_value ) > 0.

    vl_value_aux = p_value.
    vl_tam_str    = strlen( p_value ).

    vl_count    = 0.
    vl_last_pos = 0.

    DO vl_tam_str TIMES.

      IF ( vl_value_aux+vl_count(1) EQ ',' ).
        vl_last_pos = ( vl_count ).
      ENDIF.

      ADD 1 TO vl_count.
    ENDDO.

    IF vl_last_pos > 0.
      vl_value_aux = p_value(vl_last_pos).
    ELSE.
      CLEAR: vl_value_aux.
    ENDIF.

    p_value = vl_value_aux.

  ENDIF.

ENDFORM.                    " DEL_TP_DOC
*&---------------------------------------------------------------------*
*&      Form  DELETAR_REG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_reg.

  DATA: var_answer  TYPE c.

  FIELD-SYMBOLS <l_saida> TYPE ty_saida.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  IF lines( it_sel_rows ) NE 1.
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
    RETURN.
  ENDIF.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Confirma exclusão do Parâmetro?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.


  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida INTO wa_saida INDEX wa_sel_rows-index.

  MOVE-CORRESPONDING wa_saida TO wa_zfit0109.
  DELETE FROM zfit0109 WHERE codigo   = wa_zfit0109-codigo
                         AND clas_flx = wa_zfit0109-clas_flx
                         AND seq      = wa_zfit0109-seq.

  DELETE it_saida WHERE codigo   = wa_zfit0109-codigo
                    AND clas_flx = wa_zfit0109-clas_flx
                    AND seq      = wa_zfit0109-seq.

  CALL METHOD obj_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.



ENDFORM.                    " DELETAR_REG
*&---------------------------------------------------------------------*
*&      Form  SALVAR_REG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_reg .

  DATA: tp_doc_aux        TYPE string,
        tp_ped_aux        TYPE string,
        tp_ov_aux         TYPE string,
        bloq_pgto_aux     TYPE string,
        vl_like_doc       TYPE string,
        vl_like_ped       TYPE string,
        vl_like_ov        TYPE string,
        vl_like_bloq_pgto TYPE string,
        vl_msg_exibir     TYPE string.

  FIELD-SYMBOLS <l_saida> TYPE ty_saida.

  CLEAR: wa_zfit0109.

  CALL METHOD obj_alv->check_changed_data.

  LOOP AT it_saida ASSIGNING <l_saida>.
    MOVE-CORRESPONDING <l_saida> TO wa_zfit0109.

    CLEAR: wa_zfit0109-tipo_doc, wa_zfit0109-tipo_ped, wa_zfit0109-tipo_ov, wa_zfit0109-bloq_pgto.
    REFRESH: tg_tp_doc, tg_tp_ped, tg_tp_ov.

    PERFORM carrega_values USING: 'TP_DOC' <l_saida>-tipo_doc_1,
                                  'TP_PED' <l_saida>-tipo_ped_1,
                                  'TP_OV'  <l_saida>-tipo_ov_1.

*    IF ( TG_TP_DOC[] IS INITIAL ) AND
*       ( TG_TP_PED[] IS INITIAL ) AND
*       ( TG_TP_OV[]  IS INITIAL ).
*
*      "Verifica Duplicidade Parametro Tipo Doc / Forma Pgto / Bloqueio Pgto.
*      CLEAR: WA_ZFIT0109_AUX.
*      SELECT SINGLE *
*        INTO WA_ZFIT0109_AUX
*        FROM ZFIT0109
*       WHERE TIPO_DOC   EQ ''
*         AND TIPO_PED   EQ ''
*         AND TIPO_OV    EQ ''
*         AND BLOQ_PGTO  EQ WA_ZFIT0109-BLOQ_PGTO
*         AND FORMA_PGTO EQ WA_ZFIT0109-FORMA_PGTO
*         AND CODIGO     NE WA_ZFIT0109-CODIGO
*         AND SEQ        NE WA_ZFIT0109-SEQ
*         AND CLAS_FLX   NE WA_ZFIT0109-CLAS_FLX.
*
*      IF SY-SUBRC = 0.
*        ROLLBACK WORK.
*        CONCATENATE 'Parâmetro já existente para Bloq Pgto.:'  WA_ZFIT0109-BLOQ_PGTO ','
*                                                'Forma Pgto.:' WA_ZFIT0109-FORMA_PGTO '!'
*               INTO VL_MSG_EXIBIR SEPARATED BY SPACE.
*
*        MESSAGE VL_MSG_EXIBIR TYPE 'S'.
*        RETURN.
*      ENDIF.
*
*    ENDIF.

*    LOOP AT TG_TP_DOC.
*
*      CONCATENATE '%\' TG_TP_DOC-BLART '\%' INTO VL_LIKE_DOC.
*
*      IF ( TG_TP_PED[] IS INITIAL ) AND
*         ( TG_TP_OV[]  IS INITIAL ).
*
*        "Verifica Duplicidade Parametro Tipo Doc / Forma Pgto / Bloqueio Pgto.
*        CLEAR: WA_ZFIT0109_AUX.
*        SELECT SINGLE *
*          INTO WA_ZFIT0109_AUX
*          FROM ZFIT0109
*         WHERE TIPO_DOC   LIKE VL_LIKE_DOC
*           AND TIPO_PED   EQ ''
*           AND TIPO_OV    EQ ''
*           "AND BLOQ_PGTO  EQ WA_ZFIT0109-BLOQ_PGTO
*           AND FORMA_PGTO EQ WA_ZFIT0109-FORMA_PGTO
*           AND CODIGO     NE WA_ZFIT0109-CODIGO
*           AND SEQ        NE WA_ZFIT0109-SEQ
*           AND CLAS_FLX   NE WA_ZFIT0109-CLAS_FLX.
*
*        IF SY-SUBRC = 0.
*          ROLLBACK WORK.
*          CONCATENATE 'Parâmetro já existente para Tipo Doc.:'   TG_TP_DOC-BLART ','
*                                                  'Bloq Pgto.:'  WA_ZFIT0109-BLOQ_PGTO ','
*                                                  'Forma Pgto.:' WA_ZFIT0109-FORMA_PGTO '!'
*                 INTO VL_MSG_EXIBIR SEPARATED BY SPACE.
*
*          MESSAGE VL_MSG_EXIBIR TYPE 'S'.
*          RETURN.
*        ENDIF.
*
*      ENDIF.
*
*      IF TG_TP_PED[] IS NOT INITIAL.  "Verifica Duplicidade Parametro Tipo Doc e Tipo Ped
*
*        LOOP AT TG_TP_PED.
*
*          CONCATENATE '%\' TG_TP_PED-BSART '\%' INTO VL_LIKE_PED.
*
*          CLEAR: WA_ZFIT0109_AUX.
*          SELECT SINGLE *
*            INTO WA_ZFIT0109_AUX
*            FROM ZFIT0109
*           WHERE TIPO_DOC LIKE VL_LIKE_DOC
*             AND TIPO_PED LIKE VL_LIKE_PED
*             "AND BLOQ_PGTO  EQ WA_ZFIT0109-BLOQ_PGTO
*             AND FORMA_PGTO EQ WA_ZFIT0109-FORMA_PGTO
*             AND CODIGO     NE WA_ZFIT0109-CODIGO
*             AND SEQ        NE WA_ZFIT0109-SEQ
*             AND CLAS_FLX   NE WA_ZFIT0109-CLAS_FLX.
*
*          IF SY-SUBRC = 0.
*            ROLLBACK WORK.
*            CONCATENATE 'Parâmetro já existente para Tipo Doc.:'   TG_TP_DOC-BLART ','
*                                                    'Tipo Ped.:'   TG_TP_PED-BSART ','
*                                                    'Bloq Pgto.:'  WA_ZFIT0109-BLOQ_PGTO ','
*                                                    'Forma Pgto.:' WA_ZFIT0109-FORMA_PGTO '!'
*                   INTO VL_MSG_EXIBIR SEPARATED BY SPACE.
*
*            MESSAGE VL_MSG_EXIBIR TYPE 'S'.
*            RETURN.
*          ENDIF.
*
*        ENDLOOP.
*
*      ELSE. "Verifica Duplicidade Parametro Tipo Doc e Tipo OV
*
*        LOOP AT TG_TP_OV.
*
*          CONCATENATE '%\' TG_TP_OV-AUART '\%' INTO VL_LIKE_OV.
*
*          CLEAR: WA_ZFIT0109_AUX.
*          SELECT SINGLE *
*            INTO WA_ZFIT0109_AUX
*            FROM ZFIT0109
*           WHERE TIPO_DOC   LIKE VL_LIKE_DOC
*             AND TIPO_OV    LIKE VL_LIKE_OV
*             "AND BLOQ_PGTO  EQ WA_ZFIT0109-BLOQ_PGTO
*             AND FORMA_PGTO EQ WA_ZFIT0109-FORMA_PGTO
*             AND CODIGO     NE WA_ZFIT0109-CODIGO
*             AND SEQ        NE WA_ZFIT0109-SEQ
*             AND CLAS_FLX   NE WA_ZFIT0109-CLAS_FLX.
*
*          IF SY-SUBRC = 0.
*            ROLLBACK WORK.
*            CONCATENATE 'Parâmetro já existente para Tipo Doc.:'   TG_TP_DOC-BLART ','
*                                                    'Tipo OV.:'    TG_TP_OV-AUART ','
*                                                    'Bloq Pgto.:'  WA_ZFIT0109-BLOQ_PGTO ','
*                                                    'Forma Pgto.:' WA_ZFIT0109-FORMA_PGTO '!'
*                   INTO VL_MSG_EXIBIR SEPARATED BY SPACE.
*
*            MESSAGE VL_MSG_EXIBIR TYPE 'S'.
*            RETURN.
*          ENDIF.
*
*        ENDLOOP.
*
*      ENDIF.
*
*    ENDLOOP.

    wa_zfit0109-usnam    = sy-uname.
    wa_zfit0109-dt_atual = sy-datum.
    wa_zfit0109-hr_atual = sy-uzeit.

    tp_doc_aux     = <l_saida>-tipo_doc_1.
    tp_ped_aux     = <l_saida>-tipo_ped_1.
    tp_ov_aux      = <l_saida>-tipo_ov_1.
    bloq_pgto_aux  = <l_saida>-bloq_pgto_1.

    IF tp_doc_aux IS NOT INITIAL.
      CONCATENATE '\' tp_doc_aux '\' INTO tp_doc_aux.
      TRANSLATE tp_doc_aux USING ',\'.
      wa_zfit0109-tipo_doc = tp_doc_aux.
    ENDIF.

    IF tp_ped_aux IS NOT INITIAL.
      CONCATENATE '\' tp_ped_aux '\' INTO tp_ped_aux.
      TRANSLATE tp_ped_aux USING ',\'.
      wa_zfit0109-tipo_ped = tp_ped_aux.
    ENDIF.

    IF tp_ov_aux IS NOT INITIAL.
      CONCATENATE '\' tp_ov_aux '\' INTO tp_ov_aux.
      TRANSLATE tp_ov_aux USING ',\'.
      wa_zfit0109-tipo_ov = tp_ov_aux.
    ENDIF.

    IF bloq_pgto_aux IS NOT INITIAL.
      CONCATENATE '\' bloq_pgto_aux '\' INTO bloq_pgto_aux.
      TRANSLATE bloq_pgto_aux USING ',\'.
      wa_zfit0109-bloq_pgto = bloq_pgto_aux.
    ENDIF.

    IF wa_zfit0109-codigo IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Código é um campo obrigatório!' TYPE 'S'.
      RETURN.
      EXIT.
    ENDIF.

    IF wa_zfit0109-descricao IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Descrição é um campo obrigatório!' TYPE 'S'.
      RETURN.
      EXIT.
    ENDIF.

    IF wa_zfit0109-clas_flx IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Classificação é um campo obrigatório!' TYPE 'S'.
      RETURN.
      EXIT.
    ENDIF.

    IF wa_zfit0109-seq IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Sequencia do Fluxo é um campo obrigatório!' TYPE 'S'.
      RETURN.
      EXIT.
    ENDIF.

    MODIFY zfit0109 FROM wa_zfit0109.
  ENDLOOP.

  COMMIT WORK.

  MESSAGE 'Registro(s) gravado(s) com sucesso!' TYPE 'S'.


ENDFORM.                    " SALVAR_REG


FORM carrega_values USING p_tipo vl_value .

  DATA: vl_value_append TYPE string,
        vl_count        TYPE i,
        vl_idx          TYPE sytabix,
        vl_tam_str      TYPE i.


  vl_tam_str = strlen( vl_value ).

  IF vl_tam_str = 0.
    EXIT.
  ENDIF.

  vl_count = 0.
  vl_idx   = 1.
  CLEAR: vl_value_append.

  DO vl_tam_str TIMES.

    IF ( vl_value+vl_count(1) NE ',' ).
      CONCATENATE vl_value_append vl_value+vl_count(1) INTO vl_value_append.
    ENDIF.

    IF ( vl_value+vl_count(1) EQ ',' ) OR ( vl_tam_str = vl_idx ).

      IF ( vl_value_append IS NOT INITIAL ) .

        CASE p_tipo.
          WHEN 'TP_DOC'.

            CLEAR: tg_tp_doc.
            tg_tp_doc-blart = vl_value_append.
            APPEND tg_tp_doc.

          WHEN 'TP_PED'.

            CLEAR: tg_tp_ped.
            tg_tp_ped-bsart = vl_value_append.
            APPEND tg_tp_ped.

          WHEN 'TP_OV'.

            CLEAR: tg_tp_ov.
            tg_tp_ov-auart = vl_value_append.
            APPEND tg_tp_doc.

        ENDCASE.

      ENDIF.

      CLEAR: vl_value_append.

    ENDIF.

    ADD: 1 TO vl_count,
         1 TO vl_idx.

  ENDDO.



ENDFORM.                    " CARREGA_VALUES
*&---------------------------------------------------------------------*
*&      Form  PREV_PAG_INTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prev_pag_inter .

  SUBMIT zregister_data WITH p_db_tab = 'ZFIT0175'
                        WITH p_stcnam = 'ZFIT0175_OUT'
                        WITH p_scmant = '0136'
                        WITH p_title  = 'Parametro Previsao Automatica Intercompany'
                  AND RETURN.

ENDFORM.
