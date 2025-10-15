*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZSDR0194_F01                                                                         *
*& Chamado        : USER STORY 155661                                                                    *
*& Data           : 25/10/2024                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: NIlton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       |Request    | Autor         | Alteração                                                   *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 25/10/2024  |DEVK9A1XAW | NSEGATIN      | Cadastro da Finalidade RFL - Devolução NF Entrada p/ Fila   *
*--------------------------------------------------------------------------------------------------------*
FORM criar_field_catalog.

  FREE: wa_fcat, it_fcat.

  PERFORM estrutura_alv USING:

      0  'ZSDT0364'  'FINALIDADE'       'IT_SAIDA' 'FINALIDADE'            'Final. Rec.'      '10'  'X'    '' ' ' ' ' ' ' 'X' ' ' ' ',
      1  ''          'DESC_FINAL'       'IT_SAIDA' 'DESC_FINAL'            'Descrição'        '40'  ''     '' ' ' ' ' ' ' '' ' ' ' ',
      2  'ZSDT0364'  'DATA_CRIACAO'     'IT_SAIDA' 'DATA_CRIACAO'          'Data Criação'     '10'  ''     '' ' ' ' ' ' ' 'X' ' ' ' ',
      3  'ZSDT0364'  'HORA_CRIACAO'     'IT_SAIDA' 'HORA_CRIACAO'          'Hora Criação'     '10'  ''     '' ' ' ' ' ' ' 'X' ' ' ' ',
      4  'ZSDT0364'  'USUARIO_CRIACAO'  'IT_SAIDA' 'USUARIO_CRIACAO'       '  '               '30'  ''     '' ' ' ' ' ' ' 'X' ' ' ' ',
      5  'ZSDT0364'  'EXCLUIDO'         'IT_SAIDA' 'EXCLUIDO'              '  '               '2'   'X'    '' ' ' ' ' ' ' '' ' ' 'X',
      6  'ZSDT0364'  'DATA_EXCLUSAO'    'IT_SAIDA' 'DATA_EXCLUSAO'         '  '               '2'   'X'    '' ' ' ' ' ' ' '' ' ' 'X',
      7  'ZSDT0364'  'HORA_EXCLUSAO'    'IT_SAIDA' 'HORA_EXCLUSAO'         '  '               '2'   'X'    '' ' ' ' ' ' ' '' ' ' 'X',
      8  'ZSDT0364'  'USUARIO_EXCLUSAO' 'IT_SAIDA' 'USUARIO_EXCLUSAO'      '  '               '2'   'X'    '' ' ' ' ' ' ' '' ' ' 'X'.


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
                         VALUE(p_check)
                         VALUE(p_no_out).

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = p_no_out.
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

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

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

  ls_dropdown-int_value = ' '.
  ls_dropdown-value     = '       '.
  APPEND: ls_dropdown  TO lt_dropdown.

  CALL METHOD obj_alv->set_drop_down_table
    EXPORTING
      it_drop_down_alias = lt_dropdown.

ENDFORM.                    " BUILD_DROPDOWN

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados .

  DATA: vl_tam     TYPE i,
        lt_celltab TYPE lvc_t_styl,
        ls_style   TYPE lvc_s_styl.

  SELECT *
    INTO TABLE it_ZSDT0364
    FROM zsdt0364
    WHERE excluido EQ space.
  IF sy-subrc IS INITIAL.
    DATA(lt_ZSDT0364) = it_ZSDT0364.
    SORT lt_ZSDT0364 BY finalidade.
    DELETE ADJACENT DUPLICATES FROM lt_ZSDT0364 COMPARING finalidade.

    SELECT ddtext, domvalue_l
      FROM dd07t
      INTO TABLE @DATA(tl_dd07t)
    WHERE domname    EQ 'ZFIN_EXPORT_D'
      AND ddlanguage EQ @sy-langu.

    IF sy-subrc IS INITIAL.
      SORT tl_dd07t BY domvalue_l.
    ENDIF.
  ENDIF.

  LOOP AT it_ZSDT0364 INTO wa_zsdt0364.
    CLEAR: wa_saida.
    MOVE-CORRESPONDING wa_ZSDT0364 TO wa_saida.

    READ TABLE tl_dd07t ASSIGNING FIELD-SYMBOL(<fs_dd07t>)
    WITH KEY domvalue_l = wa_saida-finalidade
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_saida-desc_final = <fs_dd07t>-ddtext.
    ENDIF.

    ls_style-fieldname = 'FINALIDADE'.
    ls_style-style     = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_style INTO TABLE wa_saida-celltab.

    APPEND wa_saida TO it_saida.
  ENDLOOP.

  SORT it_saida BY finalidade.

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
    WHEN 'TIPO_OV'.
*      PERFORM del_last_value CHANGING <l_saida>-tipo_ov.
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

  MOVE-CORRESPONDING wa_saida TO wa_ZSDT0364.
  UPDATE zsdt0364 SET excluido          = abap_true
                      data_exclusao     = sy-datum
                      hora_exclusao     = sy-uzeit
                      usuario_exclusao  = sy-uname
                  WHERE finalidade      = wa_ZSDT0364-finalidade
                    AND data_criacao    = wa_ZSDT0364-data_criacao
                    AND hora_criacao    = wa_ZSDT0364-hora_criacao
                    AND usuario_criacao = wa_ZSDT0364-usuario_criacao.

  DELETE it_saida WHERE finalidade           = wa_ZSDT0364-finalidade
                         AND data_criacao    = wa_ZSDT0364-data_criacao
                         AND hora_criacao    = wa_ZSDT0364-hora_criacao
                         AND usuario_criacao = wa_ZSDT0364-usuario_criacao
                         AND excluido        = wa_ZSDT0364-excluido.

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
        vl_msg_exibir     TYPE string,
        ls_style          TYPE lvc_s_styl.

  FIELD-SYMBOLS <l_saida> TYPE ty_saida.

  CLEAR: wa_ZSDT0364,
         gv_erro.

  CALL METHOD obj_alv->check_changed_data( ).

  IF gv_modif IS NOT INITIAL AND gv_erro IS INITIAL.

    DELETE it_saida WHERE desc_final IS INITIAL.

    IF it_saida IS NOT INITIAL.

      LOOP AT it_saida ASSIGNING <l_saida>.
        IF <l_saida>-data_criacao IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        <l_saida>-data_criacao = sy-datum.
        <l_saida>-hora_criacao = sy-uzeit.
        <l_saida>-usuario_criacao = sy-uname.

        ls_style-fieldname = 'FINALIDADE'.
        ls_style-style     = cl_gui_alv_grid=>mc_style_disabled.
        INSERT ls_style INTO TABLE <l_saida>-celltab.

        MOVE-CORRESPONDING <l_saida> TO wa_ZSDT0364.

        IF wa_ZSDT0364-finalidade IS INITIAL.
          ROLLBACK WORK.
          MESSAGE 'Tipo de Fatura é um campo obrigatório!' TYPE 'S'.
          RETURN.
          EXIT.
        ENDIF.

        MODIFY zsdt0364 FROM wa_ZSDT0364.
      ENDLOOP.

      CALL METHOD obj_alv->refresh_table_display( ).

      COMMIT WORK.

      MESSAGE 'Registro(s) gravado(s) com sucesso!' TYPE 'S'.

    ENDIF.

  ENDIF.

  CLEAR gv_modif.
  FREE: it_saida_final.

ENDFORM.                    " SALVAR_REG
