*&---------------------------------------------------------------------*
*&  Include           ZFIR0065_FORM
*&---------------------------------------------------------------------*

FORM criar_field_catalog.

  FREE: wa_fcat, it_fcat.

  PERFORM estrutura_alv USING:

      0  'ZSDT0361'  'ATIVA_FORM_LOTE'     'IT_SAIDA' 'ATIVA_FORM_LOTE'          'Ativa/Desativa'               '20'  'X'     '' ' ' ' ' ' ' '' 'X' ' ',
      1  'ZSDT0361'  'DATA_ULTIMA_MODIF'   'IT_SAIDA' 'DATA_ULTIMA_MODIF'        'Data Última Modificação'      '20'  ''     '' ' ' ' ' ' ' 'X' ' ' ' ',
      2  'ZSDT0361'  'HORA_ULTIMA_MODIF'   'IT_SAIDA' 'HORA_ULTIMA_MODIF'        'Hora Última Modificação'      '20'  ''     '' ' ' ' ' ' ' 'X' ' ' ' ',
      3  'ZSDT0361'  'ULTIMO_MODIFICADOR'  'IT_SAIDA' 'ULTIMO_MODIFICADOR'       'Último Modificador'           '20'  ''     '' ' ' ' ' ' ' 'X' ' ' ' '.


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
        ls_style   TYPE lvc_s_styl,
        lt_values  TYPE TABLE OF dd07v.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_saida
    FROM zsdt0361.
  IF sy-subrc IS NOT INITIAL.
    APPEND INITIAL LINE TO it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS

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

  CLEAR: wa_ZSDT0361,
         gv_erro.

  CALL METHOD obj_alv->check_changed_data( ).

  DELETE FROM zsdt0361.
  IF sy-subrc IS INITIAL.
    COMMIT WORK.
  ENDIF.

  IF it_saida IS NOT INITIAL.

    LOOP AT it_saida ASSIGNING <l_saida>.

      <l_saida>-data_ultima_modif = sy-datum.
      <l_saida>-hora_ultima_modif = sy-uzeit.
      <l_saida>-ultimo_modificador = sy-uname.

      MOVE-CORRESPONDING <l_saida> TO wa_ZSDT0361.

      MODIFY zsdt0361 FROM wa_ZSDT0361.

    ENDLOOP.

    CALL METHOD obj_alv->refresh_table_display( ).

    COMMIT WORK.

    MESSAGE 'Registro(s) gravado(s) com sucesso!' TYPE 'S'.

  ENDIF.

  CLEAR gv_modif.
  FREE: it_saida_final.

ENDFORM.                    " SALVAR_REG
