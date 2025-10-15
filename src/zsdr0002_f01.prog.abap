*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZSDR0002_F01                                                                         *
*& Chamado        : USER STORY 168894                                                                    *
*& Data           : 18/03/2025                                                                           *
*& Especificado   : Leonardo Portela                                                                     *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 18/03/2025|DEVK9A1XAW |NSEGATIN       | Cadastro de Aprovador 1x1. Desenvolvimento inicial.           *
*--------------------------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      FORM CRIAR_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       Criação do Catalogo de Campos do ALV Grid
*----------------------------------------------------------------------*
FORM criar_field_catalog.

  FREE: wa_fcat, it_fcat.
* Carrega a estrutura de Criação do Catalogo de Campos do ALV Grid.
  PERFORM estrutura_alv USING:
*   POS  Ref. Table       Field Name   Table Out  Field Out    Label           Len   Edit     Sum      Emphasize Justify  Hotspot  F4       Checkbox No out
      1  'ZSDTVINC_APROV' 'APROVADOR'  'IT_SAIDA' 'APROVADOR'  'Aprovador'     '20'  abap_on  abap_off abap_off  abap_off abap_off abap_on  abap_off abap_off,
      2  'ZSDTVINC_APROV' 'NAME_TEXT'  'IT_SAIDA' 'NAME_TEXT'  'Nome completo' '40'  abap_off abap_off abap_off  abap_off abap_off abap_off abap_off abap_off,
      3  'ZSDTVINC_APROV' 'BUKRS'      'IT_SAIDA' 'BUKRS'      'Empresa'       '4'   abap_on  abap_off abap_off  abap_off abap_off abap_off abap_off abap_off,
      5  'ZSDTVINC_APROV' 'GRP_EMAIL'  'IT_SAIDA' 'GRP_EMAIL'  'Grp. E-mail'   '25'  abap_on  abap_off abap_off  abap_off abap_off abap_off abap_off abap_off,
      6  'ZSDTVINC_APROV' 'ATIVO'      'IT_SAIDA' 'ATIVO'      'Ativo'         '6'   abap_on  abap_off abap_off  abap_off abap_off abap_off abap_on  abap_off,
      7  'ZSDTVINC_APROV' 'US_CRIACAO' 'IT_SAIDA' 'US_CRIACAO' 'Criado por'    '12'  abap_off abap_off abap_off  abap_off abap_off abap_off abap_off abap_off,
      8  'ZSDTVINC_APROV' 'DT_CRIACAO' 'IT_SAIDA' 'DT_CRIACAO' 'Criado em'     '10'  abap_off abap_off abap_off  abap_off abap_off abap_off abap_off abap_off,
      9  'ZSDTVINC_APROV' 'HR_CRIACAO' 'IT_SAIDA' 'HR_CRIACAO' 'Hora Cria.'    '10'  abap_off abap_off abap_off  abap_off abap_off abap_off abap_off abap_off.

ENDFORM.                    " CRIAR_FIELD_CATALOG_XML
*&---------------------------------------------------------------------*
*&      FORM estrutura_alv
*&---------------------------------------------------------------------*
*       Carrega a estrutura de Criação do Catalogo de Campos do ALV Grid
*----------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       Seleciona os dados para serem exibidos e/ou alterados
*----------------------------------------------------------------------*
FORM selecionar_dados.

  DATA: lt_celltab TYPE lvc_t_styl,
        ls_style   TYPE lvc_s_styl.

  SELECT * FROM zsdtvinc_aprov INTO TABLE tg_vinc_aprov ORDER BY PRIMARY KEY.

  IF sy-subrc IS INITIAL.
    CLEAR it_saida.

    LOOP AT tg_vinc_aprov INTO eg_vinc_aprov.
* Objeto de autorização Autorização Aprovadores 1x1
      AUTHORITY-CHECK OBJECT 'ZSD_1X1_AP'
       ID 'BUKRS' FIELD eg_vinc_aprov-bukrs.

      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING eg_vinc_aprov TO wa_saida.
        wa_saida-row_id = sy-tabix.
        ls_style-fieldname = 'APROVADOR'.
        ls_style-style     = cl_gui_alv_grid=>mc_style_disabled.
        INSERT ls_style INTO TABLE wa_saida-celltab.

        APPEND wa_saida TO it_saida.
        CLEAR wa_saida.

      ENDIF.

    ENDLOOP.

    SORT it_saida BY aprovador.

  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  DELETAR_REG
*&---------------------------------------------------------------------*
*       Excluir linha do ALV Grid
*----------------------------------------------------------------------*
FORM deletar_reg.

  DATA: var_answer  TYPE c.

  FIELD-SYMBOLS <l_saida> TYPE ty_saida.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF     lines( it_sel_rows ) GT 1.
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.

  ELSEIF lines( it_sel_rows ) EQ 0.
    MESSAGE 'Selecione ao menus uma linha!' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.

  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Confirma exclusão do Aprovador?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = space
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  READ TABLE it_saida INTO wa_saida INDEX wa_sel_rows-index.

  IF sy-subrc IS INITIAL.
    DELETE FROM zsdtvinc_aprov WHERE aprovador EQ wa_saida-aprovador
                                 AND bukrs     EQ wa_saida-bukrs.

    IF sy-subrc IS INITIAL.
      COMMIT WORK.

    ELSE.
      ROLLBACK WORK.

    ENDIF.

    DELETE it_saida INDEX wa_sel_rows-index.
    DELETE it_saida_update WHERE aprovador EQ wa_saida-aprovador.

  ENDIF.

  CALL METHOD obj_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.                    " DELETAR_REG
*&---------------------------------------------------------------------*
*&      Form  SALVAR_REG
*&---------------------------------------------------------------------*
*       Salvca registro inserido da tela ALV Grid.
*----------------------------------------------------------------------*
FORM salvar_reg.

  FIELD-SYMBOLS <l_saida> TYPE ty_saida.

  DATA: tl_vinc_aprov TYPE TABLE OF zsdtvinc_aprov.

  DATA: ls_style TYPE lvc_s_styl.

  DATA: vl_name_first TYPE ad_namefir,
        vl_name_last  TYPE ad_namelas.

  CLEAR: eg_vinc_aprov,
         gv_erro.
* Chama a verificação de alteração de dados na Grid ALV.
  CALL METHOD obj_alv->check_changed_data( ).

  CHECK gv_modif IS NOT INITIAL AND gv_erro IS INITIAL.
  CHECK it_saida IS NOT INITIAL.

  IF it_saida_update IS INITIAL.
    MESSAGE 'Não há dado(s) para ser(em) salvo(s).' TYPE 'W'.
    RETURN.

  ENDIF.
* Monta verificação modificações de dados.
  MOVE-CORRESPONDING it_saida TO tl_vinc_aprov.
  eg_vinc_aprov-mandt = sy-mandt.
  MODIFY tl_vinc_aprov FROM eg_vinc_aprov TRANSPORTING mandt WHERE mandt IS INITIAL.
* Verifica se mesmo com modificações nos dados, ainda não houve alteração.
  IF tl_vinc_aprov EQ tg_vinc_aprov.
    MESSAGE 'Não há dado(s) para ser(em) salvo(s).' TYPE 'W'.
    RETURN.

  ENDIF.

  LOOP AT it_saida_update INTO wa_saida.
    READ TABLE it_saida ASSIGNING <l_saida> WITH KEY aprovador = wa_saida-aprovador
                                                     bukrs     = wa_saida-bukrs.

    IF sy-subrc IS INITIAL.
      DATA(vl_tabix) = sy-tabix.
* Objeto de autorização Autorização Aprovadores 1x1
      AUTHORITY-CHECK OBJECT 'ZSD_1X1_AP' ID 'BUKRS' FIELD wa_saida-bukrs.

      IF NOT sy-subrc       IS INITIAL OR
             wa_saida-bukrs IS INITIAL.
        ROLLBACK WORK.
* Marca a celula que está sendo criticada.
        PERFORM zf_mark_cell_check USING vl_tabix
                                         3.
        MESSAGE 'Sem autorização para essa(s) Empresa(s)' TYPE 'E'.

      ENDIF.

    ELSE.
      CONTINUE.

    ENDIF.
* Validação do Aprovador.
    IF <l_saida>-aprovador IS INITIAL.
      ROLLBACK WORK.
* Marca a celula que está sendo criticada.
      PERFORM zf_mark_cell_check USING vl_tabix
                                       1.
      MESSAGE 'APROVADOR é um campo obrigatório!' TYPE 'E'.

    ELSE.
* Validação do E-mail.
      IF <l_saida>-grp_email IS INITIAL.
        ROLLBACK WORK.
* Marca a celula que está sendo criticada.
        PERFORM zf_mark_cell_check USING vl_tabix
                                         4.
        MESSAGE 'Grp. E-mail é um campo obrigatório!' TYPE 'E'.

      ELSE.
* Informa data, hora e usuário de criação.
        <l_saida>-dt_criacao = sy-datlo.
        <l_saida>-hr_criacao = sy-timlo.
        <l_saida>-us_criacao = sy-uname.

        IF <l_saida>-name_text IS INITIAL.
* Seleciona o Nome e o sobrenome conforme o ID do Usuário.
          SELECT SINGLE b~name_first b~name_last
            FROM usr21 AS a
            INNER JOIN adrp AS b
             ON b~persnumber EQ a~persnumber
            INTO (vl_name_first, vl_name_last)
          WHERE a~bname EQ <l_saida>-aprovador.

          IF sy-subrc IS INITIAL.
            CONCATENATE vl_name_first vl_name_last INTO <l_saida>-name_text SEPARATED BY space.

          ENDIF.

        ENDIF.
* Verifiva se é registro novo ou não.
        CASE <l_saida>-new_line.
          WHEN abap_off. "Registro existente - Atualização
            READ TABLE tg_vinc_aprov INTO DATA(el_vinc_aprov) INDEX <l_saida>-row_id.
* Atualiza dados já existentes na tabela.
            UPDATE zsdtvinc_aprov
               SET bukrs     = <l_saida>-bukrs
                   grp_email = <l_saida>-grp_email
                   ativo     = <l_saida>-ativo
            WHERE aprovador EQ el_vinc_aprov-aprovador
              AND bukrs     EQ el_vinc_aprov-bukrs.

          WHEN abap_on.  "Registro novo - Inserção
* Move dados para a área de memória de TI para Salva dados na tabela.
            MOVE-CORRESPONDING <l_saida> TO eg_vinc_aprov.
* Salva dados novos na tabela.
            MODIFY zsdtvinc_aprov FROM eg_vinc_aprov.

          WHEN OTHERS.
*               Do nothing
        ENDCASE.
* Fecha o campo aprovador para edição.
        ls_style-fieldname = 'APROVADOR'.
        ls_style-style     = cl_gui_alv_grid=>mc_style_disabled.
        INSERT ls_style INTO TABLE <l_saida>-celltab.

      ENDIF.

    ENDIF.

  ENDLOOP.
* Efetiva os dados na tabela.
  COMMIT WORK.
* Seleciona os dados para serem exibidos e/ou alterados
  PERFORM selecionar_dados.
* Atualiza a tega da GRID ALV.
  CALL METHOD obj_alv->refresh_table_display( ).
  MESSAGE 'Registro(s) gravado(s) com sucesso!' TYPE 'S'.

  CLEAR: gv_modif, it_saida_update, sy-ucomm.

ENDFORM.                    " SALVAR_REG
*&---------------------------------------------------------------------*
*& Form zf_mark_cell_check
*&---------------------------------------------------------------------*
*& Marca a celula que está sendo criticada
*&---------------------------------------------------------------------*
*& -->UV_ROW_ID Índice de linhas das tabelas internas
*& -->UV_COL_ID Índice de colunas das tabelas internas
*&---------------------------------------------------------------------*
FORM zf_mark_cell_check USING uv_row_id TYPE int4
                              uv_col_id TYPE int4.

  DATA: tl_cell  TYPE TABLE OF lvc_s_ceno.

  APPEND INITIAL LINE TO tl_cell ASSIGNING FIELD-SYMBOL(<fs_cell>).
* Carrega a posição da célula de linha e coluna que foi criticada com duplicidade de Aprovador
* para destacar na Grid do ALV.
  <fs_cell>-col_id = uv_col_id.
  <fs_cell>-row_id = uv_row_id.
* Marca as celulas da Grid ALV que foram criticadas.
  CALL METHOD obj_alv->set_selected_cells_id
    EXPORTING
      it_cells = tl_cell.

ENDFORM.
