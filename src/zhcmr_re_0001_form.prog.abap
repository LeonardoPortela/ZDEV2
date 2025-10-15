*&---------------------------------------------------------------------*
*&  Include           ZHCMR_RE_0001_FORM
*&---------------------------------------------------------------------*

FORM fm_selecao.

*  CLEAR message.
*  message = |Aguarde, selecionando dados|.
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      text = message.

  SELECT * FROM zhcmt_f_uniorg
    WHERE orgeh IN @p_uorg
    AND cod_ccusto IN @p_cust AND pernr_gestor IN @p_gest
    INTO CORRESPONDING FIELDS OF TABLE @it_zhcmr.


  SORT it_zhcmr BY orgeh cod_ccusto pernr_gestor.
  "DELETE ADJACENT DUPLICATES FROM it_zhcmr.


  LOOP AT it_zhcmr INTO wa_saida.
    APPEND wa_saida TO it_saida.
    CLEAR: wa_saida..
  ENDLOOP.

  CLEAR: wa_saida.

ENDFORM.


MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'SAIR'.
      LEAVE PROGRAM.
    WHEN 'VOLTAR'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

FORM fm_cria_fieldcat .
  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  git_fcat_pend = VALUE lit_fieldcat_aux(
( tabname = 'T_SAIDA'  fieldname = 'ORGEH            '            coltext = 'Cod. Uniorg'                    col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'ORGEH            '         )
( tabname = 'T_SAIDA'  fieldname = 'AREA_FOLHA       '            coltext = 'Área Proc. Folha'               col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'AREA_FOLHA       '         )
( tabname = 'T_SAIDA'  fieldname = 'COD_CCUSTO       '            coltext = 'Cod. Centro Custo'              col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'COD_CCUSTO       '         )
( tabname = 'T_SAIDA'  fieldname = 'COD_FILIAL       '            coltext = 'Cod. Filial'                    col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'COD_FILIAL       '         )
( tabname = 'T_SAIDA'  fieldname = 'COD_EMPRESA      '            coltext = 'Cod. Empresa'                   col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'COD_EMPRESA      '         )
( tabname = 'T_SAIDA'  fieldname = 'STEXT            '            coltext = 'Nome Uniorg'                    col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'STEXT            '         )
( tabname = 'T_SAIDA'  fieldname = 'PERNR_GESTOR     '            coltext = 'Pernr Gestor'                   col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'PERNR_GESTOR     '         )
( tabname = 'T_SAIDA'  fieldname = 'CPF_GESTOR       '            coltext = 'CPF Gestor'                     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'CPF_GESTOR       '         )
( tabname = 'T_SAIDA'  fieldname = 'NOME_GESTOR      '            coltext = 'Nome Gestor'                    col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'NOME_GESTOR      '         )
( tabname = 'T_SAIDA'  fieldname = 'EMAIL_GESTOR     '            coltext = 'E-mail Gestor'                  col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'EMAIL_GESTOR     '         )
( tabname = 'T_SAIDA'  fieldname = 'NOME_CCUSTO      '            coltext = 'Nome Centro Custo'              col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'NOME_CCUSTO      '         )
( tabname = 'T_SAIDA'  fieldname = 'NOME_FILIAL      '            coltext = 'Nome Filial'                    col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'NOME_FILIAL      '         )
( tabname = 'T_SAIDA'  fieldname = 'NOME_EMPRESA     '            coltext = 'Nome Empresa'                   col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'NOME_EMPRESA     '         )
( tabname = 'T_SAIDA'  fieldname = 'PERSK_GESTOR     '            coltext = 'Sub. Grup. Empreg'              col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'PERSK_GESTOR     '         )
( tabname = 'T_SAIDA'  fieldname = 'DESC_PERSK_GESTOR'            coltext = 'Nome Sub. Grup. Empreg'         col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'DESC_PERSK_GESTOR'         )
  ).
ENDFORM.

FORM fm_alv.
  gs_variant-report = sy-repid.

  PERFORM fm_cria_fieldcat.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(

  EXPORTING
    i_titulo  = 'Relatório de Consulta na Tabela ZHCMT_F_UNIORG - ' &&  lva_data
     i_filtros = VALUE zif_screen_linha_filtro_t(
*   ( parametro = 'Filtros: '  valor = v_empresa       valor2 = v_centro     )
*   ( parametro = ''           valor = v_perido        valor2 = v_deposito   )
*   ( parametro = ''           valor = v_material      valor2 = v_pedido     )
*   ( parametro = ''           valor = v_nf            valor2 = v_fornecedor )
*   ( parametro = ''           valor = v_requisitante  valor2 = v_tpmov      )
   )

  CHANGING
    alv = gob_gui_alv_grid
    )
    EQ abap_true.

    wa_layout-sel_mode   = 'A'.

    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = gs_variant
        i_save                        = 'A'
        is_layout                     = wa_layout
      CHANGING
        it_outtab                     = it_saida
        it_fieldcatalog               = git_fcat_pend
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ENDIF.
ENDFORM.

FORM fm_exibirdados .
  CALL SCREEN '0100'.
ENDFORM.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST0100'.
  "SET TITLEBAR 'xxx'.
  PERFORM fm_alv.
ENDMODULE.
