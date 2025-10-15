*&---------------------------------------------------------------------*
*&  Include           ZFIR0074PRINT
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  create_object
*&---------------------------------------------------------------------*
*       Criando o Container e o Grid para receber ALV
*----------------------------------------------------------------------*
FORM create_object USING custom.

  IF o_custom IS INITIAL.
    CREATE OBJECT o_custom
      EXPORTING
        container_name = custom.
  ENDIF.

  IF o_grid IS INITIAL.
    CREATE OBJECT o_grid
      EXPORTING
        i_parent = o_custom.
  ENDIF.

ENDFORM.                                                    " create_object

*&---------------------------------------------------------------------*
*&      Form  create_fieldcat
*&---------------------------------------------------------------------*
*       Preenchendo a Tabela FieldCat
*----------------------------------------------------------------------*
FORM create_fieldcat USING VALUE(p_structure).

  REFRESH i_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_structure
    CHANGING
      ct_fieldcat            = i_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.                                                    " create_fieldcat

*&---------------------------------------------------------------------*
*&      Form  fieldcat_layout
*&---------------------------------------------------------------------*
*       Ajustando o Layout da Tabela de FieldCat
*----------------------------------------------------------------------*
FORM fieldcat_layout USING VALUE(p_structure).


  DATA: wa_fieldcat TYPE lvc_s_fcat.
  CLEAR wa_fieldcat.

  IF p_structure = 'ZIMP_APROVADOR'.

    LOOP AT i_fieldcat INTO wa_fieldcat.
      CASE wa_fieldcat-fieldname.
        WHEN 'BUKRS'.
          wa_fieldcat-coltext = 'Empresa De'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.

        WHEN 'BUKRS_ATE'.
          wa_fieldcat-coltext = 'Empresa Até'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'WERKS'.
          wa_fieldcat-coltext = 'Centro'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'DEP_RESP'.
          wa_fieldcat-coltext = 'Departamento'.
          wa_fieldcat-col_pos = 5.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-ref_table = 'ZIMP_CAD_DEPTO'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 6.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZIMP_APROVADOR'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 8.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'WAERS'.
          wa_fieldcat-coltext = 'Moeda'.
          wa_fieldcat-col_pos = 9.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZIMP_APROVADOR'.
        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Valor De'.
          wa_fieldcat-col_pos = 10.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 11.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 12.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 13.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 14.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 15.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 16.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 17.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 18.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 19.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário.'.
          wa_fieldcat-col_pos = 20.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 7.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 5.
    wa_fieldcat-fieldname = 'DEP_RESP_DESC'.
    wa_fieldcat-coltext = 'Nome Departamento'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

  ELSEIF p_structure = 'ZGLT037'.

    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.
        WHEN 'BUKRS'.
          wa_fieldcat-coltext = 'Empresa De'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'BUKRS_ATE'.
          wa_fieldcat-coltext = 'Empresa Até'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'DEP_RESP'.
          wa_fieldcat-coltext = 'Departamento'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-ref_table = 'ZIMP_CAD_DEPTO'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 6.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZGLT037'.
        WHEN 'PGT_FORN'.
          wa_fieldcat-coltext = 'Pgto. Fornecedor'.
          wa_fieldcat-col_pos = 8.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 9.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'WAERS'.
          wa_fieldcat-coltext = 'Moeda'.
          wa_fieldcat-col_pos = 10.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZGLT037'.
        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Valor De'.
          wa_fieldcat-col_pos = 11.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 12.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 13.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 14.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 15.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 16.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 17.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 18.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 19.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 20.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 21.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 7.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 5.
    wa_fieldcat-fieldname = 'DEP_RESP_DESC'.
    wa_fieldcat-coltext = 'Nome Departamento'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

  ELSEIF p_structure = 'ZINV_APROVADOR'.

    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.
        WHEN 'BUKRS'.
          wa_fieldcat-coltext = 'Empresa De'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'BUKRS_ATE'.
          wa_fieldcat-coltext = 'Empresa Até'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'TIPO'.
          wa_fieldcat-coltext = 'Tp.Pgto'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          "CLEAR: WA_FIELDCAT-REF_TABLE.
          "WA_FIELDCAT-REF_TABLE = 'ZINV_APROVADOR'.
        WHEN 'TP_OPERACAO'.
          wa_fieldcat-coltext = 'Operação'.
          wa_fieldcat-col_pos = 6.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          "WA_FIELDCAT-REF_TABLE = 'ZINV_APROVADOR'.
        WHEN 'MATNR'.
          wa_fieldcat-coltext = 'Material'.
          wa_fieldcat-col_pos = 8.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 10.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          "WA_FIELDCAT-REF_TABLE = 'ZINV_APROVADOR'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 12.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'WAERS'.
          wa_fieldcat-coltext = 'Moeda'.
          wa_fieldcat-col_pos = 13.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          "WA_FIELDCAT-REF_TABLE = 'ZINV_APROVADOR'.
        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Valor De'.
          wa_fieldcat-col_pos = 14.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 15.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 16.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 17.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 18.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 19.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 20.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 21.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 22.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 23.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 24.
          wa_fieldcat-key = ''.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 5.
    wa_fieldcat-fieldname = 'TIPO_DESC'.
    wa_fieldcat-coltext = 'Descrição Tp.Pagto'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 7.
    wa_fieldcat-fieldname = 'DS_OPERACAO'.
    wa_fieldcat-coltext = 'Nome Operação'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 9.
    wa_fieldcat-fieldname = 'MAKTX'.
    wa_fieldcat-coltext = 'Descrição Material'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 11.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

  ELSEIF p_structure = 'ZADTO_APROVADOR'.

    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.
        WHEN 'BUKRS'.
          wa_fieldcat-coltext = 'Empresa De'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'BUKRS_ATE'.
          wa_fieldcat-coltext = 'Empresa Até'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'DEP_RESP'.
          wa_fieldcat-coltext = 'Departamento'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-ref_table = 'ZIMP_CAD_DEPTO'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 6.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZADTO_APROVADOR'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 8.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'WAERS'.
          wa_fieldcat-coltext = 'Moeda'.
          wa_fieldcat-col_pos = 9.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZADTO_APROVADOR'.
        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Valor De'.
          wa_fieldcat-col_pos = 10.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 11.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 12.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 13.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 14.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 15.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 16.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 17.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 18.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 19.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 20.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 5.
    wa_fieldcat-fieldname = 'DEP_RESP_DESC'.
    wa_fieldcat-coltext = 'Nome Departamento'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 7.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

  ELSEIF p_structure = 'ZSDT0141'.

    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.
        WHEN 'BUKRS'.
          wa_fieldcat-coltext = 'Empresa De'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'BUKRS_ATE'.
          wa_fieldcat-coltext = 'Empresa Até'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'VKBUR'.
          wa_fieldcat-coltext = 'Esc.Venda'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'VKBUR_ATE'.
          wa_fieldcat-coltext = 'Esc.Venda Até'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 6.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZSDT0141'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 9.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'WAERS'.
          wa_fieldcat-coltext = 'Moeda'.
          wa_fieldcat-col_pos = 10.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZSDT0141'.
        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Valor De'.
          wa_fieldcat-col_pos = 11.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 12.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 13.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 14.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 15.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 16.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 17.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 18.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 19.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 20.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 21.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 7.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

  ELSEIF p_structure = 'ZSDT0152'.

    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.
        WHEN 'VKORG'.
          wa_fieldcat-coltext = 'Org.Vendas'.
          wa_fieldcat-col_pos = 1.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.

        WHEN 'WERKS'.
          wa_fieldcat-coltext = 'Centro De'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'WERKS_ATE'.
          wa_fieldcat-coltext = 'Centro Até'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 5.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZSDT0152'.
        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Valor De'.
          wa_fieldcat-col_pos = 6.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 7.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 8.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 9.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 10.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 11.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'WAERS'.
          wa_fieldcat-coltext = 'Moeda'.
          wa_fieldcat-col_pos = 12.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZSDT0152'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 13.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 14.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 15.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 16.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 17.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 5.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

  ELSEIF p_structure = 'ZLEST0156'.

    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.

        WHEN 'BUKRS'.
          wa_fieldcat-coltext = 'Empresa De'.
          wa_fieldcat-col_pos = 1.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'BUKRS_ATE'.
          wa_fieldcat-coltext = 'Empresa Até'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZSDT0152'.

        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Valor De'.
          wa_fieldcat-col_pos = 5.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 6.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 7.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 8.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 09.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 10.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 11.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 12.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 13.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 14.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 15.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 5.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.


  ELSEIF p_structure = 'ZSDT0161'.

    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.

        WHEN 'BUKRS'.
          wa_fieldcat-coltext = 'Empresa De'.
          wa_fieldcat-col_pos = 1.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'BUKRS_ATE'.
          wa_fieldcat-coltext = 'Empresa Até'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'VKBUR'.
          wa_fieldcat-coltext = 'Esc.Venda'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'VKBUR_ATE'.
          wa_fieldcat-coltext = 'Esc.Venda Até'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'TP_VENDA'.
          wa_fieldcat-coltext = 'Tp. Venda De'.
          wa_fieldcat-col_pos = 5.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'TP_VENDA_ATE'.
          wa_fieldcat-coltext = 'Tp. Venda Até'.
          wa_fieldcat-col_pos = 6.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'WAERS'.
          wa_fieldcat-coltext = 'Moeda'.
          wa_fieldcat-col_pos = 7.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 8.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 9.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZSDT0152'.
        WHEN 'NAME'.
          wa_fieldcat-coltext = 'Nome'.
          wa_fieldcat-col_pos = 10.
        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Valor De'.
          wa_fieldcat-col_pos = 11.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 12.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 13.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 14.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 15.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 16.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 17.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 18.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 19.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 20.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 21.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 10.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.


  ELSEIF p_structure = 'ZMMT0150'.
    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.
        WHEN 'BUKRS'.
          wa_fieldcat-coltext = 'Empresa De'.
          wa_fieldcat-col_pos = 1.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'BUKRS_ATE'.
          wa_fieldcat-coltext = 'Empresa Até'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-edit = 'X'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 8.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 9.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZMMT0150'.

        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Percentual De'.
          wa_fieldcat-col_pos = 10.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Percentual Até'.
          wa_fieldcat-col_pos = 11.
          wa_fieldcat-edit = 'X'.

        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 13.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 14.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 15.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 16.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 17.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 18.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 19.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 20.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 21.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.
      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.
    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 5.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

  ELSEIF p_structure = 'ZSDT0336'.

    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.

        WHEN 'BUKRS'.
          wa_fieldcat-coltext = 'Empresa De'.
          wa_fieldcat-col_pos = 1.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'BUKRS_ATE'.
          wa_fieldcat-coltext = 'Empresa Até'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'VKBUR'.
          wa_fieldcat-coltext = 'Esc.Venda'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'VKBUR_ATE'.
          wa_fieldcat-coltext = 'Esc.Venda Até'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'TP_NEGOCIO_DE'.
          wa_fieldcat-coltext = 'Tp. Negócio De'.
          wa_fieldcat-col_pos = 5.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'TP_NEGOCIO_ATE'.
          wa_fieldcat-coltext = 'Tp. Negócio Até'.
          wa_fieldcat-col_pos = 6.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'WAERS'.
          wa_fieldcat-coltext = 'Moeda'.
          wa_fieldcat-col_pos = 7.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 8.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 9.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZSDT0152'.
        WHEN 'NAME'.
          wa_fieldcat-coltext = 'Nome'.
          wa_fieldcat-col_pos = 10.
        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Valor De'.
          wa_fieldcat-col_pos = 11.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 12.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 13.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 14.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 15.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 16.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 17.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 18.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 19.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 20.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 21.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 10.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

  ELSEIF p_structure = 'ZFIWRT0033'. "150184 CS2024000781 Aprovações ZNFW - PSA

    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.

        WHEN 'DEP_RESP'.
          wa_fieldcat-coltext = 'Departamento'.
          wa_fieldcat-col_pos = 1.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZIMP_CAD_LOTE'.
          wa_fieldcat-ref_field = 'DEP_RESP'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZIMP_APROVADOR'.
          wa_fieldcat-ref_field = 'APROVADOR'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 5.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 6.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 7.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 8.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 9.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 10.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 11.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 12.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 13.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    " 06.05.2025 - 174338 - RAMON -->
  ELSEIF p_structure = 'ZSDT0385'.

    LOOP AT i_fieldcat INTO wa_fieldcat.

      CASE wa_fieldcat-fieldname.
        WHEN 'BUKRS'.
          wa_fieldcat-coltext = 'Empresa De'.
          wa_fieldcat-col_pos = 2.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'BUKRS_ATE'.
          wa_fieldcat-coltext = 'Empresa Até'.
          wa_fieldcat-col_pos = 3.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'VKBUR'.
          wa_fieldcat-coltext = 'Esc.Venda'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'VKBUR_ATE'.
          wa_fieldcat-coltext = 'Esc.Venda Até'.
          wa_fieldcat-col_pos = 4.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'APROVADOR'.
          wa_fieldcat-coltext = 'Aprovador'.
          wa_fieldcat-col_pos = 6.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZSDT0385'.
        WHEN 'NIVEL'.
          wa_fieldcat-coltext = 'Nível'.
          wa_fieldcat-col_pos = 9.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
        WHEN 'WAERS'.
          wa_fieldcat-coltext = 'Moeda'.
          wa_fieldcat-col_pos = 10.
          wa_fieldcat-key = ''.
          wa_fieldcat-edit = 'X'.
          wa_fieldcat-f4availabl = 'X'.
          wa_fieldcat-ref_table = 'ZSDT0385'.
        WHEN 'VALOR_DE'.
          wa_fieldcat-coltext = 'Valor De'.
          wa_fieldcat-col_pos = 11.
          wa_fieldcat-edit = 'X'.
        WHEN 'VALOR_ATE'.
          wa_fieldcat-coltext = 'Valor Até'.
          wa_fieldcat-col_pos = 12.
          wa_fieldcat-edit = 'X'.
        WHEN 'DT_VAL_DE'.
          wa_fieldcat-coltext = 'Validade De'.
          wa_fieldcat-col_pos = 13.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_DE'.
          wa_fieldcat-coltext = 'Hora de'.
          wa_fieldcat-col_pos = 14.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'DT_VAL_ATE'.
          wa_fieldcat-coltext = 'Validade Até'.
          wa_fieldcat-col_pos = 15.
          wa_fieldcat-edit = 'X'.
        WHEN 'HR_VAL_ATE'.
          wa_fieldcat-coltext = 'Hora Até'.
          wa_fieldcat-col_pos = 16.
          "WA_FIELDCAT-EDIT = 'X'.
        WHEN 'MOTIVO'.
          wa_fieldcat-coltext = 'Motivo'.
          wa_fieldcat-col_pos = 17.
          wa_fieldcat-edit = 'X'.
        WHEN 'TRANSF_APROV'.
          wa_fieldcat-coltext = 'Transferência'.
          wa_fieldcat-col_pos = 18.
          wa_fieldcat-edit = 'X'.
        WHEN 'DATA_ATUAL'.
          wa_fieldcat-coltext = 'Data Atual.'.
          wa_fieldcat-col_pos = 19.
        WHEN 'HORA_ATUAL'.
          wa_fieldcat-coltext = 'Hora Atual.'.
          wa_fieldcat-col_pos = 20.
        WHEN 'USUARIO'.
          wa_fieldcat-coltext = 'Usuário'.
          wa_fieldcat-col_pos = 21.
        WHEN OTHERS.
          "DO NOTHING
      ENDCASE.

      wa_fieldcat-col_opt = 'X'.
      MODIFY i_fieldcat FROM wa_fieldcat.

    ENDLOOP.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 7.
    wa_fieldcat-fieldname = 'NAME'.
    wa_fieldcat-coltext = 'Nome Aprovador'.
    wa_fieldcat-col_opt = 'X'.
    APPEND wa_fieldcat TO i_fieldcat.

    " 06.05.2025 - 174338 - RAMON --<



  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  display_output
*&---------------------------------------------------------------------*
*       Imprime Tabela de Saída
*----------------------------------------------------------------------*
FORM display_output USING itab .

  DATA: w_variant  TYPE disvariant.
  w_variant-report = sy-repid.

  gs_layout-stylefname = 'CELLTAB'.

  CALL METHOD o_grid->set_table_for_first_display
    EXPORTING
      is_variant                    = w_variant
      i_save                        = 'A'
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = itab
      it_fieldcatalog               = i_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE 'X' TO ck_disp.

  CALL METHOD o_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

  CALL METHOD o_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

ENDFORM.                                                    " display_output

*&---------------------------------------------------------------------*
*&      Form  free_objects
*&---------------------------------------------------------------------*
*       Free Objects
*----------------------------------------------------------------------*
FORM free_objects .

  CALL METHOD o_grid->free.
  CLEAR: o_grid.

  CALL METHOD o_custom->free.
  CLEAR: o_custom.

ENDFORM.
