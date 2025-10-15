*&---------------------------------------------------------------------*
*& Include          ZHCMR_BN0026_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_call_function
*&---------------------------------------------------------------------*
FORM f_call_function .

  DATA: lt_dyn_tab   TYPE REF TO data,
        lv_tab_name  TYPE char50,
        lv_function  TYPE char50,
        lv_function2 TYPE char50. "BUG - 151348 - CBRAND

  CASE abap_true.
    WHEN p_check1.
      lv_tab_name = 'TY_SAIDA'.
      lv_function = 'ZHCMF_WEBSEMPRE_FUNC_NOV_ADM'.
    WHEN p_check2.
      lv_tab_name = 'ZHCMS_WEBSEMPRE_FUNC_NOV_PLA_S'.
      lv_function = 'ZHCMF_WEBSEMPRE_FUNC_NOV_PLA'.
    WHEN p_check3.
      lv_tab_name = 'ZHCMS_WEBSEMPRE_DEP_NOV_PLA_S'.
      lv_function = 'ZHCMF_WEBSEMPRE_DEP_NOV_PLA'.
    WHEN p_check4.
      lv_tab_name  = 'ZHCMF_RETURN_FUNC_EXC_STR'.
      lv_function  = 'ZHCMF_RETURN_FUNC_EXC'.
      lv_function2 = 'ZHCMF_RETURN_FUNC_DEM'. "BUG - 151348 - CBRAND
    WHEN p_check5.
      lv_tab_name = 'ZHCMS_WEBSEMPRE_FUNC_TRS_ACM_S'.
      lv_function = 'ZHCMF_WEBSEMPRE_FUNC_TRS_ACM'.
    WHEN p_check6.
      lv_tab_name = 'ZHCMS_WEBSEMPRE_FUNC_TRS_PLN_S'.
      lv_function = 'ZHCMF_WEBSEMPRE_FUNC_TRS_PLN'.
  ENDCASE.

  CREATE DATA lt_dyn_tab TYPE TABLE OF (lv_tab_name).

  ASSIGN lt_dyn_tab->* TO <fs_dyn_tab>.

  IF p_check1 IS INITIAL.

    CALL FUNCTION lv_function
      EXPORTING
        begda    = p_begda
        endda    = p_endda
      TABLES
        it_saida = <fs_dyn_tab>.
*** BUG - 151348 - Inicio - CBRAND
    IF lv_function2 IS NOT INITIAL.
      DATA lva_birthdate TYPE pa0002-gbdat.
      CALL FUNCTION lv_function2
        EXPORTING
          begda    = p_begda
          endda    = p_endda
          pernr    = c_pernr
          tipo     = c_tipo_sol
        TABLES
          it_saida = t_saida_aux
          t_arearh = t_arearh
          t_kostl  = t_kostl
          t_uniorg = t_uniorg.

      IF t_saida_aux IS NOT INITIAL.
        FIELD-SYMBOLS: <fs_saida_aux> TYPE zhcmf_return_func_exc_str.
        LOOP AT t_saida_aux INTO DATA(lwa_saida_aux).

          APPEND INITIAL LINE TO <fs_dyn_tab> ASSIGNING <fs_saida_aux>.

          IF <fs_saida_aux> IS ASSIGNED.

            <fs_saida_aux>-dep_name = lwa_saida_aux-cname.
            <fs_saida_aux>-icnum    = lwa_saida_aux-cpf_nr.
            <fs_saida_aux>-cpf_nr   = lwa_saida_aux-cpf_nr.

            SELECT SINGLE gbdat FROM pa0002 INTO lva_birthdate WHERE pernr = lwa_saida_aux-pernr
               AND  endda >= sy-datum.

              <fs_saida_aux>-fgbdt    = lva_birthdate.

              SELECT SINGLE pernr, famsa, fcnam
               FROM pa0021
               WHERE pernr = @lwa_saida_aux-pernr
                 AND subty = '12'
                 AND objps = ''
                 AND sprps = ''
                 AND endda >= @sy-datum
                 AND begda <= @sy-datum
                 AND seqnr <> '999'
               INTO @DATA(lwa_pa0021) .


                <fs_saida_aux>-mothe    = lwa_pa0021-fcnam.

                SELECT SINGLE pernr, bukrs
                     FROM pa0001
                     WHERE pernr = @lwa_saida_aux-pernr
                         AND endda >= @Sy-datum
                      INTO @DATA(lwa_posicao_demitido) .

                  <fs_saida_aux>-bukrs    = lwa_posicao_demitido-bukrs.
                  <fs_saida_aux>-werks    = lwa_saida_aux-werks.
                  <fs_saida_aux>-kostl    = lwa_saida_aux-kostl.
                  <fs_saida_aux>-stat2    = lwa_saida_aux-descricao_motivo_demissao.

                  CLEAR: lva_birthdate, lwa_pa0021, lwa_posicao_demitido .
                ENDIF.
              ENDLOOP.
            ENDIF.

          ENDIF.
*** BUG - 151348 - Fim - CBRAND
        ELSE.

          PERFORM f_fill_filters.
          CALL FUNCTION 'ZHCMF_WEBSEMPRE_FUNC_NOV_ADM'
            EXPORTING
              begda             = p_begda
              endda             = p_endda
              tipo              = c_u
              pernr_solicitante = c_pernr
              tipo_solicitante  = c_tipo_sol
            TABLES
              it_saida          = t_func
              t_arearh          = t_arearh
              t_kostl           = t_kostl
              t_uniorg          = t_uniorg.
          PERFORM f_processes_data.
        ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_show_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_show_alv.

  IF p_check1 IS NOT INITIAL.
*    CALL SCREEN 2000.
    <fs_dyn_tab> = t_saida.
  ENDIF.

  IF <fs_dyn_tab> IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = alv
          CHANGING
            t_table      = <fs_dyn_tab> ).

        display = alv->get_display_settings( ).
        display->set_striped_pattern( cl_salv_display_settings=>true ).

        layout_settings = alv->get_layout( ).
        layout_key-report = sy-repid.
        layout_settings->set_key( layout_key ).
        layout_settings->set_save_restriction( if_salv_c_layout=>restrict_none ).

        functions = alv->get_functions( ).
        functions->set_all( ).
        functions->set_group_layout( 'X' ).
        functions->set_group_sort( ' ' ).
        functions->set_group_filter( ' ' ).
        functions->set_sort_asc( ' ' ).
        functions->set_sort_desc( ' ' ).
        functions->set_filter( ' ' ).

        columns = alv->get_columns( ).
        columns->set_optimize('X').

        PERFORM f_change_columns.

      CATCH cx_salv_msg INTO message.
    ENDTRY.
    alv->display( ).
  ELSE.
    MESSAGE 'Nenhum dado encontrado' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form init_container
*&---------------------------------------------------------------------*
FORM init_container .
  IF go_container_emp IS NOT BOUND.
    CREATE OBJECT go_container_emp
      EXPORTING
        container_name              = 'CC_EMPLOYEE'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
  IF go_container_dep IS NOT BOUND.
    CREATE OBJECT go_container_dep
      EXPORTING
        container_name              = 'CC_DEPENDENTS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form init_alv
*&---------------------------------------------------------------------*
FORM init_alv .
  IF go_alv_emp IS NOT BOUND.
    CREATE OBJECT go_alv_emp
      EXPORTING
        i_parent          = go_container_emp
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.
  IF go_alv_dep IS NOT BOUND.
    CREATE OBJECT go_alv_dep
      EXPORTING
        i_parent          = go_container_dep
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_fcat
*&---------------------------------------------------------------------*
FORM build_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form show_data
*&---------------------------------------------------------------------*
FORM show_data .

*  call method GO_ALV_DEP->SET_TABLE_FOR_FIRST_DISPLAY
*    changing
*      IT_OUTTAB                     = T_DEPEND
*      IT_FIELDCATALOG               = T_FLDCAT_DEP
*    exceptions
*      INVALID_PARAMETER_COMBINATION = 1
*      PROGRAM_ERROR                 = 2
*      TOO_MANY_LINES                = 3
*      others                        = 4.
*
*
*  call method GO_ALV_EMP->SET_TABLE_FOR_FIRST_DISPLAY
*    changing
*      IT_OUTTAB                     = <FS_DYN_TAB>
*      IT_FIELDCATALOG               = T_FLDCAT_EMP
*    exceptions
*      INVALID_PARAMETER_COMBINATION = 1
*      PROGRAM_ERROR                 = 2
*      TOO_MANY_LINES                = 3
*      others                        = 4.
*
*  set handler: LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK for GO_ALV_EMP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_fill_filters
*&---------------------------------------------------------------------*
FORM f_fill_filters .
  t_arearh = VALUE #( FOR ls_arearh IN s_arearh ( werks = ls_arearh-low ) ).
  t_kostl  = VALUE #( FOR ls_kostl  IN s_kostl  ( kostl = ls_kostl-low ) ).
  t_uniorg = VALUE #( FOR ls_uniorg IN s_uniorg ( orgeh = ls_uniorg-low ) ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_processes_data
*&---------------------------------------------------------------------*
FORM f_processes_data .
  DATA: ls_saida     LIKE LINE OF t_saida,
        ls_saida_dep LIKE LINE OF t_saida.

  LOOP AT t_func INTO DATA(ls_func).
    REFRESH t_depend.
    MOVE-CORRESPONDING ls_func TO ls_saida.

    CALL FUNCTION 'ZHCMF_WEBSEMPRE_FUNC_DEP_NOV'
      EXPORTING
        cpf_nr   = ls_func-cpf_nr
      TABLES
        it_saida = t_depend.

    APPEND ls_saida TO t_saida.
    CHECK t_depend IS NOT INITIAL.
    LOOP AT t_depend INTO DATA(ls_depend).
      ls_saida_dep-cname      = ls_depend-fcnam.
      ls_saida_dep-pernr      = ls_depend-icnum.
      ls_saida_dep-cpf_nr     = ls_depend-cpf_nr.
      ls_saida_dep-gbdat      = ls_depend-fgbdt.
      ls_saida_dep-fcnam_m    = ls_depend-mothe.
      ls_saida_dep-fcnam_p    = ls_depend-znm_paicom.
      ls_saida_dep-famst      = ls_depend-fasex.
      ls_saida_dep-gesch      = ls_depend-famsa.
      ls_saida_dep-natio      = ls_depend-fanat.
      ls_saida_dep-escol      = ls_depend-escol.
      ls_saida_dep-ident_nr   = ls_depend-znumerocom.
      ls_saida_dep-es_emis    = ls_depend-zufcom.
      ls_saida_dep-doc_issuer = ls_depend-zorgao_expcom.
      ls_saida_dep-dt_emis    = ls_depend-zdata_emissaocom.
      ls_saida_dep-usrid_long = ls_depend-usrid_long.
      ls_saida_dep-ident_nr_s = ls_depend-nhcnr.
      ls_saida_dep-bopti      = ls_depend-lbcnr.
      APPEND ls_saida_dep TO t_saida.
    ENDLOOP.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_change_column_name
*&---------------------------------------------------------------------*
FORM f_change_columns .

  IF p_check1 EQ abap_true.

    PERFORM f_set_column_name USING:
          'PERNR'                    'Matricula'       ' '                           ' ',
          'CPF_NR'                   'CPFTitular'      'CPF do Titular '             ' ' ,
          'FCNAM_M'                  'Nome Mãe'        ' '                           ' ',
          'FCNAM_P'                  'Nome Pai'        ' '                           ' ',
          'USRID_LONG'               'Email'           ' '                           ' ' ,
          'IDENT_NR_S '              'CartãoNac.'      'Cartão Nac. de Saúde'        'Cartão Nacional de Saúde' ,
          'BOPTI'                    'PlanoSaúde'      'Plano de Saúde'              ' ' ,
          'AREARH'                   'NomeFilial'      'Nome da Filial'              ' ' ,
          'CCUSTO'                   'NomeCentro'      'Nome Centro de Custo'        ' ' ,
          'UNIORG'                   'NomeUni.Or'      'Nome Uni. Org.'              'Nome Unidade Organizacional' ,
          'DESCRICAO_POSICAO'        'Nome Pos.'       'Nome Posição'                ' ' ,
          'DESCRICAO_CARGO'          'Nome Cargo'      ' '                           ' ' ,
          'DT_CADASTRO'              'DtCadastro'      'Data Cadastro'               ' ' ,
          'STAT2'                    'Cód.Situa.'      'Código Situação'             ' ' ,
          'SITUACAO'                 'Situação'        ' '                           ' ' ,
          'PERNR_GESTOR_IMEDIATO '   'Gestor I'        'Matricula Gestor I'          ' ' ,
          'NOME_GESTOR_IMEDIATO'     'NomeGest.I'      'Nome Gestor I'               ' ' ,
          'PERNR_GESTOR_MEDIATO'     'Gestor M'        'Matricula Gestor M'          ' ' ,
          'NOME_GESTOR_MEDIATO'      'NomeGest.M'      'Nome Gestor M'               ' ' ,
          'STEXT '                   'TipoEnd.'        'Tipo Endereço'               ' ' ,
          'STRAS '                   'Logradouro'      ' '                           ' ' ,
          'POSTA '                   'Complem.'        'Complemento'                 ' ' .

  ELSEIF p_check2 EQ abap_true.
    PERFORM f_set_column_name USING:
          'PERNR'        'Matricula'       ' '                           ' ',
          'FCNAM_M'      'Nome Mãe'        ' '                           ' ',
          'FCNAM_P'      'Nome Pai'        ' '                           ' ',
          'USRID_LONG'   'Email'           ' '                           ' ' ,
          'IDENT_NR_S '  'CartãoNac.'      'Cartão Nac. de Saúde'        'Cartão Nacional de Saúde' ,
          'BOPTI'        'PlanoSaúde'      'Plano de Saúde'              ' ' ,
          'AREARH'       'NomeFilial'      'Nome da Filial'              ' ' ,
          'CCUSTO'       'NomeCentro'      'Nome Centro de Custo'        ' ' ,
          'UNIORG'       'NomeUni.Or'      'Nome Uni. Org.'              'Nome Unidade Organizacional' .

  ELSEIF p_check3 EQ abap_true.
    PERFORM f_set_column_name USING:
          'PERNR'        'Matricula'       ' '                           ' ',
          'FCNAM_M'      'Nome Mãe'        ' '                           ' ',
          'FCNAM_P'      'Nome Pai'        ' '                           ' ',
          'USRID_LONG'   'Email Tit.'      'Email do titular'            ' ' ,
          'IDENT_NR_S '  'CartãoNac.'      'Cartão Nac. de Saúde'        'Cartão Nacional de Saúde' ,
          'BOPTI'        'PlanoSaúde'      'Plano de Saúde'              ' ' ,
          'AREARH'       'NomeFilial'      'Nome da Filial'              ' ' ,
          'CCUSTO'       'NomeCentro'      'Nome Centro de Custo'        ' ' ,
          'ICNUM'        'CPF'             ' '                           ' ' ,
          'CPF_NR'       'CPFTitular'      'CPF do Titular '             ' ' ,
          'UNIORG'       'NomeUni.Or'      'Nome Uni. Org.'              'Nome Unidade Organizacional' .

  ELSEIF p_check4 EQ abap_true.
    PERFORM f_set_column_name USING:
          'ICNUM'        'CPF'             ' '                           ' ' ,
          'CPF_NR'       'CPFTitular'      'CPF do Titular '             ' ' .

  ELSEIF p_check5 EQ abap_true.
    PERFORM f_set_column_name USING:
          'PERNR'        'Matricula'       ' '                           ' ',
          'BOPTI'        'PlanoSaúde'      'Plano de Saúde'              ' ',
          'CPF_NR'       'CPF'             ' '                           ' ',
          'ZDATA_T'      'Dt.Inicio'       'Data Inicio Unimed'          ' ',
          'ZCOD_SIT'     'Cod.Sit.'        'Código Situação'             ' ',
          'ZDESC_SIT'    'Desc.Sit.'       'Desc.Situação'               ' '.

  ELSEIF p_check6 EQ abap_true.
    PERFORM f_set_column_name USING:
          'PERNR'        'Matricula'       ' '                           ' ',
          'CPF_NR'       'CPF'             ' '                           ' ',
          'ZDATA_T'      'Dt.Trans.'       'Data Transferência'          ' ',
          'ZCOD_SIT'     'Cod.Sit.'        'Código Situação'             ' ',
          'ZDESC_SIT'    'Desc.Sit.'       'Desc.Situação'               ' '.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_set_column_name
*&---------------------------------------------------------------------*
FORM f_set_column_name USING p_column p_short p_medium p_long .

  TRY.
      column = columns->get_column( p_column ).
      column->set_short_text( p_short ).
      column->set_medium_text( p_medium ).
      column->set_long_text( p_short ).
    CATCH cx_salv_not_found.
  ENDTRY.

ENDFORM.
