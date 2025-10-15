*&---------------------------------------------------------------------*
*& Report  ZRD_zlest0037_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zlest0037_exit.

FORM f_exit_zlest0037_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0037 TYPE zlest0037.

  CLEAR: wl_zlest0037.

  MOVE-CORRESPONDING p_registro_manter TO wl_zlest0037.

  wl_zlest0037-zdt_atual = sy-datum.
  wl_zlest0037-zhr_atual = sy-uzeit.
  wl_zlest0037-usuario   = sy-uname.

  MOVE-CORRESPONDING wl_zlest0037 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0037_0002 USING p_registro_manter TYPE zlest0037
                        CHANGING p_error.

  DATA: g_domain TYPE dd07l-domname,
        g_value  TYPE dd07l-domvalue_l,
        g_subrc  TYPE sy-subrc,
        gwa_tab  TYPE dd07v.

  DATA: wl_zlest0037 TYPE zlest0037.

  CLEAR: wl_zlest0037.

  MOVE-CORRESPONDING p_registro_manter TO wl_zlest0037.

  wl_zlest0037-zdt_atual = sy-datum.
  wl_zlest0037-zhr_atual = sy-uzeit.
  wl_zlest0037-usuario   = sy-uname.

  IF wl_zlest0037-cd_modal <> '03'.
    CLEAR wl_zlest0037-operacao.
  ENDIF.

  IF p_registro_manter-matnr IS INITIAL.
    MESSAGE 'Campo Material obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  IF p_registro_manter-bukrs IS INITIAL.
    MESSAGE 'Campo Empresa obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

  IF p_registro_manter-cd_modal IS INITIAL.
    MESSAGE 'Campo CD Modal obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
    EXIT.
  ENDIF.

" BUG 181995 -  Retirar Obrigatóriedade - BG - INICIO
*  IF p_registro_manter-cd_modal = '03' AND p_registro_manter-operacao IS INITIAL.
*    MESSAGE 'Campo Operação obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
*    p_error = 'X'.
*    EXIT.
*  ENDIF.
" BUG 181995 -  Retirar Obrigatóriedade - BG - FIM

  IF p_registro_manter-bukrs IS NOT INITIAL.
    SELECT SINGLE bukrs
      INTO p_registro_manter-bukrs
      FROM t001
     WHERE bukrs = p_registro_manter-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa não encontrada!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_registro_manter-matnr   IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = p_registro_manter-matnr
      IMPORTING
        output = p_registro_manter-matnr.

    SELECT SINGLE matnr
      INTO wl_zlest0037-matnr
      FROM makt
     WHERE matnr = p_registro_manter-matnr.
    IF sy-subrc NE 0.
      MESSAGE 'Material não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_registro_manter-lifnr   IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_registro_manter-lifnr
      IMPORTING
        output = p_registro_manter-lifnr.

    SELECT SINGLE lifnr
      INTO wl_zlest0037-lifnr
      FROM lfa1
     WHERE lifnr = p_registro_manter-lifnr.
    IF sy-subrc NE 0.
      MESSAGE 'Fornecedor não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_registro_manter-matkl  IS NOT INITIAL.
    SELECT SINGLE matkl
      INTO p_registro_manter-matkl
      FROM t023
     WHERE matkl = p_registro_manter-matkl.
    IF sy-subrc NE 0.
      MESSAGE 'Grupo Mercadoria não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_registro_manter-cd_modal IS NOT INITIAL.
    g_domain = 'ZMODAL'.
    g_value  = p_registro_manter-cd_modal.

    CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
      EXPORTING
        domname  = g_domain
        value    = g_value
      IMPORTING
        dd07v_wa = gwa_tab
        rc       = g_subrc.
    IF g_subrc NE 0.
      MESSAGE 'CD Modal não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_registro_manter-operacao IS NOT INITIAL.
    g_domain = 'ZDM_OPER_AQUAV'.
    g_value  = p_registro_manter-operacao.

    CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
      EXPORTING
        domname  = g_domain
        value    = g_value
      IMPORTING
        dd07v_wa = gwa_tab
        rc       = g_subrc.
    IF g_subrc NE 0.
      MESSAGE 'Cod EUDR Incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING wl_zlest0037 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0037_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0037 TYPE zlest0037.

  CLEAR: wl_zlest0037.

  MOVE-CORRESPONDING p_registro_manter TO wl_zlest0037.

  wl_zlest0037-zdt_atual = sy-datum.
  wl_zlest0037-zhr_atual = sy-uzeit.
  wl_zlest0037-usuario   = sy-uname.

  MOVE-CORRESPONDING wl_zlest0037 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0037_0004 CHANGING p_saida     TYPE zlest0037_out.

  DATA: lc_saida TYPE zlest0037_out,
        g_domain TYPE dd07l-domname,
        g_value  TYPE dd07l-domvalue_l,
        g_subrc  TYPE sy-subrc,
        gwa_tab  TYPE dd07v.

  MOVE-CORRESPONDING p_saida TO lc_saida.

  CLEAR: lc_saida-descr_operacao, lc_saida-descr_modal.

  g_domain = 'ZMODAL'.
  g_value  = lc_saida-cd_modal.

  CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
    EXPORTING
      domname  = g_domain
      value    = g_value
    IMPORTING
      dd07v_wa = gwa_tab
      rc       = g_subrc.
  IF g_subrc =  0.
    lc_saida-descr_modal = gwa_tab-ddtext.
  ENDIF.

  g_domain = 'ZDM_OPER_AQUAV'.
  g_value  = lc_saida-operacao.

  CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
    EXPORTING
      domname  = g_domain
      value    = g_value
    IMPORTING
      dd07v_wa = gwa_tab
      rc       = g_subrc.
  IF g_subrc =  0.
    lc_saida-descr_operacao = gwa_tab-ddtext.
  ENDIF.

  MOVE-CORRESPONDING lc_saida TO p_saida.

ENDFORM.

FORM f_exit_zlest0037_0005 CHANGING p_registro_manter TYPE zlest0037.

  IF p_registro_manter-matnr IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = p_registro_manter-matnr
      IMPORTING
        output = p_registro_manter-matnr.
  ENDIF.

  IF p_registro_manter-lifnr IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_registro_manter-lifnr
      IMPORTING
        output = p_registro_manter-lifnr.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name = '<FS_WA_REGISTRO_MANTER>-OPERACAO'.
      IF p_registro_manter-cd_modal = '03'.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM f_exit_zlest0037_0006 USING p_saida TYPE zlest0037_out
                           CHANGING p_error.

ENDFORM.

*-Proj.EUDR-10.10.2024-#154904-JT-inicio
FORM f_exit_zlest0037_0017 USING p_tipo.

  CASE p_tipo.
    WHEN '0001'.
      PERFORM f4_val_param_cd_modal USING '<FS_WA_REGISTRO_MANTER>-CD_MODAL'.
    WHEN '0002'.
      PERFORM f4_val_param_operacao USING '<FS_WA_REGISTRO_MANTER>-OPERACAO'.
  ENDCASE.

ENDFORM.

FORM f4_val_param_cd_modal USING p_cod  TYPE help_info-dynprofld.

  TYPES: BEGIN OF ty_dados,
           cd_modal TYPE zmodal,
           descr    TYPE char60,
         END OF ty_dados.

  DATA: t_dd07v   TYPE TABLE OF dd07v,
        t_dados   TYPE TABLE OF ty_dados,
        t_mapping TYPE STANDARD TABLE OF dselc,
        s_mapping TYPE dselc,
        t_ret     TYPE TABLE OF ddshretval,
        v_rc      TYPE sy-subrc.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = 'ZMODAL'
      text      = 'X'
      langu     = sy-langu
*     BYPASS_BUFFER        = ' '
    IMPORTING
      rc        = v_rc
    TABLES
      dd07v_tab = t_dd07v.

  LOOP AT t_dd07v INTO DATA(ls_dd07v).
    APPEND VALUE #( cd_modal = ls_dd07v-domvalue_l descr = ls_dd07v-ddtext ) TO t_dados.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CD_MODAL'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Modal'
      value_org       = 'S'
    TABLES
      value_tab       = t_dados
      return_tab      = t_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.

FORM f4_val_param_operacao USING p_cod  TYPE help_info-dynprofld.

  TYPES: BEGIN OF ty_dados,
           operacao TYPE zde_oper_aquav,
           descr    TYPE char60,
         END OF ty_dados.

  DATA: t_dd07v   TYPE TABLE OF dd07v,
        t_dados   TYPE TABLE OF ty_dados,
        t_mapping TYPE STANDARD TABLE OF dselc,
        s_mapping TYPE dselc,
        t_ret     TYPE TABLE OF ddshretval,
        v_rc      TYPE sy-subrc.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = 'ZDM_OPER_AQUAV'
      text      = 'X'
      langu     = sy-langu
*     BYPASS_BUFFER        = ' '
    IMPORTING
      rc        = v_rc
    TABLES
      dd07v_tab = t_dd07v.

  LOOP AT t_dd07v INTO DATA(ls_dd07v).
    APPEND VALUE #( operacao = ls_dd07v-domvalue_l descr = ls_dd07v-ddtext ) TO t_dados.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OPERACAO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Operação'
      value_org       = 'S'
    TABLES
      value_tab       = t_dados
      return_tab      = t_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.

FORM f_exit_zlest0037_0008 CHANGING p_col_pos
                                    p_ref_tabname
                                    p_ref_fieldname
                                    p_tabname
                                    p_field
                                    p_scrtext_l
                                    p_outputlen
                                    p_edit
                                    p_sum
                                    p_emphasize
                                    p_just
                                    p_hotspot
                                    p_f4
                                    p_check.

  IF p_ref_tabname = 'ZLEST0037_OUT' AND
     p_field       = 'MATNR'.
    p_outputlen    = 18.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0037_OUT' AND
     p_field       = 'CK_SERVICO'.
    p_check        = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0037_OUT' AND
     p_field       = 'DESCR_MODAL'.
    p_scrtext_l    = 'Descrição Modal'.
    p_outputlen    = 25.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0037_OUT' AND
     p_field       = 'DESCR_OPERACAO'.
    p_scrtext_l    = 'Descrição Operacao'.
    p_outputlen    = 25.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0037_OUT' AND
     p_field       = 'USUARIO'.
    p_scrtext_l    = 'Usuário'.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0037_OUT' AND
     p_field       = 'ZDT_ATUAL'.
    p_scrtext_l    = 'Data'.
    p_outputlen    = 12.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0037_OUT' AND
     p_field       = 'ZHR_ATUAL'.
    p_scrtext_l    = 'Hora'.
    p_outputlen    = 12.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0037_0016 USING p_ucomm           TYPE sy-ucomm
                        CHANGING p_registro_manter TYPE any
                                 p_saida           TYPE any.

  DATA: w_zlest0037      TYPE zlest0037.

  CLEAR: w_zlest0037.
*
  MOVE-CORRESPONDING p_registro_manter TO w_zlest0037.

  IF w_zlest0037-cd_modal <> '03'.
    CLEAR w_zlest0037-operacao.
  ENDIF.

  MOVE-CORRESPONDING w_zlest0037       TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0037_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZLEST0037'
      tabfirst = 'X'.

ENDFORM.
