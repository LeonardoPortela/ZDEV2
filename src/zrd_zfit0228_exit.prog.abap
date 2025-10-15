*&---------------------------------------------------------------------*
*& Report  ZRD_ZHCMT_PA_0045_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0228_exit.

FORM f_exit_ZFIT0228_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_ZFIT0228 TYPE zfit0228.

  CLEAR: wl_ZFIT0228.

  wl_ZFIT0228-dt_registro = sy-datum.
  wl_ZFIT0228-hr_registro = sy-uzeit.
  wl_ZFIT0228-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_ZFIT0228 TO p_registro_manter.

ENDFORM.

FORM f_exit_ZFIT0228_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_ZFIT0228 TYPE zfit0228.
  CLEAR: wl_ZFIT0228.

  MOVE-CORRESPONDING p_registro_manter TO wl_ZFIT0228.

  CLEAR: p_error.

  IF wl_ZFIT0228-empresa IS NOT INITIAL.
    SELECT SINGLE *
           FROM t001
           INTO @DATA(w_t001)
          WHERE bukrs = @wl_ZFIT0228-empresa.
    IF sy-subrc <> 0.
      p_error = abap_true.
      MESSAGE s024(sd) WITH 'Empresa informada está incorreta.'
                       DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_exit_ZFIT0228_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_ZFIT0228 TYPE zfit0228.

  CLEAR: wl_ZFIT0228.

  MOVE-CORRESPONDING p_registro_manter TO wl_ZFIT0228.

  wl_ZFIT0228-dt_registro = sy-datum.
  wl_ZFIT0228-hr_registro = sy-uzeit.
  wl_ZFIT0228-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_ZFIT0228 TO p_registro_manter.

ENDFORM.

FORM f_exit_ZFIT0228_0004 CHANGING p_saida TYPE any.

  DATA: wl_ZFIT0228_out TYPE ZFIT0228_out,
        lv_cnpj         TYPE j_1bwfield-cgc_number.

  CLEAR: wl_ZFIT0228_out.

  MOVE-CORRESPONDING p_saida TO wl_ZFIT0228_out.

  IF wl_ZFIT0228_out-empresa IS NOT INITIAL.
    SELECT SINGLE *
           FROM t001
           INTO @DATA(w_t001)
          WHERE bukrs = @wl_ZFIT0228_out-empresa.

    IF sy-subrc = 0.
      wl_ZFIT0228_out-nome_empresa = w_t001-butxt.
    ENDIF.

    CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
      EXPORTING
        branch            = '0001'
        bukrs             = wl_ZFIT0228_out-empresa
      IMPORTING
        cgc_number        = lv_cnpj
      EXCEPTIONS
        branch_not_found  = 1
        address_not_found = 2
        company_not_found = 3
        OTHERS            = 4.

    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_INPUT'
      EXPORTING
        input     = lv_cnpj
      IMPORTING
        output    = wl_ZFIT0228_out-cnpj
      EXCEPTIONS
        not_valid = 1
        OTHERS    = 2.


  ENDIF.

  MOVE-CORRESPONDING wl_ZFIT0228_out TO p_saida.

ENDFORM.
FORM f_exit_ZFIT0228_0005 CHANGING p_registro_manter TYPE any.


  DATA: wl_ZFIT0228_out TYPE ZFIT0228_out,
        lv_cnpj         TYPE j_1bwfield-cgc_number.
  CLEAR: wl_ZFIT0228_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_ZFIT0228_out.

  SELECT SINGLE *
         FROM t001
         INTO @DATA(w_t001)
        WHERE bukrs = @wl_ZFIT0228_out-empresa.

  IF sy-subrc = 0.
    wl_ZFIT0228_out-nome_empresa = w_t001-butxt.


    CALL FUNCTION 'J_1BREAD_BRANCH_DATA'
      EXPORTING
        branch            = '0001'
        bukrs             = wl_ZFIT0228_out-empresa
      IMPORTING
        cgc_number        = lv_cnpj
      EXCEPTIONS
        branch_not_found  = 1
        address_not_found = 2
        company_not_found = 3
        OTHERS            = 4.


    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_INPUT'
      EXPORTING
        input     = lv_cnpj
      IMPORTING
        output    = wl_ZFIT0228_out-cnpj
      EXCEPTIONS
        not_valid = 1
        OTHERS    = 2.

  ENDIF.

  MOVE-CORRESPONDING wl_ZFIT0228_out TO p_registro_manter.

ENDFORM.

FORM f_exit_ZFIT0228_0008_v2  CHANGING  p_fcat_out TYPE lvc_s_fcat.

  IF  p_fcat_out-ref_table = 'ZFIT0228_OUT' AND
     p_fcat_out-ref_field      = 'EMPRESA'.
    p_fcat_out-scrtext_l = 'Empresa'.
  ENDIF.

  IF  p_fcat_out-ref_table = 'ZFIT0228_OUT' AND
     p_fcat_out-ref_field      = 'NOME_EMPRESA'.
    p_fcat_out-scrtext_l = 'Descrição Empresa'.
    p_fcat_out-outputlen = 20.
  ENDIF.

  IF  p_fcat_out-ref_table = 'ZFIT0228_OUT' AND
     p_fcat_out-ref_field      = 'CNPJ'.
    p_fcat_out-scrtext_l = 'CNPJ'.
    p_fcat_out-outputlen = 20.
  ENDIF.

ENDFORM.

FORM  f_exit_ZFIT0228_0017 USING p_tipo.

  FIELD-SYMBOLS: <fs_dados> TYPE any.

  ASSIGN ('(ZREGISTER_DATA)<FS_WA_REGISTRO_MANTER>') TO <fs_dados>.

  IF p_tipo = '0001'.
    PERFORM f4_val_banco USING '<FS_WA_REGISTRO_MANTER>-BANCO'
          <fs_dados>.
  ELSEIF p_tipo EQ '0002'.
    PERFORM f4_val_agencia USING '<FS_WA_REGISTRO_MANTER>-AGENCIA'
       <fs_dados>.
  ELSEIF p_tipo EQ '0003'..
    PERFORM f4_val_conta USING '<FS_WA_REGISTRO_MANTER>-CONTA'
       <fs_dados>.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_VAL
*&---------------------------------------------------------------------*
FORM f4_val_conta USING p_cod TYPE help_info-dynprofld
      p_dados TYPE any.

  DATA: t_mapping TYPE STANDARD TABLE OF dselc,
        t_return  TYPE STANDARD TABLE OF ddshretval,
        s_mapping TYPE dselc.

  FIELD-SYMBOLS: <fs_razao_espec> TYPE t074-umskz.

  ASSIGN COMPONENT 'BANCO' OF STRUCTURE p_dados TO FIELD-SYMBOL(<fs_banco>).
  ASSIGN COMPONENT 'EMPRESA' OF STRUCTURE p_dados TO FIELD-SYMBOL(<fs_empresa>).
  ASSIGN COMPONENT 'AGENCIA' OF STRUCTURE p_dados TO FIELD-SYMBOL(<fs_agencia>).


  SELECT bankn
    FROM t012k
    INTO TABLE @DATA(lt_bankl)
    WHERE bukrs = @<fs_empresa>
      AND hbkid = @<fs_banco>.
  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'BANKN'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Contas'
        value_org       = 'S'
      TABLES
        value_tab       = lt_bankl
        return_tab      = t_return
        dynpfld_mapping = t_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENTER' "ENTER
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_VAL
*&---------------------------------------------------------------------*
FORM f4_val_agencia USING p_cod TYPE help_info-dynprofld
      p_dados TYPE any.

  DATA: t_mapping TYPE STANDARD TABLE OF dselc,
        t_return  TYPE STANDARD TABLE OF ddshretval,
        s_mapping TYPE dselc.

  FIELD-SYMBOLS: <fs_razao_espec> TYPE t074-umskz.

  ASSIGN COMPONENT 'EMPRESA' OF STRUCTURE p_dados TO FIELD-SYMBOL(<fs_empresa>).

  SELECT bankl
    FROM t012
    INTO TABLE @DATA(lt_bankl)
    WHERE bukrs = @<fs_empresa>.
  IF sy-subrc = 0.

    LOOP AT lt_bankl ASSIGNING FIELD-SYMBOL(<fs_bankl>).
      <fs_bankl>-bankl = <fs_bankl>-bankl+4.
    ENDLOOP.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'BANKL'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Agências'
        value_org       = 'S'
      TABLES
        value_tab       = lt_bankl
        return_tab      = t_return
        dynpfld_mapping = t_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENTER' "ENTER
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_VAL
*&---------------------------------------------------------------------*
FORM f4_val_banco USING p_cod TYPE help_info-dynprofld
      p_dados TYPE any.

  DATA: t_mapping TYPE STANDARD TABLE OF dselc,
        t_return  TYPE STANDARD TABLE OF ddshretval,
        s_mapping TYPE dselc.

  FIELD-SYMBOLS: <fs_razao_espec> TYPE t074-umskz.

  ASSIGN COMPONENT 'EMPRESA' OF STRUCTURE p_dados TO FIELD-SYMBOL(<fs_empresa>).

  SELECT bukrs,hbkid
    FROM t012
    INTO TABLE @DATA(lt_hbkid)
    WHERE bukrs = @<fs_empresa>.
  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'HBKID'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Bancos'
        value_org       = 'S'
      TABLES
        value_tab       = lt_hbkid
        return_tab      = t_return
        dynpfld_mapping = t_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENTER' "ENTER
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

ENDFORM.
