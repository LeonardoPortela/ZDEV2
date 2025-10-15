*&---------------------------------------------------------------------*
*& Report  ZRD_ZPMT0076_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zpmt0076_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zpmt0076_0001 USING p_registro_manter TYPE any.


  DATA: wl_zpmt0076 TYPE zpmt0076.
  DATA(c_b) = 'B'.

  CLEAR: wl_zpmt0076.

  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0076.

  wl_zpmt0076-katalogart = c_b.

  MOVE-CORRESPONDING wl_zpmt0076 TO p_registro_manter.

ENDFORM.

FORM f_exit_zpmt0076_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zpmt0076 TYPE zpmt0076.
  DATA(c_ibau) = 'IBAU'.
  DATA(c_b) = 'B'.
  DATA(c_f) = 'F'.
  DATA(c_h) = 'H'.

  CLEAR: wl_zpmt0076.

  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0076.

  IF wl_zpmt0076-conjunto IS INITIAL OR
     wl_zpmt0076-codegruppe IS INITIAL OR
     wl_zpmt0076-code IS INITIAL OR
     wl_zpmt0076-mptyp IS INITIAL.

    p_error = abap_true.
    MESSAGE i024(sd) WITH 'Todos os campos são obrigatórios.'.
    EXIT.
  ENDIF.

  IF wl_zpmt0076-conjunto IS NOT INITIAL.
    SELECT SINGLE matnr FROM mara
      INTO @DATA(dummy1)
      WHERE matnr = @wl_zpmt0076-conjunto
        AND mtart = @c_ibau.
    IF sy-subrc <> 0.
      p_error = abap_true.
      MESSAGE i024(sd) WITH 'Código do conjunto inválido.'.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_zpmt0076-codegruppe IS NOT INITIAL.
    SELECT SINGLE codegruppe
      FROM qpgt
      INTO @DATA(dummy2)
      WHERE katalogart = @c_b
        AND sprache = @sy-langu
        AND codegruppe = @wl_zpmt0076-codegruppe.

    IF sy-subrc <> 0.
      p_error = abap_true.
      MESSAGE i024(sd) WITH 'Sistema inválido.'.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_zpmt0076-code IS NOT INITIAL.
    SELECT code
      FROM qpct
      INTO TABLE @DATA(dummy3)
      WHERE katalogart = @c_b
        AND code = @wl_zpmt0076-code
        AND codegruppe = @wl_zpmt0076-codegruppe
        AND sprache = @sy-langu.
    IF sy-subrc <> 0.
      p_error = abap_true.
      MESSAGE i024(sd) WITH 'Subsistema inválido.'.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_zpmt0076-mptyp IS NOT INITIAL.
    IF wl_zpmt0076-mptyp <> c_f AND wl_zpmt0076-mptyp <> c_h .
      p_error = abap_true.
      MESSAGE i024(sd) WITH 'Categoria do ponto de medição inválida.'.
      EXIT.

    ELSE.
      SELECT SINGLE mptyp
        FROM zpmt0074
        INTO @DATA(dummy4)
        WHERE locas = @wl_zpmt0076-conjunto
         AND mptyp = @wl_zpmt0076-mptyp.
      IF sy-subrc <> 0.
        p_error = abap_true.
        MESSAGE i024(sd) WITH 'Verificar parametros obrigatorios (ZPM0093)!'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.


FORM f_exit_zpmt0076_0005 CHANGING p_registro_manter TYPE any.

  DATA: wa_0076 TYPE zpmt0076_out.

  CLEAR: wa_0076.

  MOVE-CORRESPONDING p_registro_manter TO wa_0076.

  EXPORT registro_manter FROM wa_0076 TO MEMORY ID 'MEMO_REGISTRO_MANTER'. "Import no form f4_val_codegruppe

ENDFORM.

FORM  f_exit_zpmt0076_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

  IF p_db_tab = 'ZPMT0076'.
    APPEND 'Modificar'    TO it_excl_toolbar.
  ENDIF.
ENDFORM.

FORM f_exit_zpmt0076_0016 USING p_ucomm              TYPE sy-ucomm
                          CHANGING p_registro_manter TYPE any
                                   p_saida           TYPE any.

  DATA: wa_0076 TYPE zpmt0076_out.

  CLEAR: wa_0076.

  MOVE-CORRESPONDING p_registro_manter TO wa_0076.

  EXPORT registro_manter FROM wa_0076 TO MEMORY ID 'MEMO_REGISTRO_MANTER'. "Import no form f4_val_codegruppe
*aqui
  IF wa_0076-codegruppe IS NOT INITIAL.

    SELECT SINGLE kurztext
      INTO wa_0076-desc_sistema
      FROM qpgt
      WHERE codegruppe = wa_0076-codegruppe
        AND sprache    = sy-langu.

  ENDIF.

  IF wa_0076-code IS NOT INITIAL.
    SELECT SINGLE kurztext
      INTO wa_0076-desc_subsistema
      FROM qpct
      WHERE codegruppe = wa_0076-codegruppe
        AND code       = wa_0076-code.
  ENDIF.

  MOVE-CORRESPONDING wa_0076 TO p_saida.
  MOVE-CORRESPONDING wa_0076 TO p_registro_manter.



ENDFORM.


FORM f_exit_zpmt0076_0017 USING p_tipo.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENTER' "ENTER
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

  IF p_tipo = '0001'.
    PERFORM f4_val_conjunto USING '<FS_WA_REGISTRO_MANTER>-CONJUNTO'.
  ENDIF.

  IF p_tipo = '0002'.
    PERFORM f4_val_codegruppe USING '<FS_WA_REGISTRO_MANTER>-CODEGRUPPE'
                                    '<FS_WA_REGISTRO_MANTER>-DESC_SISTEMA'.
  ENDIF.

  IF p_tipo = '0003'.
    PERFORM f4_val_code USING '<FS_WA_REGISTRO_MANTER>-CODE'
                              '<FS_WA_REGISTRO_MANTER>-DESC_SUBSISTEMA'.
  ENDIF.

  IF p_tipo = '0004'.
    PERFORM f4_val_mptyp USING '<FS_WA_REGISTRO_MANTER>-MPTYP'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_VAL_KATALOGART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0014   text
*      -->P_0015   text
*----------------------------------------------------------------------*
FORM f4_val_conjunto  USING p_cod TYPE help_info-dynprofld.
*                              p_desc TYPE help_info-dynprofld.

  CLEAR t_return.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  DATA(c_ibau) = 'IBAU'.

  SELECT a~matnr, b~maktx
    INTO TABLE @DATA(lt_conjunto)
    FROM mara AS a INNER JOIN makt AS b
    ON a~matnr = b~matnr
    AND b~spras = @sy-langu
    WHERE mtart = @c_ibau.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'MATNR'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Catálogo'
        value_org       = 'S'
      TABLES
        value_tab       = lt_conjunto
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

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = '=ENTER' "ENTER
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_VAL_KATALOGART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0014   text
*      -->P_0015   text
*----------------------------------------------------------------------*
FORM f4_val_katalogart  USING p_cod TYPE help_info-dynprofld.
*                              p_desc TYPE help_info-dynprofld.

  CLEAR t_return.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.
  DATA(c_b) = 'B'.


  SELECT katalogart, katalogtxt
    FROM tq15t
    INTO TABLE @DATA(lt_tq15t)
    WHERE katalogart = @c_b
      AND sprache = @sy-langu.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'KATALOGART'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Catálogo'
        value_org       = 'S'
      TABLES
        value_tab       = lt_tq15t
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

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = '=ENTER' "ENTER
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_VAL_CODEGRUPPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0030   text
*----------------------------------------------------------------------*
FORM f4_val_codegruppe  USING p_cod TYPE help_info-dynprofld
                              p_desc TYPE help_info-dynprofld.

  CLEAR t_return.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.
  DATA(c_b) = 'B'.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.
  DATA: wa_registro_manter TYPE zpmt0076_out.

  IMPORT registro_manter TO wa_registro_manter FROM MEMORY ID 'MEMO_REGISTRO_MANTER'. "Export no form f_exit_zpmt0076_0016

  SELECT codegruppe, kurztext
    FROM qpgt
    INTO TABLE @DATA(lt_qpgt)
    WHERE katalogart = @c_b
      AND sprache = @sy-langu.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_desc.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CODEGRUPPE'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Grupo de codes'
        value_org       = 'S'
      TABLES
        value_tab       = lt_qpgt
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

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = '=ENTER' "ENTER
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_VAL_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0071   text
*----------------------------------------------------------------------*
FORM f4_val_code  USING p_cod TYPE help_info-dynprofld
                        p_desc TYPE help_info-dynprofld.

  CLEAR t_return.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.
  DATA(c_b) = 'B'.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.
  DATA: wa_registro_manter TYPE zpmt0076_out.

  IMPORT registro_manter TO wa_registro_manter FROM MEMORY ID 'MEMO_REGISTRO_MANTER'. "Export no form f_exit_zpmt0076_0016

  SELECT code, codegruppe, kurztext
    FROM qpct
    INTO TABLE @DATA(lt_qpct)
    WHERE katalogart = @c_b
      AND codegruppe = @wa_registro_manter-codegruppe
      AND sprache = @sy-langu.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'F0003'.
    s_mapping-dyfldname   = p_desc.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CODE'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Código'
        value_org       = 'S'
      TABLES
        value_tab       = lt_qpct
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

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = '=ENTER' "ENTER
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_VAL_MPTYP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0080   text
*----------------------------------------------------------------------*
FORM f4_val_mptyp  USING p_cod TYPE help_info-dynprofld.
*                              p_desc TYPE help_info-dynprofld.

  CLEAR t_return.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.


  SELECT mptyp, mpttx
    FROM t370p_t
    INTO TABLE @DATA(t370p_t)
    WHERE mptyp IN ( 'F', 'H' )
      AND spras = @sy-langu.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_cod.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'MPTYP'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Categoria do ponto de medição'
        value_org       = 'S'
      TABLES
        value_tab       = t370p_t
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

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = '=ENTER' "ENTER
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.

  ENDIF.

ENDFORM.
