*----------------------------------------------------------------------*
***INCLUDE MZDRE0001_9005 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9005_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9005_exit INPUT.
  CLEAR: zfit182.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_9005_EXIT  INPUT


*&---------------------------------------------------------------------*
*&      Module  STATUS_9005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9005 OUTPUT.

  SET PF-STATUS 'PF9003'.
  SET TITLEBAR 'TL9005'.

*  CLEAR: vg_nome_empresa_dest, vg_nome_estrutura_dest.
*
*  IF NOT zfit181-bukrs IS INITIAL.
*    SELECT SINGLE butxt INTO vg_nome_empresa_dest
*      FROM t001
*     WHERE bukrs EQ zfit181-bukrs.
*
*    IF NOT zfit181-cod_estrutura IS INITIAL.
*      SELECT SINGLE nome_estrutura INTO vg_nome_estrutura_dest
*        FROM zfit181
*       WHERE bukrs EQ zfit181-bukrs
*         AND cod_estrutura EQ zfit182-cod_estrutura.
*    ENDIF.
*  ENDIF.

ENDMODULE.                 " STATUS_9005  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9005 INPUT.

  DATA: it_copia_02 TYPE TABLE OF zfit182 WITH HEADER LINE,
        it_copia_03 TYPE TABLE OF zfit183 WITH HEADER LINE,
        it_copia_04 TYPE TABLE OF zfit184 WITH HEADER LINE.

  IF ok_code_9005 EQ c_conf.

    IF ( wa_zfit181_alv-cod_estrutura EQ zfit182-cod_estrutura ) .
      MESSAGE 'Deve ser informado Estrutura diferentes para cópia!' TYPE 'E'.
    ENDIF.

    SELECT SINGLE bukrs
      FROM zfit181
      INTO wa_zfit181_alv-bukrs
      WHERE cod_estrutura = zfit182-cod_estrutura.

    IF sy-subrc = 0.

      SELECT * INTO TABLE it_copia_02
        FROM zfit182
       WHERE cod_estrutura EQ wa_zfit181_alv-cod_estrutura.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE 'Estrutura não possui níveis para cópia!' TYPE 'E'.
      ENDIF.

      SELECT * INTO TABLE it_copia_03
        FROM zfit183
       WHERE cod_estrutura EQ wa_zfit181_alv-cod_estrutura.

      SELECT * INTO TABLE it_copia_04
        FROM zfit184
       WHERE cod_estrutura EQ wa_zfit181_alv-cod_estrutura.

      LOOP AT it_copia_02.
        it_copia_02-bukrs         = wa_zfit181_alv-bukrs.
        it_copia_02-cod_estrutura = zfit182-cod_estrutura.
        MODIFY it_copia_02 INDEX sy-tabix TRANSPORTING bukrs cod_estrutura.
      ENDLOOP.

      LOOP AT it_copia_03.
        it_copia_03-bukrs         = wa_zfit181_alv-bukrs.
        it_copia_03-cod_estrutura = zfit182-cod_estrutura.
        MODIFY it_copia_03 INDEX sy-tabix TRANSPORTING bukrs cod_estrutura.
      ENDLOOP.

      LOOP AT it_copia_04.
        it_copia_04-bukrs         = wa_zfit181_alv-bukrs.
        it_copia_04-cod_estrutura = zfit182-cod_estrutura.
        MODIFY it_copia_04 INDEX sy-tabix TRANSPORTING bukrs cod_estrutura.
      ENDLOOP.

      CLEAR: zfit182.
      IF NOT it_copia_02[] IS INITIAL.
        MODIFY zfit182 FROM TABLE it_copia_02.
      ENDIF.
      IF NOT it_copia_03[] IS INITIAL.
        INSERT zfit183 FROM TABLE it_copia_03.
      ENDIF.
      IF NOT it_copia_04[] IS INITIAL.
        INSERT zfit184 FROM TABLE it_copia_04.
      ENDIF.

      MESSAGE 'Cópia efetuada!' TYPE 'S'.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_9005  INPUT
*&---------------------------------------------------------------------*
*&      Module  Z_MATHCODE_EMP_9005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_mathcode_emp_9005 INPUT.
  PERFORM f4_val_emp_9005 USING 'VG_EMP1'
                                'VG_EMP_DESC1'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F4_VAL_EMP_9005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0259   text
*      -->P_0260   text
*----------------------------------------------------------------------*
FORM f4_val_emp_9005  USING    p_emp TYPE help_info-dynprofld
                      p_desc TYPE help_info-dynprofld.

*====>  Tabelas internas
  DATA: BEGIN OF t_emp OCCURS 0,
          bukrs TYPE t001-bukrs,
          butxt TYPE t001-butxt,
        END OF t_emp.

  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  SELECT  bukrs butxt
    FROM  t001 INTO TABLE t_emp
    WHERE bukrs <> space.

  IF sy-subrc = 0.

    s_mapping-fldname     = 'F0001'.
    s_mapping-dyfldname   = p_emp.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'F0002'.
    s_mapping-dyfldname   = p_desc.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'BUKRS'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_emp
        window_title    = 'Empresa - Saldo da aplicação'
        value_org       = 'S'
      TABLES
        value_tab       = t_emp
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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  Z_MATHCODE_SEQ1_9005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_mathcode_cod_estrutura_9005 INPUT.
  PERFORM f4_val_9005 USING 'ZFIT182-COD_ESTRUTURA'
                            'VG_NOME_ESTRUTURA'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F4_VAL_9005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0418   text
*      -->P_0419   text
*----------------------------------------------------------------------*
FORM f4_val_9005  USING     p_cod TYPE help_info-dynprofld
                            p_desc TYPE help_info-dynprofld.

*====>  Tabelas internas
  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  SELECT cod_estrutura, nome_estrutura
   FROM zfit181 INTO TABLE @DATA(t_estrutura)
   WHERE cod_estrutura <> @wa_zfit181_alv-cod_estrutura.

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
        retfield        = 'COD_ESTRUTURA'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = p_cod
        window_title    = 'Código de fluxo de caixa'
        value_org       = 'S'
      TABLES
        value_tab       = t_estrutura
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

ENDFORM.
