*----------------------------------------------------------------------*
***INCLUDE MZFIR089_9007.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9007  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9007 OUTPUT.

  SET PF-STATUS 'PF9003'.
  SET TITLEBAR 'TL9007'.

  IF sy-ucomm IS NOT INITIAL.
    CLEAR: vg_emp1,
           vg_emp2,
           vg_emp3,
           vg_emp_desc1,
           vg_emp_desc2,
           vg_emp_desc3.
  ENDIF.

  IF vg_emp1 IS NOT INITIAL.
    SELECT SINGLE butxt
    FROM t001
    INTO vg_emp_desc1
    WHERE bukrs = vg_emp1.
  ENDIF.

  IF vg_emp2 IS NOT INITIAL.
    SELECT SINGLE butxt
    FROM t001
    INTO vg_emp_desc2
    WHERE bukrs = vg_emp2.
  ENDIF.

  IF vg_emp3 IS NOT INITIAL.
    SELECT SINGLE butxt
    FROM t001
    INTO vg_emp_desc3
    WHERE bukrs = vg_emp3.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  Z_MATHCODE_EMP2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_mathcode_emp1 INPUT.
  PERFORM f4_val_emp USING 'VG_EMP1'
                       'VG_EMP_DESC1'.
ENDMODULE.

MODULE z_mathcode_emp2 INPUT.
  PERFORM f4_val_emp USING 'VG_EMP2'
                       'VG_EMP_DESC2'.
ENDMODULE.

MODULE z_mathcode_emp3 INPUT.
  PERFORM f4_val_emp USING 'VG_EMP3'
                       'VG_EMP_DESC3'.
ENDMODULE.

FORM f4_val_emp  USING p_emp TYPE help_info-dynprofld
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
        window_title    = 'Código da Empresa'
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
*&      Module  USER_COMMAND_9007_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9007_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

MODULE z_check_code_emp1 INPUT.
  PERFORM f_check_cod_emp USING vg_emp1.
ENDMODULE.

MODULE z_check_code_emp2 INPUT.
  PERFORM f_check_cod_emp USING vg_emp2.
ENDMODULE.

MODULE z_check_code_emp3 INPUT.
  PERFORM f_check_cod_emp USING vg_emp3.
ENDMODULE.

FORM f_check_cod_emp  USING    p_vg_emp.
  CHECK p_vg_emp IS NOT INITIAL.

  SELECT SINGLE *
  FROM t001
  INTO @DATA(ls_t001)
  WHERE bukrs = @p_vg_emp.
  IF sy-subrc <> 0.
    CONCATENATE 'Empresa inválida -' p_vg_emp INTO DATA(cod_emp_invalida) SEPARATED BY space.
    MESSAGE cod_emp_invalida TYPE 'E'." DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_EMP_SALDO_APLIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_EMP1  text
*      -->P_VG_EMP_DESC1  text
*----------------------------------------------------------------------*
FORM atualiza_emp_saldo_aplic  USING    p_vg_emp
                                        p_vg_emp_desc.

  DATA: cod_emp_cadas TYPE char50.

  SELECT SINGLE * INTO @DATA(vg_cc)
    FROM zfit184
   WHERE bukrs EQ @zfit182-bukrs
     AND cod_estrutura EQ @zfit182-cod_estrutura
     AND nivel EQ @zfit182-nivel
     AND empresa   EQ @p_vg_emp.

  IF sy-subrc IS INITIAL.

    IF p_vg_emp IS NOT INITIAL.
      SELECT SINGLE butxt
        FROM t001
        INTO vg_emp_desc1
        WHERE bukrs = p_vg_emp.
      IF sy-subrc <> 0.
        CLEAR p_vg_emp_desc.
        CONCATENATE 'Empresa' p_vg_emp 'inválida' INTO DATA(cod_emp_invalida) SEPARATED BY space.
        MESSAGE cod_emp_invalida TYPE 'E'.
      ENDIF.
    ENDIF.

    CLEAR: cod_emp_cadas.
    CONCATENATE 'Cód. Empresa já cadastrado!' p_vg_emp '-' p_vg_emp_desc INTO cod_emp_cadas SEPARATED BY space.
    MESSAGE cod_emp_cadas TYPE 'E'.
  ENDIF.

  zfit184-bukrs = zfit182-bukrs.
  zfit184-cod_estrutura = zfit182-cod_estrutura.
  zfit184-item_estrutura = zfit182-item_estrutura.
  zfit184-nivel = zfit182-nivel.
  zfit184-empresa   = p_vg_emp.
  zfit184-descricao = p_vg_emp_desc.

  IF zfit184-empresa IS NOT INITIAL AND zfit184-descricao IS INITIAL.
    SELECT SINGLE butxt
      FROM t001
      INTO zfit184-descricao
      WHERE bukrs = zfit184-empresa.
    IF sy-subrc <> 0.
      CLEAR zfit184-descricao.
    ENDIF.
  ENDIF.

  insert zfit184.

  prim_dre_nivel_re = c_x.
  MOVE-CORRESPONDING zfit184 TO zfit182.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9007  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9007 INPUT.

  IF ok_code_9007 EQ c_conf.

    IF NOT ( vg_emp1 IS INITIAL ).
      PERFORM atualiza_emp_saldo_aplic USING vg_emp1 vg_emp_desc1.
    ENDIF.

    IF NOT ( vg_emp2 IS INITIAL ).
      PERFORM atualiza_emp_saldo_aplic USING vg_emp2 vg_emp_desc2.
    ENDIF.

    IF NOT ( vg_emp3 IS INITIAL ).
      PERFORM atualiza_emp_saldo_aplic USING vg_emp3 vg_emp_desc3.
    ENDIF.

    CALL METHOD g_tree->delete_all_nodes.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.
