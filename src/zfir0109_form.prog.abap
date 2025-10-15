
MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.
  CREATE OBJECT lo_report.
  lo_report->get_data( ).
  lo_report->generate_output( ).
ENDMODULE.

MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
ENDMODULE.

MODULE user_command_0200 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE busca_filial.
  CLEAR: dynfields, dynfields[].

  dynfields-fieldname = 'WA_SAIDA-BUKRS'.
  APPEND dynfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-cprog
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = dynfields.
  IF sy-subrc = 0 AND dynfields-fieldvalue IS NOT INITIAL.

    LOOP AT dynfields ASSIGNING FIELD-SYMBOL(<_get_filial>).
      CASE <_get_filial>-fieldname.
        WHEN 'WA_SAIDA-BUKRS'.
          wa_saida-bukrs = <_get_filial>-fieldvalue.
      ENDCASE.
    ENDLOOP.

    SELECT DISTINCT
       'I' AS sign,
     'EQ' AS option,
     a~gsber AS low,
    a~gsber AS high
     FROM csks AS a
    WHERE a~kokrs = 'MAGI' AND a~datbi >= '20241205'
    AND a~gsber <> ''
    AND a~bukrs = @wa_saida-bukrs
    AND a~datbi >= @sy-datum
      INTO TABLE @DATA(lt_CSKS_werks_range).


    DATA: lt_return_t001w TYPE TABLE OF ddshretval,
          ls_return_t001w TYPE ddshretval,
          ls_t001w        TYPE t001w.
    SELECT DISTINCT b~werks,b~name1
  FROM t001w AS b
  WHERE b~vkorg = @wa_saida-bukrs
  AND substring( b~werks,1,1 ) BETWEEN '0' AND '9'
  AND b~name1 <> ''
      AND b~werks IN @lt_CSKS_werks_range
      ORDER BY b~werks ASCENDING
  INTO TABLE @DATA(lt_t001w).

    IF sy-subrc = 0.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'WERKS'
          dynpprog        = sy-cprog
          dynpnr          = sy-dynnr
          dynprofield     = 'WERKS'
          value_org       = 'S'
        TABLES
          value_tab       = lt_t001W
          return_tab      = lt_return_t001w
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc = 0.
        READ TABLE lt_return_t001w INTO ls_return_t001w INDEX 1.
        IF sy-subrc = 0.
          wa_saida-werks = ls_return_t001w-fieldval.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDIF.
        " ELSE.
        " MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        "         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE 'Peencha corretamente o campo empresa!' TYPE 'I'.
  ENDIF.
ENDMODULE.

MODULE busca_cc.
  CLEAR: dynfields, dynfields[].

  dynfields-fieldname = 'WA_SAIDA-BUKRS'.
  APPEND dynfields.
  dynfields-fieldname = 'WA_SAIDA-WERKS'.
  APPEND dynfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-cprog
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = dynfields.
  IF sy-subrc = 0.

    LOOP AT dynfields ASSIGNING FIELD-SYMBOL(<_get_cc>).
      CASE <_get_cc>-fieldname.
        WHEN 'WA_SAIDA-BUKRS'.
          wa_saida-bukrs = <_get_cc>-fieldvalue.
        WHEN 'WA_SAIDA-WERKS'.
          wa_saida-werks = <_get_cc>-fieldvalue.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
    IF wa_saida-bukrs IS NOT INITIAL AND wa_saida-werks IS NOT INITIAL.

      DATA: lt_return_CSKS TYPE TABLE OF ddshretval,
            ls_return_CSKS TYPE ddshretval,
            ls_CSKS        TYPE csks.

      SELECT DISTINCT
      a~kostl,a~datbi,a~bukrs,a~gsber
      FROM csks AS a
      INNER JOIN tka02 AS b ON a~bukrs = a~bukrs
      WHERE 1 = 1
      AND a~kokrs = b~kokrs
      AND a~bukrs = @wa_saida-bukrs
      AND a~gsber = @wa_saida-werks
      AND a~datbi >= @sy-datum
        ORDER BY a~bukrs,a~gsber INTO TABLE @DATA(lt_CSKS).

*      SELECT kostl,datbi,bukrs,gsber FROM csks WHERE kokrs = 'MAGI'
*        AND datbi >= @sy-datum
*        AND bukrs = @wa_saida-bukrs
*      AND gsber = @wa_saida-werks
*      ORDER BY bukrs,gsber INTO TABLE @DATA(lt_CSKS).

      IF sy-subrc = 0.
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'KOSTL'
            dynpprog        = sy-cprog
            dynpnr          = sy-dynnr
            dynprofield     = 'KOSTL'
            value_org       = 'S'
          TABLES
            value_tab       = lt_CSKS
*           FIELD_TAB       =
            return_tab      = lt_return_CSKS
*           DYNPFLD_MAPPING =
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.
        IF sy-subrc = 0.
          READ TABLE lt_return_CSKS INTO ls_return_CSKS INDEX 1.
          IF sy-subrc = 0.
            wa_saida-kostl = ls_return_CSKS-fieldval.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

          ENDIF.
          " ELSE.
          " MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          "         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ELSE.
        MESSAGE 'Não existe centro de custo para esta filial!' TYPE 'I'.
      ENDIF.
    ELSE.
      MESSAGE 'Não existe centro de custo para esta filial!' TYPE 'I'.
    ENDIF.
  ELSE.
    MESSAGE 'Peencha corretamente o campo filial!' TYPE 'I'.
  ENDIF.

ENDMODULE.

FORM action_process.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCELAR'.
      SET SCREEN 0.
      LEAVE SCREEN.
      CLEAR: wa_saida.
    WHEN 'SALVAR'.

      IF wa_saida-bukrs IS NOT INITIAL AND wa_saida-werks IS NOT INITIAL AND wa_saida-kostl IS NOT INITIAL AND wa_saida-usnam IS NOT INITIAL.

        wa_saida-userid = sy-uname.
        wa_saida-dt_entrada = sy-datum.
        wa_saida-hr_entrada = sy-uzeit.

        DATA: wa_zfit0218 TYPE zfit0218.
        CLEAR:wa_zfit0218.
        MOVE-CORRESPONDING wa_saida TO wa_zfit0218.
        MODIFY zfit0218 FROM wa_zfit0218.
        lo_report->set_refresh( ).
        SET SCREEN 0.
        LEAVE SCREEN.
        CLEAR: wa_saida,wa_zfit0218.
      ELSE.
        MESSAGE 'Favor preencher todos os Campos!' TYPE 'I'.
      ENDIF.
  ENDCASE.
ENDFORM.

MODULE busca_usuario INPUT.
  BREAK-POINT.
  CLEAR: dynfields, dynfields[].

  dynfields-fieldname = 'WA_SAIDA-USNAM'.
  APPEND dynfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-cprog
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = dynfields.
  IF sy-subrc = 0 .".

    IF dynfields-fieldvalue IS NOT INITIAL.
      READ TABLE dynfields ASSIGNING FIELD-SYMBOL(<_get_usnam>) WITH KEY fieldname = 'WA_SAIDA-USNAM'.
      IF sy-subrc = 0.
        TYPES: ty_bname TYPE RANGE OF usr21-bname.
        DATA: lr_bname TYPE RANGE OF usr21-bname.
        CLEAR: lr_bname.
        lr_bname  = VALUE ty_bname(
        LET s = 'I' o = 'EQ' IN
        sign = s option = o
        ( low = <_get_usnam>-fieldvalue )
        ).
      ENDIF.

    ENDIF.

    TYPES: BEGIN OF ty_usnam,
             bname     TYPE bname,
             name_text TYPE name_text,
           END OF ty_usnam.

    DATA: lt_return_usnam TYPE TABLE OF ddshretval,
          ls_return_usnam TYPE ddshretval,
          ls_usnam        TYPE ty_usnam.
    SELECT DISTINCT b~bname,c~name_text
  FROM usr21 AS b
      INNER JOIN v_usr_name AS c ON b~bname = c~bname
  WHERE 1 = 1
      AND b~bname IN @lr_bname
      ORDER BY b~bname ASCENDING
  INTO TABLE @DATA(lt_usnam).

    IF sy-subrc = 0.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'BNAME'
          dynpprog        = sy-cprog
          dynpnr          = sy-dynnr
          dynprofield     = 'BNAME'
          value_org       = 'S'
        TABLES
          value_tab       = lt_usnam
          return_tab      = lt_return_usnam
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc = 0.
        READ TABLE lt_return_usnam INTO ls_return_usnam INDEX 1.
        IF sy-subrc = 0.
          wa_saida-usnam = ls_return_usnam-fieldval.
          READ TABLE lt_usnam INTO wa_saida-name_text WITH KEY bname = wa_saida-usnam.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDIF.
        " ELSE.
        " MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        "         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE 'Peencha corretamente o campo Responsável!' TYPE 'I'.
  ENDIF.
ENDMODULE.
