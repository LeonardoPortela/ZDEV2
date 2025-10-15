*&---------------------------------------------------------------------*
*& Include          ZFIS46_PARAMS_MODULE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100'.
  CREATE OBJECT lo_report.
  lo_report->get_data( ).
  lo_report->generate_output( ).
ENDMODULE.

MODULE status_0200 OUTPUT.
SET PF-STATUS 'STATUS_0200'.
ENDMODULE.

MODULE user_command_0200 INPUT.
  TYPES: BEGIN OF ty_list01,
           domvalue_l TYPE domvalue_l,
           ddtext     TYPE ddtext,
         END OF ty_list01.

  DATA: it_conc_invest TYPE TABLE OF ty_list01,
        lt_return      TYPE TABLE OF ddshretval,
        ls_return      TYPE ddshretval.

  CASE sy-ucomm.
    WHEN 'FB_BUSCAR'.
      CALL METHOD lcl_f4=>f4_custon_001.
    WHEN 'FB_CANCELAR'.
      CLEAR:lt_return,ls_return,wa_saida.
      FREE: it_conc_invest,lt_return.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'FB_SALVAR'.
      MODIFY ZMMT0186 FROM wa_saida.
      IF sy-subrc = 0 .
        CALL METHOD lcl_report=>set_refresh CHANGING co_alv = o_alv.
        SET SCREEN 0.
        LEAVE SCREEN.
      ENDIF.
      CLEAR: lt_return,ls_return,wa_saida.
      FREE: it_conc_invest,lt_return.
    WHEN 'FB_AGRUPAMENTO'.

*      IF wa_saida-moeda_funcional IS NOT INITIAL AND wa_saida-pais IS NOT INITIAL.
*
*        SELECT
*          'I' AS sign,
*          'EQ' AS option,
*          agrupamento AS low,
*          agrupamento AS high
*          FROM ZMMT0186 WHERE moeda_funcional = @wa_saida-moeda_funcional AND pais = @wa_saida-pais INTO TABLE @DATA(it_agrupamento_filter).
*

*        IF wa_saida-moeda_funcional = 'BRL' AND wa_saida-pais = 'BR'.
*
*          SELECT
*            domvalue_l,
*            ddtext
*            FROM dd07v
*            WHERE domname = 'ZGL083_CONC_INSVEST_BRL'
*            INTO TABLE @it_conc_invest.
*
*        ELSEIF wa_saida-moeda_funcional = 'USD' AND wa_saida-pais = 'BR'.
*          SELECT             domvalue_l,
*            ddtext
*  FROM dd07v
*  WHERE domname = 'ZGL083_CONC_INSVEST_USD'
*  INTO TABLE @it_conc_invest.
*
*        ELSE.
*          SELECT
*            domvalue_l,
*            ddtext
*FROM dd07v
*WHERE domname = 'ZGL083_CONC_INSVEST_USD_INTER'
*INTO TABLE @it_conc_invest.
*
*        ENDIF.
*
*        IF it_agrupamento_filter IS NOT INITIAL.
*          DELETE it_conc_invest WHERE ddtext IN it_agrupamento_filter.
*        ENDIF.
*
*        IF it_conc_invest IS NOT INITIAL.
*
*          sort it_conc_invest by domvalue_l ASCENDING.
*
*          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*            EXPORTING
*              retfield        = 'DOMVALUE_L'
*              dynpprog        = sy-repid
*              dynpnr          = sy-dynnr
*              value_org       = 'S'
*            TABLES
*              value_tab       = it_conc_invest
*              return_tab      = lt_return
*            EXCEPTIONS
*              parameter_error = 1
*              no_values_found = 2
*              OTHERS          = 3.
*
*          IF sy-subrc = 0.
*            READ TABLE lt_return INTO ls_return INDEX 1.
*            IF sy-subrc = 0.
*              READ TABLE it_conc_invest WITH KEY domvalue_l = ls_return-fieldval ASSIGNING FIELD-SYMBOL(<_get_name>).
*              wa_saida-sequencia = <_get_name>-domvalue_l.
*              wa_saida-agrupamento = <_get_name>-ddtext.
*            ENDIF.
*          ELSE.
*
*          ENDIF.
*
*        ELSE.
*          MESSAGE 'Verifique os campos Investidora / Investida e Moeda se estão preenchidos ou se já atingiram o limite de Agrupamento!' TYPE 'I'.
*          "CALL METHOD lcl_f4=>f4_custon_001.
*        ENDIF.
*      ENDIF.

    WHEN 'FB_AGRUPAMENTO_CLEAR'.

      "CLEAR: wa_saida-agrupamento.

    WHEN 'FB_CONTA_CLEAR'.

      "CLEAR: wa_saida-conta.

    WHEN 'FB_CONTA'.

*      IF wa_saida-conta IS NOT INITIAL.
*
*        TYPES: BEGIN OF ty_contas,
*                 saknr TYPE ska1-saknr,
*                 txt50 TYPE skat-txt50,
*               END OF ty_contas.
*
*        DATA: it_ska1 TYPE STANDARD TABLE OF ty_contas.
*        DATA: lr_contas TYPE RANGE OF ska1-saknr WITH HEADER LINE.
*        DATA: _saknr(10) TYPE c.
*
*        FREE: it_ska1,lr_contas.
*        CLEAR: lr_contas,_saknr.
*
*        SPLIT wa_saida-conta AT ',' INTO TABLE DATA(_contas).
*
*        LOOP AT _contas ASSIGNING FIELD-SYMBOL(<contas>).
*          CLEAR: _saknr.
*          UNPACK <contas> TO _saknr.
*          lr_contas-option = 'EQ'.
*          lr_contas-sign = 'I'.
*          lr_contas-low = _saknr.
*          APPEND lr_contas TO lr_contas[].
*          CLEAR: _saknr.
*        ENDLOOP.
*
*        SELECT DISTINCT a~saknr,b~txt50
*          FROM
*          ska1 AS a
*           INNER JOIN skat AS b ON a~saknr = b~saknR
*        WHERE b~spras = 'P'
*          AND b~ktopl = '0050'
*          AND a~saknr NOT IN @lr_contas[]
*          INTO TABLE @it_ska1.
*
*      ELSE.
*        SELECT DISTINCT a~saknr,b~txt50
*          FROM
*          ska1 AS a
*           INNER JOIN skat AS b ON a~saknr = b~saknR
*        WHERE b~spras = 'P'
*          AND b~ktopl = '0050'
*          INTO TABLE @it_ska1.
*      ENDIF.
*
*      SORT it_ska1 BY saknr ASCENDING.
*
*      IF it_ska1 IS NOT INITIAL.
*
*        LOOP AT it_ska1 ASSIGNING FIELD-SYMBOL(<_ajsut>).
*          UNPACK <_ajsut>-saknr TO <_ajsut>-saknr.
*        ENDLOOP.
*
*        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*          EXPORTING
*            retfield        = 'SAKNR'
*            dynpprog        = sy-repid
*            dynpnr          = sy-dynnr
*            value_org       = 'S'
*          TABLES
*            value_tab       = it_ska1
*            return_tab      = lt_return
*          EXCEPTIONS
*            parameter_error = 1
*            no_values_found = 2
*            OTHERS          = 3.
*
*        IF sy-subrc = 0.
*          READ TABLE lt_return INTO ls_return INDEX 1.
*          IF sy-subrc = 0.
*            "CLEAR: _saknr.
*            "UNPACK ls_return-fieldval TO _saknr.
*            IF wa_saida-conta IS NOT INITIAL.
*              CONCATENATE wa_saida-conta ls_return-fieldval INTO wa_saida-conta SEPARATED BY ','.
*            ELSE.
*              wa_saida-conta = ls_return-fieldval.
*            ENDIF.
*            "CLEAR: _saknr.
*          ENDIF.
*        ELSE.
*          "MESSAGE 'Verifique os campos Investidora / Investida e Moeda se estão preenchidos!' TYPE 'I'.
*        ENDIF.
*      ENDIF.

    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      CLEAR:lt_return,ls_return,wa_saida.
      FREE: it_conc_invest,lt_return.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      CLEAR:lt_return,ls_return,wa_saida.
      FREE: it_conc_invest,lt_return.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.



MODULE user_command_0100 INPUT.
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
  ENDCASE.
ENDMODULE.
