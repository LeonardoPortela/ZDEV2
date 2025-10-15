
TABLES: zfit0232.

CLASS:lcl_report_100 DEFINITION DEFERRED,
  lcl_report_200 DEFINITION DEFERRED.

DATA: lo_report_100 TYPE REF TO lcl_report_100,
      lo_report_200 TYPE REF TO lcl_report_200.

DATA: it_screen_status TYPE TABLE OF sy-ucomm.

TYPES: BEGIN OF ty_param_hkont_ativo,
         hkont_ativo TYPE zfit0231-hkont_ativo,
       END OF ty_param_hkont_ativo.

DATA: lt_param_hkont_ativo TYPE TABLE OF ty_param_hkont_ativo,
      p_dtini              TYPE sy-datum,
      p_dtfim              TYPE sy-datum,
      p_dtsub              TYPE sy-datum,
      p_ukurs              TYPE p DECIMALS 4,
      p_create             TYPE bool.

    " Batch input nova estrutura do campo de tabela
    TYPES: BEGIN OF ty_bdcdata,
             program  TYPE bdcdata-program,  " Pool de módulos BDC
             dynpro   TYPE bdcdata-dynpro,   " NÚmero de tela BDC
             dynbegin TYPE bdcdata-dynbegin, " Início BDC de uma tela
             fnam     TYPE bdcdata-fnam,     " Nome do campo
             fval     TYPE bdcdata-fval,     " Valor do campo BDC
           END OF ty_bdcdata.

    " Tabelas Internas ....
    DATA: it_bdcdata TYPE TABLE OF ty_bdcdata.

    " Estruturas ...
    DATA: st_bdcdata TYPE ty_bdcdata.

DATA t_rows               TYPE lvc_t_row.

DATA it_sap TYPE TABLE OF zfit0231.

DATA: vg_edit          TYPE int4.    "<<<------"189660 - NMS ------->>>

CONSTANTS: gv_status1000 TYPE sy-pfkey VALUE 'STATUS_1000'.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-c01 .
  SELECT-OPTIONS:
  p_bukrs FOR zfit0232-bukrs  NO-EXTENSION NO INTERVALS," OBLIGATORY,
  p_gjahr   FOR zfit0232-ano  NO-EXTENSION NO INTERVALS MATCHCODE OBJECT zyear," OBLIGATORY,
  p_monat   FOR zfit0232-mes NO-EXTENSION NO INTERVALS MATCHCODE OBJECT zshfi_mes," OBLIGATORY,
  p_paber   FOR zfit0232-abertura NO-EXTENSION NO INTERVALS," OBLIGATORY.
  p_hkont   FOR zfit0232-hkont.
SELECTION-SCREEN END OF BLOCK part1.

*SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-C02 .
*  PARAMETERS: p_bsik  AS CHECKBOX DEFAULT abap_false USER-COMMAND filtro,
*              p_bsak  AS CHECKBOX DEFAULT abap_false USER-COMMAND filtro,
*              p_bsid  AS CHECKBOX DEFAULT abap_false USER-COMMAND filtro,
*              p_bsad  AS CHECKBOX DEFAULT abap_false USER-COMMAND filtro.
*
*SELECTION-SCREEN END OF BLOCK part2.

INITIALIZATION.


AT SELECTION-SCREEN OUTPUT.

  it_screen_status = VALUE #( ( CONV sy-ucomm( '' ) ) ).

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = gv_status1000
      p_program = sy-repid
    TABLES
      p_exclude = it_screen_status.


AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'BT_SCREEN100'.
      LOOP AT SCREEN.
        IF p_bukrs IS INITIAL.
*          IF screen-name(7) = 'P_BUKRS'.
*            screen-required = '2'.
*            MODIFY SCREEN.
*          ENDIF.
        ENDIF.
      ENDLOOP.

*      IF P_BSID IS INITIAL AND P_BSAD IS INITIAL AND P_BSIK IS INITIAL AND P_BSAK IS INITIAL.
*        MESSAGE 'Selecione Uma Tabela!' TYPE 'E'.
*        STOP.
*      ENDIF.

      IF p_bukrs IS INITIAL.
        MESSAGE 'Empresa é Obrigatorio!' TYPE 'E'.
        STOP.
      ENDIF.

      IF p_gjahr IS INITIAL.
        MESSAGE 'Ano é Obrigatorio!' TYPE 'E'.
        STOP.
      ENDIF.

      IF p_monat IS INITIAL.
        MESSAGE 'Mês é Obrigatorio!' TYPE 'E'.
        STOP.
      ENDIF.

      IF p_paber IS INITIAL.
        MESSAGE 'Data de Partida em Aberto é Obrigatorio!' TYPE 'E'.
        STOP.
      ENDIF.

      PERFORM convert_tool CHANGING p_dtini p_dtfim p_dtsub p_ukurs.

      SELECT SINGLE * FROM zfit0232 WHERE ano = @p_gjahr-low AND mes = @p_monat-low AND bukrs = @p_bukrs-low INTO @DATA(_saida03).

      IF _saida03 IS INITIAL.

        p_create = abap_true.

        FREE: lt_param_hkont_ativo.
        SELECT DISTINCT hkont_ativo FROM zfit0231 WHERE hkont_ativo IS NOT INITIAL INTO TABLE @lt_param_hkont_ativo.
        IF lt_param_hkont_ativo IS INITIAL.
          MESSAGE 'Sem Parâmetros cadastrados na tabela zfit0231!' TYPE 'I'.
          STOP.
        ENDIF.
      ENDIF.

      CALL SELECTION-SCREEN '0100'.

    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'BT_SCREEN200'.

      CALL SELECTION-SCREEN '0200'.

  ENDCASE.


START-OF-SELECTION.
