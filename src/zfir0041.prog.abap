*&---------------------------------------------------------------------*
*& Report  ZFIR0041
*&
*&---------------------------------------------------------------------*
*&TITULO: Atualização INVOICES - Interface
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 09.12.2013
*TRANSACAO: ZFI0038
*&---------------------------------------------------------------------*

REPORT  zfir0041.

TYPE-POOLS: vrm.
DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zib_contabil_chv.



*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_cadlan,
         bukrs TYPE zib_contabil_chv-bukrs,
         belnr TYPE zib_contabil_chv-belnr,
         gjahr TYPE zib_contabil_chv-gjahr,
         navio TYPE zfit0036-navio,
         invoi TYPE zfit0036-invoice_terc,
         invot TYPE zfit0036-invoice,
         matnr TYPE zfit0036-matnr,
         tipo  TYPE zfit0036-id_tipo_invoice,
         perfo TYPE zfit0036-in_performance,
       END OF ty_cadlan.
*----------------------------------------------------------------------*
* TABELAS INTERNAS
*----------------------------------------------------------------------*
DATA: it_zib_contabil               TYPE TABLE OF zib_contabil.
*----------------------------------------------------------------------*
* TABELAS WORKAREAS
*----------------------------------------------------------------------*

DATA: wa_zib_contabil_chv TYPE zib_contabil_chv,
      wa_zib_contabil     TYPE zib_contabil,
      wa_zfit0036         TYPE zfit0036,
      wg_cadlan           TYPE ty_cadlan.


*----------------------------------------------------------------------*
* TABELAS VARIÁVEIS
*----------------------------------------------------------------------*
DATA: v_navio                       TYPE zfit0036-navio.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS    : p_bukrs TYPE zib_contabil_chv-bukrs OBLIGATORY,
                  p_belnr TYPE zib_contabil_chv-belnr OBLIGATORY,
                  p_gjahr TYPE zib_contabil_chv-gjahr OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

INITIALIZATION.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
          f_seleciona_dados.


END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  DATA: zlv_offset TYPE i.

  CLEAR wa_zib_contabil_chv.

    SELECT SINGLE *
    FROM zib_contabil_chv
    INTO wa_zib_contabil_chv
    WHERE bukrs = p_bukrs
      AND belnr = p_belnr
      AND gjahr = p_gjahr.


  IF sy-subrc NE 0.
    MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Documento Contábil não encontrado'.
    EXIT.
  ENDIF.

*  SELECT SINGLE *
*    FROM ZFIT0036
*    INTO WA_ZFIT0036
*    WHERE OBJ_KEY = WA_ZIB_CONTABIL_CHV-OBJ_KEY.

*  IF SY-SUBRC EQ 0.
*    MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Invoice já gravada!'.
*    EXIT.
*  ENDIF.

  SELECT SINGLE *
  FROM zib_contabil
  INTO wa_zib_contabil
  WHERE obj_key =  wa_zib_contabil_chv-obj_key.

  CLEAR wg_cadlan-navio.
  TRANSLATE wa_zib_contabil-sgtxt TO UPPER CASE.

  FIND FIRST OCCURRENCE OF 'NAVIO' IN wa_zib_contabil-sgtxt MATCH OFFSET zlv_offset.
  IF zlv_offset = 0.
    FIND FIRST OCCURRENCE OF 'VESSEL:' IN wa_zib_contabil-sgtxt MATCH OFFSET zlv_offset.
    IF zlv_offset GT 0.
      ADD 7 TO zlv_offset.
      wg_cadlan-navio = wa_zib_contabil-sgtxt+zlv_offset.
    ENDIF.
  ELSE.

    IF zlv_offset GT 0.
      ADD 6 TO zlv_offset.
      wg_cadlan-navio   = wa_zib_contabil-sgtxt+zlv_offset.
    ENDIF.
  ENDIF.

  wg_cadlan-invoi =  wa_zib_contabil-zuonr.
  wg_cadlan-bukrs =  p_bukrs.
  wg_cadlan-belnr =  p_belnr.
  wg_cadlan-gjahr =  p_gjahr.

  CALL SCREEN 0100 STARTING AT 070 3
                     ENDING AT 130 15.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA   w_answer(1).

  CASE sy-ucomm.
    WHEN 'SAIR'.
      SET SCREEN 0.
    WHEN 'SAVE'.
      IF  wg_cadlan-belnr IS INITIAL.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Documento Contábil não encontrado'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
       FROM zfit0036
       INTO wa_zfit0036
       WHERE obj_key = wa_zib_contabil_chv-obj_key.

      w_answer = 'N'.
      IF sy-subrc = 0.
        w_answer = 'S'.
        MESSAGE 'Invoice já gravada, será alterado apenas o material' TYPE 'I'.

        UPDATE zfit0036
           SET matnr = wg_cadlan-matnr
        WHERE obj_key = wa_zib_contabil_chv-obj_key.

      ELSE.
        CLEAR wa_zfit0036.
        wa_zfit0036-obj_key         = wa_zib_contabil_chv-obj_key.
        wa_zfit0036-bukrs           = wa_zib_contabil_chv-bukrs.
        wa_zfit0036-invoice         = wa_zib_contabil-zuonr.
        wa_zfit0036-navio           = wg_cadlan-navio.
        wa_zfit0036-in_performance  = wg_cadlan-perfo.
        wa_zfit0036-invoice_terc    = wg_cadlan-invot.
        wa_zfit0036-id_tipo_invoice = wg_cadlan-tipo.
        wa_zfit0036-matnr           = wg_cadlan-matnr.
        MODIFY zfit0036 FROM wa_zfit0036.
      ENDIF.

      MESSAGE 'Invoice gravada com sucesso' TYPE 'I'.
      SET SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_TIPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_tipo INPUT.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_tipo OCCURS 0,
          id_tipo_invoice TYPE zfit0047-id_tipo_invoice,
          descricao       TYPE zfit0047-descricao,
        END OF tl_tipo.

  SELECT id_tipo_invoice descricao
  FROM zfit0047
  INTO TABLE tl_tipo.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ID_TIPO_INVOICE'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0047-ID_TIPO_INVOICE'
      value_org       = 'S'
    TABLES
      value_tab       = tl_tipo
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.                 " SEARCH_TIPO  INPUT

*&---------------------------------------------------------------------*
*&      Module  SEARCH_PERFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_perfo INPUT.
  DATA: tl_return_tab2 TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc2      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_perfo OCCURS 0,
          pgto      TYPE zfit0047-pgto,
          descricao TYPE zfit0047-descricao,
        END OF tl_perfo.

  tl_perfo-pgto = 'S'.
  tl_perfo-descricao = 'Sim'.
  APPEND tl_perfo.

  tl_perfo-pgto = 'N'.
  tl_perfo-descricao = 'Não'.
  APPEND tl_perfo.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PGTO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0047-PGTO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_perfo
      return_tab      = tl_return_tab2
      dynpfld_mapping = tl_dselc2.

ENDMODULE.                 " SEARCH_PERFO  INPUT
