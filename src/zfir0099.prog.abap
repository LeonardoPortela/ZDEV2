*&---------------------------------------------------------------------*
*& Report ZFIR0099
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0099.
TABLES: j_1bnflin.
DATA: it_j_1bnflin TYPE TABLE OF j_1bnflin.
FIELD-SYMBOLS: <ws_j_1bnflin> TYPE j_1bnflin.

DATA: v_text   TYPE string,
      vg_tmiss TYPE j_1btmiss.



* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_cp_01 RADIOBUTTON GROUP rad1 USER-COMMAND act DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i01. "Alteração CFOP
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_cp_02 RADIOBUTTON GROUP rad1.
    SELECTION-SCREEN COMMENT 03(65) TEXT-i02. "Imposto sobre serviço - nenhum cálculo de ICMS/IPI
  SELECTION-SCREEN END OF LINE.

*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS r_cp_10 RADIOBUTTON GROUP rad1.
*    SELECTION-SCREEN COMMENT 03(65) TEXT-i16. "
*  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:  p_docnum FOR j_1bnflin-docnum,
                   p_cfop FOR j_1bnflin-cfop NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_rd_01 RADIOBUTTON GROUP gr2 USER-COMMAND act DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i03. "Marcar - Imposto sobre serviço - nenhum cálculo de ICMS/IPI
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_rd_02 RADIOBUTTON GROUP gr2.
    SELECTION-SCREEN COMMENT 03(65) TEXT-i04. "Desmarcar - Imposto sobre serviço - nenhum cálculo de ICMS/IPI
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.

*  LOOP AT SCREEN.
*    CASE screen-name.
*      WHEN '%CI03017_1000'.
*        IF r_cp_02  IS INITIAL.
*          screen-active = '0'.         "show parameters     "n921165
*        ELSE.                                               "n921165
*          screen-active = '1'.         "Hide parameters     "n921165
*        ENDIF.
*        MODIFY SCREEN.
*
*    ENDCASE.
*  ENDLOOP.

START-OF-SELECTION.

  CLEAR:v_text.

  CONDENSE p_docnum NO-GAPS.

  CASE abap_true.
    WHEN r_cp_01.

      IF p_docnum IS NOT INITIAL AND  p_cfop IS NOT INITIAL.

        SELECT * FROM j_1bnflin
          INTO TABLE it_j_1bnflin
          WHERE docnum IN  p_docnum.

        IF sy-subrc IS INITIAL.
          LOOP AT it_j_1bnflin ASSIGNING <ws_j_1bnflin>.
            <ws_j_1bnflin>-cfop = p_cfop-low.
          ENDLOOP.

          MODIFY j_1bnflin FROM TABLE it_j_1bnflin.
          COMMIT WORK.
          v_text = 'CFOP Alterado com sucesso!'.
          MESSAGE v_text  TYPE 'S' .
        ENDIF.

      ELSE.
        v_text = 'Favor informar os parametros obrigatórios!'.
        "MESSAGE a024(sd) with  v_text.
        MESSAGE v_text  TYPE 'S' DISPLAY LIKE 'A'.
        RETURN.
        "exit.

      ENDIF.

    WHEN r_cp_02.
      CLEAR: vg_tmiss.
      IF p_docnum IS NOT INITIAL.
        IF r_rd_01 IS NOT INITIAL.
          vg_tmiss = abap_true.
        ELSE.
          vg_tmiss = abap_false.
        ENDIF.

        SELECT * FROM j_1bnflin
        INTO TABLE it_j_1bnflin
        WHERE docnum IN  p_docnum.

        IF sy-subrc IS INITIAL.
          LOOP AT it_j_1bnflin ASSIGNING <ws_j_1bnflin>.
            <ws_j_1bnflin>-tmiss = vg_tmiss.
          ENDLOOP.

          MODIFY j_1bnflin FROM TABLE it_j_1bnflin.
          COMMIT WORK.
          v_text = 'Imposto sobre serviço Alterado com sucesso!'.
          MESSAGE v_text  TYPE 'S' .
        ENDIF.

      ENDIF.
    WHEN OTHERS.
  ENDCASE.
