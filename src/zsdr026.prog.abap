*&--------------------------------------------------------------------&*
*&                     Relatório Módulo - SD                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 04/07/2024                                              &*
*& Descrição: Cadastro de valores UPF e Alíquotas Retenção nas Vendas &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zsdr026.

TABLES: t001, t001w.

DATA: r_bukrs  TYPE RANGE OF t001-bukrs  WITH HEADER LINE,
      r_bukrs2 TYPE RANGE OF t001-bukrs  WITH HEADER LINE,
      r_werks  TYPE RANGE OF t001w-werks WITH HEADER LINE.

TYPES: BEGIN OF ty_bukrs,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE char4,
         high   TYPE char4,
       END OF ty_bukrs.

DATA: it_bukrs  TYPE STANDARD TABLE OF ty_bukrs WITH HEADER LINE,
      it_bukrs2 TYPE STANDARD TABLE OF ty_bukrs WITH HEADER LINE,
      it_range  TYPE TABLE OF ZSDT0343_range,
      wa_range  TYPE ZSDT0343_range.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs  FOR t001-bukrs  NO INTERVALS OBLIGATORY MEMORY ID buk,
                  p_werks  FOR t001w-werks. " MEMORY ID wrk.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: r_cada RADIOBUTTON GROUP g1 DEFAULT 'X',
              r_calc RADIOBUTTON GROUP g1,
              r_vinc RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.

START-OF-SELECTION.

  PERFORM: fs_selection,
           fs_transactions.


*&---------------------------------------------------------------------*
*& Form fs_selection
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fs_selection .

  TRY.

      DELETE FROM ZSDT0343_range WHERE campo IN ( 'BUKRS', 'WERKS' ).
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.

      LOOP AT p_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>).
        wa_range-mandt    = sy-mandt.
        wa_range-campo    = 'BUKRS'.
        wa_range-id       = sy-tabix.
        wa_range-sign     = <fs_bukrs>-sign.
        wa_range-c_option = <fs_bukrs>-option.
        wa_range-low      = <fs_bukrs>-low.
        wa_range-high     = <fs_bukrs>-high.
        APPEND wa_range TO it_range.
        CLEAR wa_range.
      ENDLOOP.

      LOOP AT p_werks ASSIGNING FIELD-SYMBOL(<fs_werks>).
        wa_range-mandt    = sy-mandt.
        wa_range-campo    = 'WERKS'.
        wa_range-id       = sy-tabix.
        wa_range-sign     = <fs_werks>-sign.
        wa_range-c_option = <fs_werks>-option.
        wa_range-low      = <fs_werks>-low.
        wa_range-high     = <fs_werks>-high.
        APPEND wa_range TO it_range.
        CLEAR wa_range.
      ENDLOOP.

      MODIFY ZSDT0343_range FROM TABLE it_range.
      COMMIT WORK.

    CATCH cx_root.
      MESSAGE e398(00) WITH  'ERROR: Na seleção da tela,'
                             'contatar TI.'
                      RAISING error.
      RETURN.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fs_transactions
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fs_transactions .

  CASE abap_true.
    WHEN r_cada.
      CALL TRANSACTION 'ZSDT0212'. " AND SKIP FIRST SCREEN.
    WHEN r_calc.
      CALL TRANSACTION 'ZSDT0213'. " AND SKIP FIRST SCREEN.
    WHEN r_vinc.
      CALL TRANSACTION 'ZSDT0214'. " AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.
