*&---------------------------------------------------------------------*
*& Report ZPPR029
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr029.

INCLUDE zppr029_class.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS: s_nrobol FOR lcl_programa=>w_tela-nrobol OBLIGATORY NO INTERVALS NO-EXTENSION,
                  s_werks  FOR lcl_programa=>w_tela-werks  OBLIGATORY NO INTERVALS NO-EXTENSION,
                  s_dtmvto FOR lcl_programa=>w_tela-dtmvto OBLIGATORY NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  DATA:
        o_programa TYPE REF TO lcl_programa.

  CREATE OBJECT o_programa
    EXPORTING
      i_r_nrobol = s_nrobol[]
      i_r_werks  = s_werks[]
      i_r_dtmvto = s_dtmvto[].

  o_programa->iniciar( ).
