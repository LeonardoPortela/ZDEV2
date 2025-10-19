*&---------------------------------------------------------------------*
*& Report  ZLESR0145
*&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Mostra arquivo XML ANTAQ                                *
* Transação..: ZLES                                                    *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data    | Nome      | Request | Descrição                            *
*----------------------------------------------------------------------*
**23.04.20|JALEXANDRE |         | Gera  arquivo XML ANTAQ              *
*----------------------------------------------------------------------*

REPORT ZLESR0145.

DATA: W_ZLEST0202 TYPE ZLEST0202,
      W_ZLEST0203 TYPE ZLEST0203.

SELECTION-SCREEN: BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS:  P_NRATR TYPE ZLEST0202-NR_ATRACACAO.
SELECTION-SCREEN END OF BLOCK BL1.

AT SELECTION-SCREEN.

  GET PARAMETER ID 'ATR' FIELD P_NRATR.

START-OF-SELECTION.

  PERFORM ZF_BUSCAR_ATRACACAO.

  CHECK W_ZLEST0202 IS NOT INITIAL.

  PERFORM ZF_VISUALIZAR_XML.

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCAR_ATRACAÇÃO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_BUSCAR_ATRACACAO .

  CLEAR: W_ZLEST0202, W_ZLEST0203.

* Buscar xml da atracação
  SELECT SINGLE * FROM ZLEST0202
    INTO W_ZLEST0202
    WHERE NR_ATRACACAO = P_NRATR.

  IF W_ZLEST0202-NR_MOVIMENTO IS NOT INITIAL.

    SELECT SINGLE * FROM ZLEST0203
      INTO W_ZLEST0203
      WHERE NR_MOVIMENTO = W_ZLEST0202-NR_MOVIMENTO.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VISUALIZAR_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_VISUALIZAR_XML .

  CONSTANTS: C_DATATYPE     TYPE PRX_R3NAME VALUE 'ZLESMESSAGE1'.

  DATA : L_XML  TYPE XSTRING.

  TRY.

      L_XML =  W_ZLEST0203-XML.

* Remove namespace das tags
*      CALL TRANSFORMATION ZLES_REMOVE_NAMESPACE_XSLT
*      SOURCE XML L_XML
*      RESULT XML L_XML.

*   Just to show the xml .
      CL_SRTG_HELPER=>WRITE_UTF8_XMLDOC(
        DOC = L_XML
        USE_HTML_CONTROL = 'X'
      ).

    CATCH CX_TRANSFORMATION_ERROR .
    CATCH CX_ROOT .                                      "#EC CATCH_ALL

  ENDTRY.

ENDFORM.
