*----------------------------------------------------------------------*
***INCLUDE ZMMR105_0303 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0303  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0303 OUTPUT.

  CLEAR: IT_EXCLUDE_FCODE.

  WA_EXCLUDE_FCODE = OK_CONFIRMAR.
  APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.

  SET PF-STATUS 'PFTELA' EXCLUDING IT_EXCLUDE_FCODE.
  SET TITLEBAR 'TL0303'.

  IF WA_ADD_NFE_0103-N55_CHAVE_ACESSO IS NOT INITIAL.
    PERFORM BUSCAR_INDO_NOTA_DIGITADA.
  ENDIF.

ENDMODULE.                 " STATUS_0303  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0303_EXIT INPUT.
  WA_ADD_NFE_0103-CK_INCLUIR = ABAP_FALSE.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0303_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUSCAR_INDO_NOTA_DIGITADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCAR_INDO_NOTA_DIGITADA .

  DATA: QTD           TYPE I,
        WA_ACTIVE     TYPE J_1BNFE_ACTIVE,
        WA_J_1BNFDOC  TYPE J_1BNFDOC.

  QTD = STRLEN( WA_ADD_NFE_0103-N55_CHAVE_ACESSO ).

  WA_ADD_NFE_0103-CK_INCLUIR = ABAP_FALSE.

  IF QTD EQ 44.
    WA_ACTIVE-REGIO   = WA_ADD_NFE_0103-N55_CHAVE_ACESSO(2).
    WA_ACTIVE-NFYEAR  = WA_ADD_NFE_0103-N55_CHAVE_ACESSO+2(2).
    WA_ACTIVE-NFMONTH = WA_ADD_NFE_0103-N55_CHAVE_ACESSO+4(2).
    WA_ACTIVE-STCD1   = WA_ADD_NFE_0103-N55_CHAVE_ACESSO+6(14).
    WA_ACTIVE-MODEL   = WA_ADD_NFE_0103-N55_CHAVE_ACESSO+20(2).
    WA_ACTIVE-SERIE   = WA_ADD_NFE_0103-N55_CHAVE_ACESSO+22(3).
    WA_ACTIVE-NFNUM9  = WA_ADD_NFE_0103-N55_CHAVE_ACESSO+25(9).
    WA_ACTIVE-DOCNUM9 = WA_ADD_NFE_0103-N55_CHAVE_ACESSO+34(9).
    WA_ACTIVE-CDV     = WA_ADD_NFE_0103-N55_CHAVE_ACESSO+43(1).

    SELECT SINGLE * INTO WA_ACTIVE
      FROM J_1BNFE_ACTIVE
     WHERE REGIO   EQ WA_ACTIVE-REGIO
       AND NFYEAR  EQ WA_ACTIVE-NFYEAR
       AND NFMONTH EQ WA_ACTIVE-NFMONTH
       AND STCD1   EQ WA_ACTIVE-STCD1
       AND MODEL   EQ WA_ACTIVE-MODEL
       AND SERIE   EQ WA_ACTIVE-SERIE
       AND NFNUM9  EQ WA_ACTIVE-NFNUM9
       AND DOCNUM9 EQ WA_ACTIVE-DOCNUM9
       AND CDV     EQ WA_ACTIVE-CDV.

    IF SY-SUBRC IS INITIAL.

      WA_ADD_NFE_0103-CK_INCLUIR = ABAP_TRUE.
      WA_ADD_NFE_0103-DOCNUM_NFE = WA_ACTIVE-DOCNUM.
      WA_ADD_NFE_0103-BUKRS      = WA_ACTIVE-BUKRS.
      WA_ADD_NFE_0103-BRANCH     = WA_ACTIVE-BRANCH.
      WA_ADD_NFE_0103-PARID      = WA_ACTIVE-PARID.

      SELECT SINGLE BUTXT INTO WA_ADD_NFE_0103-BUTXT
        FROM T001
       WHERE BUKRS EQ WA_ADD_NFE_0103-BUKRS.

      SELECT SINGLE NAME INTO WA_ADD_NFE_0103-NAME
        FROM J_1BBRANCH
       WHERE BUKRS  EQ WA_ADD_NFE_0103-BUKRS
         AND BRANCH EQ WA_ADD_NFE_0103-BRANCH.

      CASE WA_ACTIVE-PARTYP.
        WHEN 'V'.
          SELECT SINGLE NAME1 INTO WA_ADD_NFE_0103-NAME1
            FROM LFA1
           WHERE LIFNR EQ WA_ADD_NFE_0103-PARID.
        WHEN 'C'.
          SELECT SINGLE NAME1 INTO WA_ADD_NFE_0103-NAME1
            FROM KNA1
           WHERE KUNNR EQ WA_ADD_NFE_0103-PARID.
        WHEN 'B'.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_ACTIVE-PARID
            IMPORTING
              OUTPUT = WA_ACTIVE-PARID.

          SELECT SINGLE NAME INTO WA_ADD_NFE_0103-NAME1
            FROM J_1BBRANCH
           WHERE BUKRS  EQ WA_ACTIVE-PARID+2(4)
             AND BRANCH EQ WA_ACTIVE-PARID+6(4).
      ENDCASE.

      SELECT SINGLE * INTO WA_J_1BNFDOC
        FROM J_1BNFDOC
       WHERE DOCNUM EQ WA_ACTIVE-DOCNUM.

      WA_ADD_NFE_0103-NFTOT = WA_J_1BNFDOC-NFTOT.
      WA_ADD_NFE_0103-NTGEW = WA_J_1BNFDOC-NTGEW.
      WA_ADD_NFE_0103-N55_STAT_SEFAZ = WA_ACTIVE-CODE.

    ELSE.
      "Doc. Fiscal não Loc.: Nr.: &1 Sr.: &2 Md.: &3 CNPJ: &4
      MESSAGE S010 WITH WA_ACTIVE-NFNUM9 WA_ACTIVE-SERIE WA_ACTIVE-MODEL WA_ACTIVE-STCD1.
    ENDIF.
  ELSE.
    MESSAGE S002 WITH WA_ADD_NFE_0103-N55_CHAVE_ACESSO.
  ENDIF.

ENDFORM.                    " BUSCAR_INDO_NOTA_DIGITADA
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0303 INPUT.

  CASE OK_CODE.
    WHEN OK_SALVAR.
      PERFORM BUSCAR_INDO_NOTA_DIGITADA.
      IF WA_ADD_NFE_0103-CK_INCLUIR EQ ABAP_TRUE.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0303  INPUT
