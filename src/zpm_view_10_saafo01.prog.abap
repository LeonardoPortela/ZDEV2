*&---------------------------------------------------------------------*
*&  Include           ZPM_VIEW_10_SAAFO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DESCRICAO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DESCRICAO OUTPUT.
*
  IF IT_ZTPM[] IS INITIAL.
    SELECT * INTO TABLE IT_ZTPM
      FROM ZTPM_VEI_OP_SAAF.
    LOOP AT IT_ZTPM.
      MOVE-CORRESPONDING IT_ZTPM TO T_VIEW.
      APPEND T_VIEW.
    ENDLOOP.
    SORT T_VIEW ASCENDING BY EQART TYPBZ.

  ENDIF.
* Descrições
  IF NOT V_CODE IS INITIAL.
    SELECT CODE KURZTEXT INTO TABLE IT_CODE
      FROM QPCT UP TO 1 ROWS
      WHERE KATALOGART = LC_S
        AND CODEGRUPPE = LC_FPN
        AND CODE       = V_CODE
        AND SPRACHE    = LC_PT.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_CODE INDEX 1.
      V_CODE = IT_CODE-CODE.
      V_DESC_CODE = IT_CODE-KURZTEXT.
    ENDIF.
  ENDIF.
*
  IF NOT V_EQART IS INITIAL.
    SELECT EQART EARTX INTO TABLE IT_EQART
    FROM T370K_T
    WHERE EQART  = V_EQART
      AND SPRAS  = LC_PT.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_EQART INDEX 1.
      V_EQART = IT_EQART-EQART.
      V_DESC_EQART = IT_EQART-EARTX.
    ENDIF.
  ENDIF.
*
  IF NOT V_TYPBZ IS INITIAL.
    CLEAR IT_ZVAL[].

    SELECT ZVAL CONST
     FROM ZTPARAM
     INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
     WHERE PARAM EQ LC_TP_OBJ
     AND ABASTEC EQ LC_X.
    LOOP AT IT_PARAM.
      IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
      APPEND IT_ZVAL.
    ENDLOOP.
*
    SELECT TYPBZ INTO TABLE IT_TYPBZ
      FROM EQUI FOR ALL ENTRIES IN IT_ZVAL
      WHERE EQTYP   = IT_ZVAL-ZVAL
        AND EQART   = V_EQART
        AND TYPBZ   = V_TYPBZ.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_TYPBZ INDEX 1.
      V_TYPBZ = IT_TYPBZ-TYPBZ.
    ENDIF.
  ENDIF.
ENDMODULE.
