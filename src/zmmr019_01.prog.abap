*&---------------------------------------------------------------------*
*& Report  ZMMR019_01
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR019_01.

FORM VERIFICA_MIRO  USING    P_WA_MOV_ESTQ TYPE ZMMT_EE_ZGR
                             P_VG_OBJ_KEY  TYPE ZMMT_EE_ZGR-OBJ_KEY
                    CHANGING P_CK_COTINUE TYPE CHAR01
                             VG_RETURN    TYPE STRING.

  SELECT SINGLE DOCNUM
    INTO @DATA(V_DOCNUM)
      FROM ZSDT0231
     WHERE OBJ_KEY EQ @P_WA_MOV_ESTQ-OBJ_KEY(11).

  IF SY-SUBRC IS INITIAL.

    SELECT SINGLE REFKEY
      INTO @DATA(V_REFKEY)
        FROM J_1BNFLIN
      WHERE DOCNUM EQ @V_DOCNUM.

    IF SY-SUBRC IS INITIAL.

      SELECT SINGLE BELNR
        INTO @DATA(V_BELNR)
          FROM RBKP
        WHERE BELNR EQ @V_REFKEY(10)
          AND STBLG EQ @ABAP_FALSE.

      IF SY-SUBRC IS INITIAL.
        VG_RETURN = |Já tem uma MIRO ativa para essa Entrada de estoque MIRO: { V_BELNR }!|.
        P_CK_COTINUE = ABAP_TRUE.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
