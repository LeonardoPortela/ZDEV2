*&---------------------------------------------------------------------*
*& Report  ZMMR019_02
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR019_02.

FORM WAIT_DOCUMENTO_FISCAL  USING P_WREFKEY TYPE J_1BNFLIN-REFKEY
                                  P_WA_ITEM_MOVE_TYPE TYPE BWART
                            CHANGING E_DOCNUM TYPE J_1BDOCNUM.

  CHECK P_WREFKEY IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(WA_T156) FROM T156
   WHERE BWART EQ @P_WA_ITEM_MOVE_TYPE.

  CHECK SY-SUBRC IS INITIAL.

  CHECK WA_T156-J_1BNFTYPE IS NOT INITIAL.

  DO 5 TIMES.
    IF E_DOCNUM IS INITIAL.
      WAIT UP TO 2 SECONDS.
      SELECT SINGLE DOCNUM INTO @E_DOCNUM FROM J_1BNFLIN WHERE REFKEY = @P_WREFKEY.
    ENDIF.
  ENDDO.

ENDFORM.
