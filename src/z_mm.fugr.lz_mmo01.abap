*----------------------------------------------------------------------*
***INCLUDE LZ_MMO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module IM_LIST OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE IM_LIST OUTPUT.
*  SET TITLEBAR 'T03' WITH DYNP_TITLE_TEXT.
*  IF SAV_EINSCHRAENKUNG IS INITIAL.
*    SET PF-STATUS 'S200' EXCLUDING 'EINS'.
*  ELSE.
*    SET PF-STATUS 'S200'.
*  ENDIF.
*
*  SUPPRESS DIALOG.
*  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
*
*  FORMAT INTENSIFIED ON.
*  LOOP AT DYNP_ANZEIGE_LISTE.
*    WRITE / DYNP_ANZEIGE_LISTE.
*    IF SY-TABIX = 1. ULINE. FORMAT INTENSIFIED OFF. ENDIF.
*    HIDE DYNP_ANZEIGE_LISTE.
*  ENDLOOP.
*  CLEAR DYNP_ANZEIGE_LISTE.
ENDMODULE.
