*=============================================================================*
* Report  ZSDR0068                                                           *
*                                                                             *
*=============================================================================*
REPORT ZSDR0068.
*=============================================================================*
*TYPES                                                                        *
*=============================================================================*
TYPES: BEGIN OF TY_ZSDT0136_ALV,
         ANTIG TYPE CHAR1.
        INCLUDE STRUCTURE ZSDT0136.
TYPES: CELLSTYLES TYPE LVC_T_STYL.
TYPES: END OF TY_ZSDT0136_ALV.
*=============================================================================*
*VARIÁVEIS                                                                    *
*=============================================================================*
DATA: IT_ZSDT0136 TYPE STANDARD TABLE OF TY_ZSDT0136_ALV.
*=============================================================================*
*START OF SELECTION                                                           *
*=============================================================================*
START-OF-SELECTION.

  PERFORM BUSCA_NROMANEIOS.

  CALL FUNCTION 'ZENQUEUE_ZSDT0116'
    EXPORTING
      CHAVE          = 'ZSDT0116'
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
  ELSE.
    CALL SCREEN 5000.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_NROMANEIOS
*&---------------------------------------------------------------------*
FORM BUSCA_NROMANEIOS.

  DATA: WA_ZSDT0136 TYPE TY_ZSDT0136_ALV.

  SELECT *
    FROM ZSDT0136
    INTO CORRESPONDING FIELDS OF TABLE IT_ZSDT0136.

  LOOP AT IT_ZSDT0136 INTO WA_ZSDT0136.
    MOVE ABAP_TRUE TO WA_ZSDT0136-ANTIG.
    MODIFY IT_ZSDT0136 FROM WA_ZSDT0136 INDEX SY-TABIX.
  ENDLOOP.


ENDFORM.

INCLUDE ZSDR0068_5000.
