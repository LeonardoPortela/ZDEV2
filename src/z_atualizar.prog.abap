*----------------------------------------------------------------------*
***INCLUDE Z_ATUALIZAR .
*----------------------------------------------------------------------*
FORM INSERT_INFOS_AFTER.
  IF NOT ( ZSDT0060-USNAM IS INITIAL ).

    UPDATE ZSDT0060
        SET DATA_ATUAL =  SY-DATUM
            HORA_ATUAL =  SY-UZEIT
*          USUARIO    =  SY-UNAME
    WHERE USNAM EQ ZSDT0060-USNAM.

    IF ( SY-SUBRC EQ 0 ).
      ZSDT0060-DATA_ATUAL = SY-DATUM.
      ZSDT0060-HORA_ATUAL = SY-UZEIT.
*    ZSDT0060-USUARIO    = SY-UNAME.
    ENDIF.


  ENDIF.
ENDFORM.                    "
*&---------------------------------------------------------------------*
*&      Form  INSERT_INFOS_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INSERT_INFOS_SAVE.
  ZSDT0060-DATA_ATUAL = SY-DATUM.
  ZSDT0060-HORA_ATUAL = SY-UZEIT.
*  ZSDT0060-USNAM      = SY-UNAME.
ENDFORM.                    "INSERT_INFOS_SAVE
