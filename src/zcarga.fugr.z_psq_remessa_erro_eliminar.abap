FUNCTION Z_PSQ_REMESSA_ERRO_ELIMINAR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN_VA
*"  EXCEPTIONS
*"      ERRO
*"----------------------------------------------------------------------

  DATA: EXC_REF    TYPE REF TO CX_SY_NATIVE_SQL_ERROR,
        ERROR_TEXT TYPE STRING.

  CLEAR: IT_REMESSA[], IT_REMESSA, WA_REMESSA.

  TRY.
      EXEC SQL.
        OPEN REMESSAS FOR
          SELECT KP.VBELN, KP.ERNAM, KP.INCO1, KP.ROUTE, LP.MEINS, LP.LFIMG, LP.NTGEW, LP.BRGEW, LP.ARKTX
            FROM SAPHANADB.LIPS LP,
                 SAPHANADB.LIKP KP
           WHERE LP.MANDT = :SY-MANDT
             AND LP.VGBEL = :I_VBELN
             AND LP.MANDT = KP.MANDT
             AND LP.VBELN = KP.VBELN
             AND EXISTS ( SELECT *
                            FROM SAPHANADB.VBFA VF
                           WHERE VF.MANDT = LP.MANDT
                             AND VF.VBELV = LP.VBELN
                             AND VF.POSNV = LP.POSNR
                             AND VF.VBTYP_V = 'J'
                             AND VF.VBTYP_N = 'h' )
      ENDEXEC.
    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
      ERROR_TEXT = EXC_REF->GET_TEXT( ).
      MESSAGE ERROR_TEXT TYPE 'E' RAISING ERRO.
  ENDTRY.

  DO.
    EXEC SQL.
      FETCH NEXT REMESSAS INTO
      :WA_REMESSA-VBELN,
      :WA_REMESSA-ERNAM,
      :WA_REMESSA-INCO1,
      :WA_REMESSA-ROUTE,
      :WA_REMESSA-MEINS,
      :WA_REMESSA-LFIMG,
      :WA_REMESSA-NTGEW,
      :WA_REMESSA-BRGEW,
      :WA_REMESSA-ARKTX
    ENDEXEC.
    IF SY-SUBRC <> 0.
      EXIT.
    ELSE.
      WA_REMESSA-ICO_REME = ICON_DELETE.
      APPEND WA_REMESSA TO IT_REMESSA.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE REMESSAS
  ENDEXEC.

  PI_VBELN = I_VBELN.

  CALL SCREEN 9003 STARTING AT 10 05.

ENDFUNCTION.
