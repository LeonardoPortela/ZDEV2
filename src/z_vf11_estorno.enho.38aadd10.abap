"Name: \PR:SAPMV60A\FO:FCODE_BEARBEITEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_VF11_ESTORNO.
*
  if sy-tcode = 'VF11'.
    DATA:   XDOC_REM      TYPE VBFA-VBELV    ,
        XVBELN        TYPE VBFA-VBELN        ,
        VTKNUM        TYPE VBAK-TKNUM        .

    READ TABLE XVBRK index 1.
    SELECT SINGLE  VBELV
        FROM VBFA
        INTO XDOC_REM
        WHERE VBELN   = XVBRK-VBELN
        AND   VBTYP_N  =  'M'
        AND   VBTYP_V  =  'J'
        AND   STUFE    =  ''.

        if SY-SUBRC = 0.
           SELECT SINGLE  VBELN
           FROM VBFA
           INTO XVBELN
           WHERE VBELV  = XDOC_REM
           AND   VBTYP_N  =  '8'
           AND   VBTYP_V  =  'J'.

           if SY-SUBRC = 0.
             SELECT SINGLE TKNUM
               FROM VBAK
               INTO VTKNUM
               WHERE TKNUM =   XVBELN.

             IF SY-SUBRC = 0.
               MESSAGE E398(00) WITH 'Fatura não pode ser estornada precisa antes   '
                                     'cancelar o processo ref.doc.transp.nro.       '
                                     XVBELN.
             ENDIF.
           endif.
        endif.
  endif.
ENDENHANCEMENT.
