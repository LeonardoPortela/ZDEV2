*         20020109
*-------------------------------------------------------------------*
*         Gerado por      SPROCWORK
*         Data:           26.10.09
*         Hora            14:26.05
*-------------------------------------------------------------------*

*-------------------------------------------------------------------*
*         Include fuer SAPFGVTR (FGVTRF64)
*         Der Include ist generiert vom Programm RGUGVTR0
*-------------------------------------------------------------------*


*-------------------------------------------------------------------*
*         FORM CLEAR_UPDATE_TABLES
*         Diese Form initialisiert alle internen Tabellen
*         der logischen GL-Datenbanken.
*-------------------------------------------------------------------*

FORM  CLEAR_UPDATE_TABLES.
  CLEAR TAB_COFIS . REFRESH TAB_COFIS .
  CLEAR TAB_COFIS_ADD . REFRESH TAB_COFIS_ADD .
  CLEAR TAB_ECMCA . REFRESH TAB_ECMCA .
  CLEAR TAB_ECMCA_ADD . REFRESH TAB_ECMCA_ADD .
  CLEAR TAB_FAGLFLEXA . REFRESH TAB_FAGLFLEXA .
  CLEAR TAB_FAGLFLEXA_ADD . REFRESH TAB_FAGLFLEXA_ADD .
  CLEAR TAB_FBICRC001A . REFRESH TAB_FBICRC001A .
  CLEAR TAB_FBICRC001A_ADD . REFRESH TAB_FBICRC001A_ADD .
  CLEAR TAB_FBICRC002A . REFRESH TAB_FBICRC002A .
  CLEAR TAB_FBICRC002A_ADD . REFRESH TAB_FBICRC002A_ADD .
  CLEAR TAB_FILCA . REFRESH TAB_FILCA .
  CLEAR TAB_FILCA_ADD . REFRESH TAB_FILCA_ADD .
  CLEAR TAB_FMGLFLEXA . REFRESH TAB_FMGLFLEXA .
  CLEAR TAB_FMGLFLEXA_ADD . REFRESH TAB_FMGLFLEXA_ADD .
  CLEAR TAB_GLFUNCA . REFRESH TAB_GLFUNCA .
  CLEAR TAB_GLFUNCA_ADD . REFRESH TAB_GLFUNCA_ADD .
  CLEAR TAB_GLPCA . REFRESH TAB_GLPCA .
  CLEAR TAB_GLPCA_ADD . REFRESH TAB_GLPCA_ADD .
  CLEAR TAB_GLS0 . REFRESH TAB_GLS0 .
  CLEAR TAB_GLS0_ADD . REFRESH TAB_GLS0_ADD .
  CLEAR TAB_GLS1 . REFRESH TAB_GLS1 .
  CLEAR TAB_GLS1_ADD . REFRESH TAB_GLS1_ADD .
  CLEAR TAB_GLS2 . REFRESH TAB_GLS2 .
  CLEAR TAB_GLS2_ADD . REFRESH TAB_GLS2_ADD .
  CLEAR TAB_GLS3 . REFRESH TAB_GLS3 .
  CLEAR TAB_GLS3_ADD . REFRESH TAB_GLS3_ADD .
  CLEAR TAB_GLSPC . REFRESH TAB_GLSPC .
  CLEAR TAB_GLSPC_ADD . REFRESH TAB_GLSPC_ADD .
  CLEAR TAB_GMAVCA . REFRESH TAB_GMAVCA .
  CLEAR TAB_GMAVCA_ADD . REFRESH TAB_GMAVCA_ADD .
  CLEAR TAB_JVPSC01A . REFRESH TAB_JVPSC01A .
  CLEAR TAB_JVPSC01A_ADD . REFRESH TAB_JVPSC01A_ADD .
  CLEAR TAB_JVSO1 . REFRESH TAB_JVSO1 .
  CLEAR TAB_JVSO1_ADD . REFRESH TAB_JVSO1_ADD .
  CLEAR TAB_JVSO2 . REFRESH TAB_JVSO2 .
  CLEAR TAB_JVSO2_ADD . REFRESH TAB_JVSO2_ADD .
  CLEAR TAB_TRACTSLA . REFRESH TAB_TRACTSLA .
  CLEAR TAB_TRACTSLA_ADD . REFRESH TAB_TRACTSLA_ADD .
  CLEAR TAB_GLU1.          REFRESH TAB_GLU1.
  CLEAR TAB_GLU1_ADD.      REFRESH TAB_GLU1_ADD.
  CLEAR USED.              REFRESH USED.
ENDFORM.


*-------------------------------------------------------------------*
*         FORM CLEAR_BS_PL_TABLES
*-------------------------------------------------------------------*

FORM CLEAR_BS_PL_TABLES.
    CLEAR T_BS_COFIS . REFRESH T_BS_COFIS .
    CLEAR T_PL_COFIS . REFRESH T_PL_COFIS .
    CLEAR T_BS_ECMCA . REFRESH T_BS_ECMCA .
    CLEAR T_PL_ECMCA . REFRESH T_PL_ECMCA .
    CLEAR T_BS_FAGLFLEXA . REFRESH T_BS_FAGLFLEXA .
    CLEAR T_PL_FAGLFLEXA . REFRESH T_PL_FAGLFLEXA .
    CLEAR T_BS_FBICRC001A . REFRESH T_BS_FBICRC001A .
    CLEAR T_PL_FBICRC001A . REFRESH T_PL_FBICRC001A .
    CLEAR T_BS_FBICRC002A . REFRESH T_BS_FBICRC002A .
    CLEAR T_PL_FBICRC002A . REFRESH T_PL_FBICRC002A .
    CLEAR T_BS_FILCA . REFRESH T_BS_FILCA .
    CLEAR T_PL_FILCA . REFRESH T_PL_FILCA .
    CLEAR T_BS_FMGLFLEXA . REFRESH T_BS_FMGLFLEXA .
    CLEAR T_PL_FMGLFLEXA . REFRESH T_PL_FMGLFLEXA .
    CLEAR T_BS_GLFUNCA . REFRESH T_BS_GLFUNCA .
    CLEAR T_PL_GLFUNCA . REFRESH T_PL_GLFUNCA .
    CLEAR T_BS_GLPCA . REFRESH T_BS_GLPCA .
    CLEAR T_PL_GLPCA . REFRESH T_PL_GLPCA .
    CLEAR T_BS_GLS0 . REFRESH T_BS_GLS0 .
    CLEAR T_PL_GLS0 . REFRESH T_PL_GLS0 .
    CLEAR T_BS_GLS1 . REFRESH T_BS_GLS1 .
    CLEAR T_PL_GLS1 . REFRESH T_PL_GLS1 .
    CLEAR T_BS_GLS2 . REFRESH T_BS_GLS2 .
    CLEAR T_PL_GLS2 . REFRESH T_PL_GLS2 .
    CLEAR T_BS_GLS3 . REFRESH T_BS_GLS3 .
    CLEAR T_PL_GLS3 . REFRESH T_PL_GLS3 .
    CLEAR T_BS_GLSPC . REFRESH T_BS_GLSPC .
    CLEAR T_PL_GLSPC . REFRESH T_PL_GLSPC .
    CLEAR T_BS_GMAVCA . REFRESH T_BS_GMAVCA .
    CLEAR T_PL_GMAVCA . REFRESH T_PL_GMAVCA .
    CLEAR T_BS_JVPSC01A . REFRESH T_BS_JVPSC01A .
    CLEAR T_PL_JVPSC01A . REFRESH T_PL_JVPSC01A .
    CLEAR T_BS_JVSO1 . REFRESH T_BS_JVSO1 .
    CLEAR T_PL_JVSO1 . REFRESH T_PL_JVSO1 .
    CLEAR T_BS_JVSO2 . REFRESH T_BS_JVSO2 .
    CLEAR T_PL_JVSO2 . REFRESH T_PL_JVSO2 .
    CLEAR T_BS_TRACTSLA . REFRESH T_BS_TRACTSLA .
    CLEAR T_PL_TRACTSLA . REFRESH T_PL_TRACTSLA .
ENDFORM.


*eject
*-------------------------------------------------------------------*
*
*-------------------------------------------------------------------*
*         FORM UPDATE_TABLES_SAVOR
*         Diese Form schreibt die internen Tabellen mit
*         dem erzeugten GLU1-Satz fort.
*         Vorher wird der aufgebaute Satz auf gültige
*         Objektnummern überprüft und evtl. verworfen.
*-------------------------------------------------------------------*

FORM  UPDATE_TABLES_SAVOR USING UTS_TAB.
  CASE UTS_TAB.

*-------------------------------------------------------------------*
*         Summentabelle: COFIT                           Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'COFIT                         '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_COFIS                         . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_COFIS_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_COFIS_ADD-RPMAX.
      MOVE '000'              TO TAB_COFIS_ADD-OFFSET.
      MOVE 'X'                TO TAB_COFIS_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'COFIS ' TAB_COFIS .
      ENDIF.
      APPEND TAB_COFIS                         .
      APPEND TAB_COFIS_ADD.
      USED-TAB   = 'COFIS                         '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: ECMCT                           Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'ECMCT                         '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_ECMCA                         . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_ECMCA_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_ECMCA_ADD-RPMAX.
      MOVE '000'              TO TAB_ECMCA_ADD-OFFSET.
      MOVE 'X'                TO TAB_ECMCA_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'ECMCA ' TAB_ECMCA .
      ENDIF.
      APPEND TAB_ECMCA                         .
      APPEND TAB_ECMCA_ADD.
      USED-TAB   = 'ECMCA                         '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: FAGLFLEXT                       Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'FAGLFLEXT                     '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_FAGLFLEXA                     . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_FAGLFLEXA_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_FAGLFLEXA_ADD-RPMAX.
      MOVE '000'              TO TAB_FAGLFLEXA_ADD-OFFSET.
      MOVE 'X'                TO TAB_FAGLFLEXA_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'FAGLFLEXA ' TAB_FAGLFLEXA .
      ENDIF.
      APPEND TAB_FAGLFLEXA                     .
      APPEND TAB_FAGLFLEXA_ADD.
      USED-TAB   = 'FAGLFLEXA                     '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: FBICRC001T                      Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'FBICRC001T                    '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_FBICRC001A                    . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_FBICRC001A_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_FBICRC001A_ADD-RPMAX.
      MOVE '000'              TO TAB_FBICRC001A_ADD-OFFSET.
      MOVE 'X'                TO TAB_FBICRC001A_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'FBICRC001A ' TAB_FBICRC001A .
      ENDIF.
      APPEND TAB_FBICRC001A                    .
      APPEND TAB_FBICRC001A_ADD.
      USED-TAB   = 'FBICRC001A                    '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: FBICRC002T                      Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'FBICRC002T                    '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_FBICRC002A                    . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_FBICRC002A_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_FBICRC002A_ADD-RPMAX.
      MOVE '000'              TO TAB_FBICRC002A_ADD-OFFSET.
      MOVE 'X'                TO TAB_FBICRC002A_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'FBICRC002A ' TAB_FBICRC002A .
      ENDIF.
      APPEND TAB_FBICRC002A                    .
      APPEND TAB_FBICRC002A_ADD.
      USED-TAB   = 'FBICRC002A                    '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: FILCT                           Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'FILCT                         '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_FILCA.            "#EC CI_FLDEXT_OK[2610650]             .
      MOVE GD_WRITE_SI      TO TAB_FILCA_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_FILCA_ADD-RPMAX.
      MOVE '000'              TO TAB_FILCA_ADD-OFFSET.
      MOVE 'X'                TO TAB_FILCA_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'FILCA ' TAB_FILCA .
      ENDIF.
      APPEND TAB_FILCA                         .
      APPEND TAB_FILCA_ADD.
      USED-TAB   = 'FILCA                         '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: FMGLFLEXT                       Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'FMGLFLEXT                     '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_FMGLFLEXA                     . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_FMGLFLEXA_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_FMGLFLEXA_ADD-RPMAX.
      MOVE '000'              TO TAB_FMGLFLEXA_ADD-OFFSET.
      MOVE 'X'                TO TAB_FMGLFLEXA_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'FMGLFLEXA ' TAB_FMGLFLEXA .
      ENDIF.
      APPEND TAB_FMGLFLEXA                     .
      APPEND TAB_FMGLFLEXA_ADD.
      USED-TAB   = 'FMGLFLEXA                     '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: GLFUNCT                         Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'GLFUNCT                       '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_GLFUNCA                       . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_GLFUNCA_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_GLFUNCA_ADD-RPMAX.
      MOVE '000'              TO TAB_GLFUNCA_ADD-OFFSET.
      MOVE 'X'                TO TAB_GLFUNCA_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'GLFUNCA ' TAB_GLFUNCA .
      ENDIF.
      APPEND TAB_GLFUNCA                       .
      APPEND TAB_GLFUNCA_ADD.
      USED-TAB   = 'GLFUNCA                       '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: GLPCT                           Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'GLPCT                         '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_GLPCA                         . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_GLPCA_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_GLPCA_ADD-RPMAX.
      MOVE '000'              TO TAB_GLPCA_ADD-OFFSET.
      MOVE 'X'                TO TAB_GLPCA_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'GLPCA ' TAB_GLPCA .
      ENDIF.
      APPEND TAB_GLPCA                         .
      APPEND TAB_GLPCA_ADD.
      USED-TAB   = 'GLPCA                         '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: GLT0                            Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'GLT0                          '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_GLS0                          . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_GLS0_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_GLS0_ADD-RPMAX.
      MOVE '000'              TO TAB_GLS0_ADD-OFFSET.
      MOVE 'X'                TO TAB_GLS0_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'GLS0 ' TAB_GLS0 .
      ENDIF.
      APPEND TAB_GLS0                          .
      APPEND TAB_GLS0_ADD.
      USED-TAB   = 'GLS0                          '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: GLT1                            Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'GLT1                          '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_GLS1                          . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_GLS1_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_GLS1_ADD-RPMAX.
      MOVE '000'              TO TAB_GLS1_ADD-OFFSET.
      MOVE 'X'                TO TAB_GLS1_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'GLS1 ' TAB_GLS1 .
      ENDIF.
      APPEND TAB_GLS1                          .
      APPEND TAB_GLS1_ADD.
      USED-TAB   = 'GLS1                          '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: GLT2                            Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'GLT2                          '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_GLS2                          . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_GLS2_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_GLS2_ADD-RPMAX.
      MOVE '000'              TO TAB_GLS2_ADD-OFFSET.
      MOVE 'X'                TO TAB_GLS2_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'GLS2 ' TAB_GLS2 .
      ENDIF.
      APPEND TAB_GLS2                          .
      APPEND TAB_GLS2_ADD.
      USED-TAB   = 'GLS2                          '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: GLT3                            Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'GLT3                          '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_GLS3                          . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_GLS3_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_GLS3_ADD-RPMAX.
      MOVE '000'              TO TAB_GLS3_ADD-OFFSET.
      MOVE 'X'                TO TAB_GLS3_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'GLS3 ' TAB_GLS3 .
      ENDIF.
      APPEND TAB_GLS3                          .
      APPEND TAB_GLS3_ADD.
      USED-TAB   = 'GLS3                          '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: GLTPC                           Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'GLTPC                         '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_GLSPC                         . "#EC CI_FLDEXT_OK[2215424]
      MOVE GD_WRITE_SI      TO TAB_GLSPC_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_GLSPC_ADD-RPMAX.
      MOVE '000'              TO TAB_GLSPC_ADD-OFFSET.
      MOVE 'X'                TO TAB_GLSPC_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'GLSPC ' TAB_GLSPC .
      ENDIF.
      APPEND TAB_GLSPC                         .
      APPEND TAB_GLSPC_ADD.
      USED-TAB   = 'GLSPC                         '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: GMAVCT                          Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'GMAVCT                        '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_GMAVCA                        . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_GMAVCA_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_GMAVCA_ADD-RPMAX.
      MOVE '000'              TO TAB_GMAVCA_ADD-OFFSET.
      MOVE 'X'                TO TAB_GMAVCA_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'GMAVCA ' TAB_GMAVCA .
      ENDIF.
      APPEND TAB_GMAVCA                        .
      APPEND TAB_GMAVCA_ADD.
      USED-TAB   = 'GMAVCA                        '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: JVPSC01T                        Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'JVPSC01T                      '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_JVPSC01A                      . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_JVPSC01A_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_JVPSC01A_ADD-RPMAX.
      MOVE '000'              TO TAB_JVPSC01A_ADD-OFFSET.
      MOVE 'X'                TO TAB_JVPSC01A_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'JVPSC01A ' TAB_JVPSC01A .
      ENDIF.
      APPEND TAB_JVPSC01A                      .
      APPEND TAB_JVPSC01A_ADD.
      USED-TAB   = 'JVPSC01A                      '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: JVTO1                           Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'JVTO1                         '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_JVSO1                         . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_JVSO1_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_JVSO1_ADD-RPMAX.
      MOVE '000'              TO TAB_JVSO1_ADD-OFFSET.
      MOVE 'X'                TO TAB_JVSO1_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'JVSO1 ' TAB_JVSO1 .
      ENDIF.
      APPEND TAB_JVSO1                         .
      APPEND TAB_JVSO1_ADD.
      USED-TAB   = 'JVSO1                         '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: JVTO2                           Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'JVTO2                         '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_JVSO2                         . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_JVSO2_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_JVSO2_ADD-RPMAX.
      MOVE '000'              TO TAB_JVSO2_ADD-OFFSET.
      MOVE 'X'                TO TAB_JVSO2_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'JVSO2 ' TAB_JVSO2 .
      ENDIF.
      APPEND TAB_JVSO2                         .
      APPEND TAB_JVSO2_ADD.
      USED-TAB   = 'JVSO2                         '.
      COLLECT USED.

*-------------------------------------------------------------------*
*         Summentabelle: TRACTSLT                        Einzelposten:
*-------------------------------------------------------------------*
    WHEN 'TRACTSLT                      '.
*         Sind Einzelposten zu schreiben?
*         (bei Saldovortrag keine GLSI)
      MOVE-CORRESPONDING GLU1 TO TAB_TRACTSLA                      . "#EC CI_FLDEXT_OK[2610650]
      MOVE GD_WRITE_SI      TO TAB_TRACTSLA_ADD-GLSIP.
      MOVE T800A-FLDGR        TO TAB_TRACTSLA_ADD-RPMAX.
      MOVE '000'              TO TAB_TRACTSLA_ADD-OFFSET.
      MOVE 'X'                TO TAB_TRACTSLA_ADD-POST.
      IF T800A-OBJCHECK NE SPACE.
        PERFORM VALIDATE_RECORD
          USING 'TRACTSLA ' TAB_TRACTSLA .
      ENDIF.
      APPEND TAB_TRACTSLA                      .
      APPEND TAB_TRACTSLA_ADD.
      USED-TAB   = 'TRACTSLA                      '.
      COLLECT USED.

  ENDCASE.
ENDFORM.


*eject
*-------------------------------------------------------------------*
*
*-------------------------------------------------------------------*
*         FORM UPDATE_DB
*         Diese Form ruft ein Programm zum Update der logischen
*         GL-Datenbanken auf.
*-------------------------------------------------------------------*

FORM  UPDATE_DB.
  CLEAR  TAB_ACTIVITY.
  APPEND TAB_ACTIVITY.
  SET UPDATE TASK LOCAL.

  LOOP AT USED_V1 WHERE PROGROUP = 'A'.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'G_GLDB_POSTING_A' IN UPDATE TASK
                   TABLES INT_USED          =   USED_V1
                          INT_ACTIVITY      =   TAB_ACTIVITY
                          INT_COFIP = TAB_COFIP
                          INT_COFIP_ADD = TAB_COFIP_ADD
                          INT_COFIS = TAB_COFIS
                          INT_COFIS_ADD = TAB_COFIS_ADD
                          INT_ECMCA = TAB_ECMCA
                          INT_ECMCA_ADD = TAB_ECMCA_ADD
                          INT_FAGLFLEXA = TAB_FAGLFLEXA
                          INT_FAGLFLEXA_ADD = TAB_FAGLFLEXA_ADD
                          INT_FAGLFLEXP = TAB_FAGLFLEXP
                          INT_FAGLFLEXP_ADD = TAB_FAGLFLEXP_ADD
                          INT_FBICRC001A = TAB_FBICRC001A
                          INT_FBICRC001A_ADD = TAB_FBICRC001A_ADD
                          INT_FBICRC001P = TAB_FBICRC001P
                          INT_FBICRC001P_ADD = TAB_FBICRC001P_ADD
                          INT_FBICRC002A = TAB_FBICRC002A
                          INT_FBICRC002A_ADD = TAB_FBICRC002A_ADD
                          INT_FBICRC002P = TAB_FBICRC002P
                          INT_FBICRC002P_ADD = TAB_FBICRC002P_ADD
                          INT_FILCA = TAB_FILCA
                          INT_FILCA_ADD = TAB_FILCA_ADD
                          INT_FILCP = TAB_FILCP
                          INT_FILCP_ADD = TAB_FILCP_ADD
                          INT_FMGLFLEXA = TAB_FMGLFLEXA
                          INT_FMGLFLEXA_ADD = TAB_FMGLFLEXA_ADD
                          INT_FMGLFLEXP = TAB_FMGLFLEXP
                          INT_FMGLFLEXP_ADD = TAB_FMGLFLEXP_ADD
                          INT_GLP0 = TAB_GLP0
                          INT_GLP0_ADD = TAB_GLP0_ADD
                          INT_GLPCA = TAB_GLPCA
                          INT_GLPCA_ADD = TAB_GLPCA_ADD
                          INT_GLPCP = TAB_GLPCP
                          INT_GLPCP_ADD = TAB_GLPCP_ADD
                          INT_GLS0 = TAB_GLS0
                          INT_GLS0_ADD = TAB_GLS0_ADD
                          INT_GLS3 = TAB_GLS3
                          INT_GLS3_ADD = TAB_GLS3_ADD
                          INT_GMAVCA = TAB_GMAVCA
                          INT_GMAVCA_ADD = TAB_GMAVCA_ADD
                          INT_GMAVCP = TAB_GMAVCP
                          INT_GMAVCP_ADD = TAB_GMAVCP_ADD
                          INT_JVPO1 = TAB_JVPO1
                          INT_JVPO1_ADD = TAB_JVPO1_ADD
                          INT_JVPSC01A = TAB_JVPSC01A
                          INT_JVPSC01A_ADD = TAB_JVPSC01A_ADD
                          INT_JVPSC01P = TAB_JVPSC01P
                          INT_JVPSC01P_ADD = TAB_JVPSC01P_ADD
                          INT_JVSO1 = TAB_JVSO1
                          INT_JVSO1_ADD = TAB_JVSO1_ADD
                          INT_JVSO2 = TAB_JVSO2
                          INT_JVSO2_ADD = TAB_JVSO2_ADD
                          INT_TRACTSLA = TAB_TRACTSLA
                          INT_TRACTSLA_ADD = TAB_TRACTSLA_ADD
                          INT_TRACTSLP = TAB_TRACTSLP
                          INT_TRACTSLP_ADD = TAB_TRACTSLP_ADD .

  ENDIF.
  LOOP AT USED_V1 WHERE PROGROUP = '1'.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'G_GLDB_POSTING_1' IN UPDATE TASK
                   TABLES INT_USED          =   USED_V1
                          INT_ACTIVITY      =   TAB_ACTIVITY
                          INT_GLP1 = TAB_GLP1
                          INT_GLP1_ADD = TAB_GLP1_ADD
                          INT_GLP2 = TAB_GLP2
                          INT_GLP2_ADD = TAB_GLP2_ADD
                          INT_GLPPC = TAB_GLPPC
                          INT_GLPPC_ADD = TAB_GLPPC_ADD
                          INT_GLS1 = TAB_GLS1
                          INT_GLS1_ADD = TAB_GLS1_ADD
                          INT_GLS2 = TAB_GLS2
                          INT_GLS2_ADD = TAB_GLS2_ADD
                          INT_GLSPC = TAB_GLSPC
                          INT_GLSPC_ADD = TAB_GLSPC_ADD
                          INT_GLU1          =   TAB_GLU1
                          INT_GLU1_ADD      =   TAB_GLU1_ADD.

  ENDIF.
  LOOP AT USED_V1 WHERE PROGROUP = '3'.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'G_GLDB_POSTING_3' IN UPDATE TASK
                   TABLES INT_USED          =   USED_V1
                          INT_ACTIVITY      =   TAB_ACTIVITY
                          INT_GLFUNCA = TAB_GLFUNCA
                          INT_GLFUNCA_ADD = TAB_GLFUNCA_ADD
                          INT_GLFUNCP = TAB_GLFUNCP
                          INT_GLFUNCP_ADD = TAB_GLFUNCP_ADD
                          INT_GLU1          =   TAB_GLU1
                          INT_GLU1_ADD      =   TAB_GLU1_ADD.

  ENDIF.
ENDFORM.

*-------------------------------------------------------------------*
*         FORM COLLECT_T_BS
*         --> Bilanzkonten in T_BS_xxxxx
*             GuV-Konten in T_PL_xxxxx
*-------------------------------------------------------------------*
FORM COLLECT_T_BS   USING BS_GLU1 LIKE GLU1
                         BS_TAB  LIKE T800A-TAB.

  CASE BS_TAB.

    WHEN 'COFIT                         '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_COFIS                         .
      COLLECT T_BS_COFIS                         .
    WHEN 'ECMCT                         '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_ECMCA                         .
      COLLECT T_BS_ECMCA                         .
    WHEN 'FAGLFLEXT                     '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_FAGLFLEXA                     .
      COLLECT T_BS_FAGLFLEXA                     .
    WHEN 'FBICRC001T                    '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_FBICRC001A                    .
      COLLECT T_BS_FBICRC001A                    .
    WHEN 'FBICRC002T                    '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_FBICRC002A                    .
      COLLECT T_BS_FBICRC002A                    .
    WHEN 'FILCT                         '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_FILCA                         .
      COLLECT T_BS_FILCA                         .
    WHEN 'FMGLFLEXT                     '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_FMGLFLEXA                     .
      COLLECT T_BS_FMGLFLEXA                     .
    WHEN 'GLFUNCT                       '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_GLFUNCA                       .
      COLLECT T_BS_GLFUNCA                       .
    WHEN 'GLPCT                         '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_GLPCA                         .
      COLLECT T_BS_GLPCA                         .
    WHEN 'GLT0                          '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_GLS0                          .
      COLLECT T_BS_GLS0                          .
    WHEN 'GLT1                          '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_GLS1                          .
      COLLECT T_BS_GLS1                          .
    WHEN 'GLT2                          '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_GLS2                          .
      COLLECT T_BS_GLS2                          .
    WHEN 'GLT3                          '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_GLS3                          .
      COLLECT T_BS_GLS3                          .
    WHEN 'GLTPC                         '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_GLSPC                         .
      COLLECT T_BS_GLSPC                         .
    WHEN 'GMAVCT                        '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_GMAVCA                        .
      COLLECT T_BS_GMAVCA                        .
    WHEN 'JVPSC01T                      '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_JVPSC01A                      .
      COLLECT T_BS_JVPSC01A                      .
    WHEN 'JVTO1                         '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_JVSO1                         .
      COLLECT T_BS_JVSO1                         .
    WHEN 'JVTO2                         '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_JVSO2                         .
      COLLECT T_BS_JVSO2                         .
    WHEN 'TRACTSLT                      '.
      MOVE-CORRESPONDING BS_GLU1 TO T_BS_TRACTSLA                      .
      COLLECT T_BS_TRACTSLA                      .
  ENDCASE.

ENDFORM.


*-------------------------------------------------------------------*
*         FORM COLLECT_T_PL
*         --> Bilanzkonten in T_BS_xxxxx
*             GuV-Konten in T_PL_xxxxx
*-------------------------------------------------------------------*
FORM COLLECT_T_PL   USING PL_GLU1 LIKE GLU1
                         PL_TAB  LIKE T800A-TAB.

  CASE PL_TAB.

    WHEN 'COFIT                         '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_COFIS                         .
      COLLECT T_PL_COFIS                         .
    WHEN 'ECMCT                         '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_ECMCA                         .
      COLLECT T_PL_ECMCA                         .
    WHEN 'FAGLFLEXT                     '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_FAGLFLEXA                     .
      COLLECT T_PL_FAGLFLEXA                     .
    WHEN 'FBICRC001T                    '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_FBICRC001A                    .
      COLLECT T_PL_FBICRC001A                    .
    WHEN 'FBICRC002T                    '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_FBICRC002A                    .
      COLLECT T_PL_FBICRC002A                    .
    WHEN 'FILCT                         '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_FILCA                         .
      COLLECT T_PL_FILCA                         .
    WHEN 'FMGLFLEXT                     '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_FMGLFLEXA                     .
      COLLECT T_PL_FMGLFLEXA                     .
    WHEN 'GLFUNCT                       '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_GLFUNCA                       .
      COLLECT T_PL_GLFUNCA                       .
    WHEN 'GLPCT                         '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_GLPCA                         .
      COLLECT T_PL_GLPCA                         .
    WHEN 'GLT0                          '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_GLS0                          .
      COLLECT T_PL_GLS0                          .
    WHEN 'GLT1                          '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_GLS1                          .
      COLLECT T_PL_GLS1                          .
    WHEN 'GLT2                          '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_GLS2                          .
      COLLECT T_PL_GLS2                          .
    WHEN 'GLT3                          '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_GLS3                          .
      COLLECT T_PL_GLS3                          .
    WHEN 'GLTPC                         '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_GLSPC                         .
      COLLECT T_PL_GLSPC                         .
    WHEN 'GMAVCT                        '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_GMAVCA                        .
      COLLECT T_PL_GMAVCA                        .
    WHEN 'JVPSC01T                      '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_JVPSC01A                      .
      COLLECT T_PL_JVPSC01A                      .
    WHEN 'JVTO1                         '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_JVSO1                         .
      COLLECT T_PL_JVSO1                         .
    WHEN 'JVTO2                         '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_JVSO2                         .
      COLLECT T_PL_JVSO2                         .
    WHEN 'TRACTSLT                      '.
      MOVE-CORRESPONDING PL_GLU1 TO T_PL_TRACTSLA                      .
      COLLECT T_PL_TRACTSLA                      .
  ENDCASE.

ENDFORM.


*-------------------------------------------------------------------*
*         FORM LOOP_T_BS
*         --> Übergabe Bilanzkonten an VB-Tabellen
*-------------------------------------------------------------------*
FORM LOOP_T_BS      USING BS_TAB  LIKE T800A-TAB.

  CASE BS_TAB.

    WHEN 'COFIT                         '.
      LOOP AT T_BS_COFIS                         .
        MOVE-CORRESPONDING T_BS_COFIS                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'ECMCT                         '.
      LOOP AT T_BS_ECMCA                         .
        MOVE-CORRESPONDING T_BS_ECMCA                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'FAGLFLEXT                     '.
      LOOP AT T_BS_FAGLFLEXA                     .
        MOVE-CORRESPONDING T_BS_FAGLFLEXA                      TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'FBICRC001T                    '.
      LOOP AT T_BS_FBICRC001A                    .
        MOVE-CORRESPONDING T_BS_FBICRC001A                     TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'FBICRC002T                    '.
      LOOP AT T_BS_FBICRC002A                    .
        MOVE-CORRESPONDING T_BS_FBICRC002A                     TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'FILCT                         '.
      LOOP AT T_BS_FILCA                         .
        MOVE-CORRESPONDING T_BS_FILCA                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'FMGLFLEXT                     '.
      LOOP AT T_BS_FMGLFLEXA                     .
        MOVE-CORRESPONDING T_BS_FMGLFLEXA                      TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'GLFUNCT                       '.
      LOOP AT T_BS_GLFUNCA                       .
        MOVE-CORRESPONDING T_BS_GLFUNCA                        TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'GLPCT                         '.
      LOOP AT T_BS_GLPCA                         .
        MOVE-CORRESPONDING T_BS_GLPCA                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'GLT0                          '.
      LOOP AT T_BS_GLS0                          .
        MOVE-CORRESPONDING T_BS_GLS0                           TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'GLT1                          '.
      LOOP AT T_BS_GLS1                          .
        MOVE-CORRESPONDING T_BS_GLS1                           TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'GLT2                          '.
      LOOP AT T_BS_GLS2                          .
        MOVE-CORRESPONDING T_BS_GLS2                           TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'GLT3                          '.
      LOOP AT T_BS_GLS3                          .
        MOVE-CORRESPONDING T_BS_GLS3                           TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'GLTPC                         '.
      LOOP AT T_BS_GLSPC                         .
        MOVE-CORRESPONDING T_BS_GLSPC                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'GMAVCT                        '.
      LOOP AT T_BS_GMAVCA                        .
        MOVE-CORRESPONDING T_BS_GMAVCA                         TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'JVPSC01T                      '.
      LOOP AT T_BS_JVPSC01A                      .
        MOVE-CORRESPONDING T_BS_JVPSC01A                       TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'JVTO1                         '.
      LOOP AT T_BS_JVSO1                         .
        MOVE-CORRESPONDING T_BS_JVSO1                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'JVTO2                         '.
      LOOP AT T_BS_JVSO2                         .
        MOVE-CORRESPONDING T_BS_JVSO2                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
    WHEN 'TRACTSLT                      '.
      LOOP AT T_BS_TRACTSLA                      .
        MOVE-CORRESPONDING T_BS_TRACTSLA                       TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 'X'.
      ENDLOOP.
  ENDCASE.

ENDFORM.


*-------------------------------------------------------------------*
*         FORM LOOP_T_PL
*         --> Übergabe GuVKonten an VB-Tabellen
*-------------------------------------------------------------------*
FORM LOOP_T_PL      USING PL_TAB  LIKE T800A-TAB.

  CASE PL_TAB.

    WHEN 'COFIT                         '.
      LOOP AT T_PL_COFIS                         .
        MOVE-CORRESPONDING T_PL_COFIS                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'ECMCT                         '.
      LOOP AT T_PL_ECMCA                         .
        MOVE-CORRESPONDING T_PL_ECMCA                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'FAGLFLEXT                     '.
      LOOP AT T_PL_FAGLFLEXA                     .
        MOVE-CORRESPONDING T_PL_FAGLFLEXA                      TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'FBICRC001T                    '.
      LOOP AT T_PL_FBICRC001A                    .
        MOVE-CORRESPONDING T_PL_FBICRC001A                     TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'FBICRC002T                    '.
      LOOP AT T_PL_FBICRC002A                    .
        MOVE-CORRESPONDING T_PL_FBICRC002A                     TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'FILCT                         '.
      LOOP AT T_PL_FILCA                         .
        MOVE-CORRESPONDING T_PL_FILCA                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'FMGLFLEXT                     '.
      LOOP AT T_PL_FMGLFLEXA                     .
        MOVE-CORRESPONDING T_PL_FMGLFLEXA                      TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'GLFUNCT                       '.
      LOOP AT T_PL_GLFUNCA                       .
        MOVE-CORRESPONDING T_PL_GLFUNCA                        TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'GLPCT                         '.
      LOOP AT T_PL_GLPCA                         .
        MOVE-CORRESPONDING T_PL_GLPCA                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'GLT0                          '.
      LOOP AT T_PL_GLS0                          .
        MOVE-CORRESPONDING T_PL_GLS0                           TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'GLT1                          '.
      LOOP AT T_PL_GLS1                          .
        MOVE-CORRESPONDING T_PL_GLS1                           TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'GLT2                          '.
      LOOP AT T_PL_GLS2                          .
        MOVE-CORRESPONDING T_PL_GLS2                           TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'GLT3                          '.
      LOOP AT T_PL_GLS3                          .
        MOVE-CORRESPONDING T_PL_GLS3                           TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'GLTPC                         '.
      LOOP AT T_PL_GLSPC                         .
        MOVE-CORRESPONDING T_PL_GLSPC                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'GMAVCT                        '.
      LOOP AT T_PL_GMAVCA                        .
        MOVE-CORRESPONDING T_PL_GMAVCA                         TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'JVPSC01T                      '.
      LOOP AT T_PL_JVPSC01A                      .
        MOVE-CORRESPONDING T_PL_JVPSC01A                       TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'JVTO1                         '.
      LOOP AT T_PL_JVSO1                         .
        MOVE-CORRESPONDING T_PL_JVSO1                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'JVTO2                         '.
      LOOP AT T_PL_JVSO2                         .
        MOVE-CORRESPONDING T_PL_JVSO2                          TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
    WHEN 'TRACTSLT                      '.
      LOOP AT T_PL_TRACTSLA                      .
        MOVE-CORRESPONDING T_PL_TRACTSLA                       TO GLU1.
        PERFORM RECORD_END_PROCESS USING GLU1 ' '.
      ENDLOOP.
  ENDCASE.

ENDFORM.
