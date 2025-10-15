"Name: \TY:CL_WINDOW_MM\ME:HANDLE_EVENT\SE:END\EI
ENHANCEMENT 0 Z_MOSTRA_CONTA.
*
  DATA: WL_ZGLT041 TYPE ZGLT041,
        TL_ZGLT041 TYPE TABLE OF ZGLT041,
        WL_ZGLT039 TYPE ZGLT039,
        WL_NAME    TYPE THEAD-TDNAME,
        TG_TEXTO   TYPE TABLE OF TLINE,
        WG_TEXTO   TYPE TLINE.


 DATA: TL_TEXTO TYPE CATSXT_LONGTEXT_ITAB,
       WL_TEXTO TYPE LINE OF CATSXT_LONGTEXT_ITAB.

 DATA:  CHAVE1(40),
        CHAVE2(40).

 FIELD-SYMBOLS: <SAKNR> TYPE ANY,
                <BUKRS> TYPE ANY.


IF L_CURSOR-METAFIELD = 551. "conta single
  ASSIGN ('(SAPLMEACCTVI)MEACCT1100-SAKTO') TO <SAKNR>.
  if sy-tcode+0(3) = 'ME5'.
      ASSIGN ('(SAPLMEACCTVI)MEACCT1200-BUKRS') TO <BUKRS>.
  else.
      ASSIGN ('(SAPLMEGUI)MEPO1222-BUKRS') TO <BUKRS>.
  endif.
  IF <SAKNR> IS ASSIGNED AND <BUKRS> IS ASSIGNED.
   SELECT  *
     FROM ZGLT041
     INTO TABLE TL_ZGLT041
     WHERE BUKRS = <BUKRS>
     AND   SAKNR = <SAKNR>.
    LOOP AT TL_ZGLT041 INTO WL_ZGLT041.
       REFRESH: TL_TEXTO, TG_TEXTO.
       select SINGLE *
         from ZGLT039
         into WL_ZGLT039
         where CODIGO   = WL_ZGLT041-COD_CLAS_BAL
         and   COD_NOTA = WL_ZGLT041-COD_CLAS_NOT2.

       CONCATENATE WL_ZGLT039-CODIGO '-' WL_ZGLT039-DESCR INTO CHAVE1.
       CONCATENATE WL_ZGLT039-COD_NOTA '-' WL_ZGLT039-DESCR_NOTA INTO CHAVE2.
       CONCATENATE WL_ZGLT041-BUKRS WL_ZGLT041-SAKNR CHAVE1 CHAVE2 INTO WL_NAME.
       CALL FUNCTION 'READ_TEXT'
         EXPORTING
           ID                      = 'ZCRI'
           LANGUAGE                = SY-LANGU
           NAME                    = WL_NAME
           OBJECT                  = 'ZCRITERIO'
         TABLES
           LINES                   = TG_TEXTO
         EXCEPTIONS
           ID                      = 1
           LANGUAGE                = 2
           NAME                    = 3
           NOT_FOUND               = 4
           OBJECT                  = 5
           REFERENCE_CHECK         = 6
           WRONG_ACCESS_TO_ARCHIVE = 7
           OTHERS                  = 8.
       "
       LOOP AT TG_TEXTO INTO WG_TEXTO.
        MOVE: WG_TEXTO-TDLINE   TO WL_TEXTO.

        APPEND WL_TEXTO TO TL_TEXTO.
        CLEAR: WL_TEXTO.
       ENDLOOP.
       IF TL_TEXTO[] IS NOT INITIAL.
          CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
            EXPORTING
              IM_TITLE = 'Critérios Reconciliação'
            CHANGING
              CH_TEXT  = TL_TEXTO.
           EXIT.
       ENDIF.
    ENDLOOP.
    REFRESH: TL_TEXTO, TG_TEXTO.
  ENDIF.
 ENDIF.
ENDENHANCEMENT.
